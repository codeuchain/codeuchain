const { Context, Chain, Link, LoggingMiddleware, TimingMiddleware } = require('../core');

// E-commerce Order Processing Example
class OrderValidationLink extends Link {
  async call(ctx) {
    const items = ctx.get('items');
    const customerId = ctx.get('customerId');
    
    if (!items || items.length === 0) {
      throw new Error('Order must contain at least one item');
    }
    if (!customerId) {
      throw new Error('Customer ID is required');
    }

    const total = items.reduce((sum, item) => sum + (item.price * item.quantity), 0);
    return ctx.insert('orderTotal', total).insert('validated', true);
  }
  getName() { return 'OrderValidationLink'; }
}

class InventoryCheckLink extends Link {
  constructor(inventory) {
    super();
    this.inventory = inventory;
  }

  async call(ctx) {
    const items = ctx.get('items');
    const insufficient = [];

    for (const item of items) {
      const available = this.inventory[item.id] || 0;
      if (available < item.quantity) {
        insufficient.push({
          id: item.id,
          requested: item.quantity,
          available
        });
      }
    }

    if (insufficient.length > 0) {
      return ctx.insert('inventoryIssues', insufficient).insert('canFulfill', false);
    }

    return ctx.insert('canFulfill', true);
  }
  getName() { return 'InventoryCheckLink'; }
}

class PaymentProcessingLink extends Link {
  async call(ctx) {
    const orderTotal = ctx.get('orderTotal');
    const paymentMethod = ctx.get('paymentMethod');

    if (!paymentMethod || !paymentMethod.type) {
      throw new Error('Payment method is required');
    }

    // Simulate payment processing
    if (paymentMethod.type === 'credit_card' && paymentMethod.number) {
      // In real implementation, this would call payment gateway
      console.log(`💳 Processing payment of $${orderTotal} via credit card`);
      return ctx.insert('paymentStatus', 'completed').insert('transactionId', `txn_${Date.now()}`);
    }

    throw new Error('Unsupported payment method');
  }
  getName() { return 'PaymentProcessingLink'; }
}

class OrderFulfillmentLink extends Link {
  constructor(inventory) {
    super();
    this.inventory = inventory;
  }

  async call(ctx) {
    const items = ctx.get('items');
    const canFulfill = ctx.get('canFulfill');

    if (!canFulfill) {
      throw new Error('Cannot fulfill order due to inventory issues');
    }

    // Update inventory
    for (const item of items) {
      this.inventory[item.id] -= item.quantity;
    }

    const orderId = `order_${Date.now()}`;
    return ctx
      .insert('orderId', orderId)
      .insert('fulfilledAt', new Date().toISOString())
      .insert('status', 'fulfilled');
  }
  getName() { return 'OrderFulfillmentLink'; }
}

class ShippingNotificationLink extends Link {
  async call(ctx) {
    const orderId = ctx.get('orderId');
    const shippingAddress = ctx.get('shippingAddress');

    console.log(`📦 Order ${orderId} shipped to ${shippingAddress}`);

    return ctx.insert('shippingNotificationSent', true);
  }
  getName() { return 'ShippingNotificationLink'; }
}

describe('End-to-End Tests', () => {
  let inventory;
  let orderProcessingChain;

  beforeEach(() => {
    // Initialize inventory
    inventory = {
      'item_001': 50, // Laptop
      'item_002': 100, // Mouse
      'item_003': 25, // Keyboard
      'item_004': 0, // Out of stock item
    };

    // Create order processing chain
    orderProcessingChain = new Chain();

    // Add links
    orderProcessingChain.addLink(new OrderValidationLink(), 'validate');
    orderProcessingChain.addLink(new InventoryCheckLink(inventory), 'inventory');
    orderProcessingChain.addLink(new PaymentProcessingLink(), 'payment');
    orderProcessingChain.addLink(new OrderFulfillmentLink(inventory), 'fulfill');
    orderProcessingChain.addLink(new ShippingNotificationLink(), 'notify');

    // Connect links with conditions
    orderProcessingChain.connect('validate', 'inventory');
    orderProcessingChain.connect('inventory', 'payment', (ctx) => ctx.get('canFulfill') === true);
    orderProcessingChain.connect('payment', 'fulfill');
    orderProcessingChain.connect('fulfill', 'notify');

    // Add middleware
    orderProcessingChain.useMiddleware(new LoggingMiddleware());
    orderProcessingChain.useMiddleware(new TimingMiddleware());

    // Error handling
    orderProcessingChain.onError((error, ctx, linkName) => {
      console.error(`❌ Order processing error in ${linkName}: ${error.message}`);
      ctx.insert('error', error.message);
    });
  });

  describe('Successful Order Processing', () => {
    test('should process a complete order successfully', async () => {
      const orderData = {
        customerId: 'customer_123',
        items: [
          { id: 'item_001', name: 'Laptop', price: 1200, quantity: 1 },
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 2 }
        ],
        shippingAddress: '123 Main St, Anytown, USA',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(orderData);
      const result = await orderProcessingChain.run(initialCtx);

      // Verify order validation
      expect(result.get('validated')).toBe(true);
      expect(result.get('orderTotal')).toBe(1250); // 1200 + (25 * 2)

      // Verify inventory check
      expect(result.get('canFulfill')).toBe(true);
      expect(result.get('inventoryIssues')).toBeUndefined();

      // Verify payment processing
      expect(result.get('paymentStatus')).toBe('completed');
      expect(result.get('transactionId')).toBeDefined();
      expect(result.get('transactionId')).toMatch(/^txn_\d+$/);

      // Verify fulfillment
      expect(result.get('orderId')).toBeDefined();
      expect(result.get('orderId')).toMatch(/^order_\d+$/);
      expect(result.get('fulfilledAt')).toBeDefined();
      expect(result.get('status')).toBe('fulfilled');

      // Verify inventory was updated
      expect(inventory['item_001']).toBe(49); // 50 - 1
      expect(inventory['item_002']).toBe(98); // 100 - 2

      // Verify notification
      expect(result.get('shippingNotificationSent')).toBe(true);
    });

    test('should handle multiple items with different quantities', async () => {
      const orderData = {
        customerId: 'customer_456',
        items: [
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 3 },
          { id: 'item_003', name: 'Keyboard', price: 75, quantity: 1 }
        ],
        shippingAddress: '456 Oak Ave, Somewhere, USA',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(orderData);
      const result = await orderProcessingChain.run(initialCtx);

      expect(result.get('orderTotal')).toBe(150); // (25 * 3) + 75
      expect(result.get('canFulfill')).toBe(true);
      expect(result.get('status')).toBe('fulfilled');

      // Verify inventory updates
      expect(inventory['item_002']).toBe(97); // 100 - 3
      expect(inventory['item_003']).toBe(24); // 25 - 1
    });
  });

  describe('Error Handling and Edge Cases', () => {
    test('should handle insufficient inventory', async () => {
      const orderData = {
        customerId: 'customer_789',
        items: [
          { id: 'item_004', name: 'Out of Stock Item', price: 50, quantity: 1 } // Out of stock
        ],
        shippingAddress: '789 Pine St, Nowhere, USA',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(orderData);
      const result = await orderProcessingChain.run(initialCtx);

      // Should pass validation and inventory check
      expect(result.get('validated')).toBe(true);
      expect(result.get('canFulfill')).toBe(false);

      // Should have inventory issues
      const issues = result.get('inventoryIssues');
      expect(issues).toHaveLength(1);
      expect(issues[0]).toEqual({
        id: 'item_004',
        requested: 1,
        available: 0
      });

      // Should not proceed to payment/fulfillment
      expect(result.get('paymentStatus')).toBeUndefined();
      expect(result.get('orderId')).toBeUndefined();
      expect(result.get('status')).toBeUndefined();
    });

    test('should handle invalid order data', async () => {
      const invalidOrderData = {
        // Missing customerId
        items: [], // Empty items
        shippingAddress: '123 Test St',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111'
        }
      };

      const initialCtx = new Context(invalidOrderData);

      await expect(orderProcessingChain.run(initialCtx)).rejects.toThrow('Order must contain at least one item');
    });

    test('should handle payment method errors', async () => {
      const orderData = {
        customerId: 'customer_999',
        items: [
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 1 }
        ],
        shippingAddress: '999 Test Ave, Errorville, USA',
        paymentMethod: {
          type: 'unsupported_method'
        }
      };

      const initialCtx = new Context(orderData);

      await expect(orderProcessingChain.run(initialCtx)).rejects.toThrow('Unsupported payment method');
    });

    test('should handle partial inventory issues', async () => {
      // Set up scenario where some items are available, others are not
      inventory['item_001'] = 1; // Only 1 laptop available

      const orderData = {
        customerId: 'customer_partial',
        items: [
          { id: 'item_001', name: 'Laptop', price: 1200, quantity: 2 }, // Request 2, only 1 available
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 1 } // This is available
        ],
        shippingAddress: 'Partial St, Incomplete, USA',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(orderData);
      const result = await orderProcessingChain.run(initialCtx);

      expect(result.get('canFulfill')).toBe(false);

      const issues = result.get('inventoryIssues');
      expect(issues).toHaveLength(1);
      expect(issues[0]).toEqual({
        id: 'item_001',
        requested: 2,
        available: 1
      });

      // Should not proceed to fulfillment
      expect(result.get('orderId')).toBeUndefined();
    });
  });

  describe('Complex Business Logic', () => {
    test('should handle bulk orders with discounts', async () => {
      // Create a chain with discount logic
      const bulkOrderChain = new Chain();

      class BulkDiscountLink extends Link {
        async call(ctx) {
          const items = ctx.get('items');
          const totalItems = items.reduce((sum, item) => sum + item.quantity, 0);

          let discount = 0;
          if (totalItems >= 10) {
            discount = 0.15; // 15% discount for 10+ items
          } else if (totalItems >= 5) {
            discount = 0.10; // 10% discount for 5+ items
          }

          const subtotal = ctx.get('orderTotal');
          const discountAmount = subtotal * discount;
          const finalTotal = subtotal - discountAmount;

          return ctx
            .insert('discountPercent', discount)
            .insert('discountAmount', discountAmount)
            .insert('finalTotal', finalTotal);
        }
        getName() { return 'BulkDiscountLink'; }
      }

      bulkOrderChain.addLink(new OrderValidationLink(), 'validate');
      bulkOrderChain.addLink(new BulkDiscountLink(), 'discount');
      bulkOrderChain.addLink(new InventoryCheckLink(inventory), 'inventory');
      bulkOrderChain.addLink(new PaymentProcessingLink(), 'payment');
      bulkOrderChain.addLink(new OrderFulfillmentLink(inventory), 'fulfill');

      bulkOrderChain.connect('validate', 'discount');
      bulkOrderChain.connect('discount', 'inventory');
      bulkOrderChain.connect('inventory', 'payment', (ctx) => ctx.get('canFulfill') === true);
      bulkOrderChain.connect('payment', 'fulfill');

      // Test bulk order
      const bulkOrderData = {
        customerId: 'customer_bulk',
        items: [
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 6 } // 6 items = 10% discount
        ],
        shippingAddress: 'Bulk St, Wholesale, USA',
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(bulkOrderData);
      const result = await bulkOrderChain.run(initialCtx);

      expect(result.get('orderTotal')).toBe(150); // 25 * 6
      expect(result.get('discountPercent')).toBe(0.10); // 10% discount
      expect(result.get('discountAmount')).toBe(15); // 150 * 0.10
      expect(result.get('finalTotal')).toBe(135); // 150 - 15
      expect(result.get('status')).toBe('fulfilled');
    });

    test('should handle international shipping with different rules', async () => {
      const internationalChain = new Chain();

      class ShippingCalculatorLink extends Link {
        async call(ctx) {
          const items = ctx.get('items');
          const shippingAddress = ctx.get('shippingAddress');
          const country = shippingAddress.country;

          let shippingCost = 0;
          let shippingMethod = 'standard';

          if (country === 'US') {
            shippingCost = items.length * 5; // $5 per item
          } else if (country === 'CA') {
            shippingCost = items.length * 8; // $8 per item
            shippingMethod = 'express'; // Faster for Canada
          } else {
            // For international, calculate based on total quantity
            const totalQuantity = items.reduce((sum, item) => sum + item.quantity, 0);
            shippingCost = totalQuantity * 15; // $15 per item international
            shippingMethod = 'international';
          }

          return ctx
            .insert('shippingCost', shippingCost)
            .insert('shippingMethod', shippingMethod)
            .insert('totalWithShipping', ctx.get('orderTotal') + shippingCost);
        }
        getName() { return 'ShippingCalculatorLink'; }
      }

      internationalChain.addLink(new OrderValidationLink(), 'validate');
      internationalChain.addLink(new ShippingCalculatorLink(), 'shipping');
      internationalChain.addLink(new InventoryCheckLink(inventory), 'inventory');
      internationalChain.addLink(new PaymentProcessingLink(), 'payment');
      internationalChain.addLink(new OrderFulfillmentLink(inventory), 'fulfill');

      internationalChain.connect('validate', 'shipping');
      internationalChain.connect('shipping', 'inventory');
      internationalChain.connect('inventory', 'payment', (ctx) => ctx.get('canFulfill') === true);
      internationalChain.connect('payment', 'fulfill');

      // Test international order
      const internationalOrder = {
        customerId: 'customer_intl',
        items: [
          { id: 'item_002', name: 'Mouse', price: 25, quantity: 2 }
        ],
        shippingAddress: {
          street: '123 International St',
          city: 'London',
          country: 'UK'
        },
        paymentMethod: {
          type: 'credit_card',
          number: '4111111111111111',
          expiry: '12/25'
        }
      };

      const initialCtx = new Context(internationalOrder);
      const result = await internationalChain.run(initialCtx);

      expect(result.get('orderTotal')).toBe(50); // 25 * 2
      expect(result.get('shippingCost')).toBe(30); // 2 items * $15 international
      expect(result.get('shippingMethod')).toBe('international');
      expect(result.get('totalWithShipping')).toBe(80); // 50 + 30
      expect(result.get('status')).toBe('fulfilled');
    });
  });

  describe('Performance and Scalability', () => {
    test('should handle high-volume order processing', async () => {
      const highVolumeChain = new Chain();

      class SimpleValidationLink extends Link {
        async call(ctx) {
          const order = ctx.get('order');
          if (!order.customerId || !order.items?.length) {
            throw new Error('Invalid order');
          }
          return ctx.insert('validated', true);
        }
        getName() { return 'SimpleValidationLink'; }
      }

      class SimpleFulfillmentLink extends Link {
        async call(ctx) {
          // Simulate some processing time
          await new Promise(resolve => setTimeout(resolve, 1));
          return ctx.insert('fulfilled', true);
        }
        getName() { return 'SimpleFulfillmentLink'; }
      }

      highVolumeChain.addLink(new SimpleValidationLink(), 'validate');
      highVolumeChain.addLink(new SimpleFulfillmentLink(), 'fulfill');
      highVolumeChain.connect('validate', 'fulfill');

      // Create 100 orders
      const orders = Array.from({ length: 100 }, (_, i) => ({
        customerId: `customer_${i}`,
        items: [{ id: 'item_001', name: 'Test Item', price: 10, quantity: 1 }]
      }));

      const startTime = Date.now();

      // Process all orders concurrently
      const promises = orders.map(order => {
        const ctx = new Context({ order });
        return highVolumeChain.run(ctx);
      });

      const results = await Promise.all(promises);
      const endTime = Date.now();

      // Verify all orders were processed
      results.forEach(result => {
        expect(result.get('validated')).toBe(true);
        expect(result.get('fulfilled')).toBe(true);
      });

      // Performance check - should complete within reasonable time
      const processingTime = endTime - startTime;
      console.log(`Processed 100 orders in ${processingTime}ms`);
      expect(processingTime).toBeLessThan(5000); // Should complete in under 5 seconds
    });

    test('should handle memory efficiently with large orders', async () => {
      const largeOrderChain = new Chain();

      class LargeOrderProcessor extends Link {
        async call(ctx) {
          const order = ctx.get('order');
          // Process large order data
          const processedItems = order.items.map(item => ({
            ...item,
            processed: true,
            processingTimestamp: Date.now()
          }));

          return ctx.insert('processedItems', processedItems);
        }
        getName() { return 'LargeOrderProcessor'; }
      }

      largeOrderChain.addLink(new LargeOrderProcessor(), 'process');

      // Create order with 1000 items
      const largeOrder = {
        customerId: 'customer_large',
        items: Array.from({ length: 1000 }, (_, i) => ({
          id: `item_${i}`,
          name: `Item ${i}`,
          price: Math.random() * 100,
          quantity: Math.floor(Math.random() * 5) + 1
        }))
      };

      const initialCtx = new Context({ order: largeOrder });
      const result = await largeOrderChain.run(initialCtx);

      const processedItems = result.get('processedItems');
      expect(processedItems).toHaveLength(1000);

      // Verify each item was processed
      processedItems.forEach(item => {
        expect(item.processed).toBe(true);
        expect(item.processingTimestamp).toBeDefined();
      });
    });
  });
});