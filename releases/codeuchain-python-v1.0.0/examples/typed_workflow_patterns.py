"""
CodeUChain: Typed Workflow Patterns

This example demonstrates common patterns for building typed workflows in CodeUChain.
These patterns show how to structure complex business processes with full type safety.

Key Patterns Demonstrated:
1. Sequential Processing Pipeline
2. Conditional Branching with Types
3. Error Handling with Typed Results
4. Data Enrichment Workflows
5. Validation and Transformation Chains
6. Parallel Processing with Type Safety
"""

import asyncio
from typing import List, TypedDict, Union, Optional
from codeuchain.core import Chain, Context, Link

# =============================================================================
# SHARED TYPE DEFINITIONS
# =============================================================================

class OrderInput(TypedDict):
    """Initial order data."""
    order_id: str
    customer_id: str
    items: List[dict]
    total_amount: float


class OrderValidated(TypedDict):
    """Order after validation."""
    order_id: str
    customer_id: str
    items: List[dict]
    total_amount: float
    is_valid: bool
    validation_errors: List[str]


class OrderWithCustomer(TypedDict):
    """Order with customer information."""
    order_id: str
    customer_id: str
    items: List[dict]
    total_amount: float
    is_valid: bool
    validation_errors: List[str]
    customer_name: str
    customer_email: str
    customer_loyalty_tier: str


class OrderProcessed(TypedDict):
    """Fully processed order."""
    order_id: str
    customer_id: str
    items: List[dict]
    total_amount: float
    is_valid: bool
    validation_errors: List[str]
    customer_name: str
    customer_email: str
    customer_loyalty_tier: str
    tax_amount: float
    discount_amount: float
    final_amount: float
    processing_status: str


class OrderResult(TypedDict):
    """Final order result."""
    order_id: str
    customer_id: str
    items: List[dict]
    total_amount: float
    is_valid: bool
    validation_errors: List[str]
    customer_name: str
    customer_email: str
    customer_loyalty_tier: str
    tax_amount: float
    discount_amount: float
    final_amount: float
    processing_status: str
    payment_status: str
    fulfillment_status: str


# =============================================================================
# PATTERN 1: SEQUENTIAL PROCESSING PIPELINE
# =============================================================================

class ValidateOrderLink(Link[OrderInput, OrderValidated]):
    """Validate order data."""

    async def call(self, ctx: Context[OrderInput]) -> Context[OrderValidated]:
        order_id = ctx.get("order_id") or ""
        items = ctx.get("items") or []
        total_amount = ctx.get("total_amount") or 0.0

        errors = []

        if not order_id:
            errors.append("Order ID is required")

        if not items:
            errors.append("Order must have at least one item")

        if total_amount <= 0:
            errors.append("Total amount must be positive")

        # Validate each item has required fields
        for i, item in enumerate(items):
            if not isinstance(item, dict):
                errors.append(f"Item {i} must be a dictionary")
                continue

            if "product_id" not in item:
                errors.append(f"Item {i} missing product_id")
            if "quantity" not in item:
                errors.append(f"Item {i} missing quantity")
            if "price" not in item:
                errors.append(f"Item {i} missing price")

        is_valid = len(errors) == 0

        return ctx.insert_as("is_valid", is_valid).insert_as("validation_errors", errors)


class LoadCustomerLink(Link[OrderValidated, OrderWithCustomer]):
    """Load customer information."""

    async def call(self, ctx: Context[OrderValidated]) -> Context[OrderWithCustomer]:
        customer_id = ctx.get("customer_id") or ""

        # Mock customer lookup - in real code, this would query a database
        customer_data = self._lookup_customer(customer_id)

        return (
            ctx
            .insert_as("customer_name", customer_data["name"])
            .insert_as("customer_email", customer_data["email"])
            .insert_as("customer_loyalty_tier", customer_data["tier"])
        )

    def _lookup_customer(self, customer_id: str) -> dict:
        """Mock customer lookup."""
        # Simulate database lookup
        mock_customers = {
            "CUST001": {"name": "Alice Johnson", "email": "alice@example.com", "tier": "Gold"},
            "CUST002": {"name": "Bob Smith", "email": "bob@example.com", "tier": "Silver"},
        }
        return mock_customers.get(customer_id, {"name": "Unknown", "email": "", "tier": "Bronze"})


class CalculatePricingLink(Link[OrderWithCustomer, OrderProcessed]):
    """Calculate taxes, discounts, and final pricing."""

    async def call(self, ctx: Context[OrderWithCustomer]) -> Context[OrderProcessed]:
        total_amount = ctx.get("total_amount") or 0.0
        loyalty_tier = ctx.get("customer_loyalty_tier") or "Bronze"

        # Calculate tax (8.5%)
        tax_amount = total_amount * 0.085

        # Calculate discount based on loyalty tier
        discount_rate = {"Bronze": 0.0, "Silver": 0.05, "Gold": 0.10}.get(loyalty_tier, 0.0)
        discount_amount = total_amount * discount_rate

        final_amount = total_amount + tax_amount - discount_amount

        return (
            ctx
            .insert_as("tax_amount", round(tax_amount, 2))
            .insert_as("discount_amount", round(discount_amount, 2))
            .insert_as("final_amount", round(final_amount, 2))
            .insert_as("processing_status", "completed")
        )


class SequentialProcessingChain:
    """Sequential processing pipeline with full type safety."""

    def __init__(self):
        self.chain: Chain[OrderInput, OrderProcessed] = Chain()
        self.chain.add_link(ValidateOrderLink(), "validate")
        self.chain.add_link(LoadCustomerLink(), "load_customer")
        self.chain.add_link(CalculatePricingLink(), "calculate_pricing")

    async def process(self, ctx: Context[OrderInput]) -> Context[OrderProcessed]:
        return await self.chain.run(ctx)


# =============================================================================
# PATTERN 2: CONDITIONAL BRANCHING WITH TYPES
# =============================================================================

class PaymentProcessingLink(Link[OrderProcessed, OrderResult]):
    """Process payment with conditional logic."""

    async def call(self, ctx: Context[OrderProcessed]) -> Context[OrderResult]:
        is_valid = ctx.get("is_valid") or False
        final_amount = ctx.get("final_amount") or 0.0

        if not is_valid:
            # Invalid orders cannot be paid
            return (
                ctx
                .insert_as("payment_status", "rejected")
                .insert_as("fulfillment_status", "cancelled")
            )

        if final_amount > 1000.00:
            # High-value orders require manual approval
            return (
                ctx
                .insert_as("payment_status", "pending_approval")
                .insert_as("fulfillment_status", "on_hold")
            )

        # Normal processing
        payment_success = await self._process_payment(final_amount)

        return (
            ctx
            .insert_as("payment_status", "completed" if payment_success else "failed")
            .insert_as("fulfillment_status", "processing" if payment_success else "cancelled")
        )

    async def _process_payment(self, amount: float) -> bool:
        """Mock payment processing."""
        # Simulate payment gateway call
        await asyncio.sleep(0.1)  # Simulate network delay
        return amount < 500.00  # Simulate some payments failing


class ConditionalProcessingChain:
    """Chain with conditional branching based on order characteristics."""

    def __init__(self):
        self.chain: Chain[OrderProcessed, OrderResult] = Chain()
        self.chain.add_link(PaymentProcessingLink(), "process_payment")

        # Add conditional connections
        self.chain.connect("process_payment", "process_payment",
                          lambda ctx: ctx.get("processing_status") == "completed")

    async def process(self, ctx: Context[OrderProcessed]) -> Context[OrderResult]:
        return await self.chain.run(ctx)


# =============================================================================
# PATTERN 3: ERROR HANDLING WITH TYPED RESULTS
# =============================================================================

class ErrorHandlingLink(Link[OrderResult, OrderResult]):
    """Handle errors and edge cases with typed error information."""

    async def call(self, ctx: Context[OrderResult]) -> Context[OrderResult]:
        payment_status = ctx.get("payment_status") or ""
        validation_errors = ctx.get("validation_errors") or []

        if payment_status == "failed":
            # Add specific error information
            return ctx.insert_as("error_code", "PAYMENT_FAILED").insert_as("error_message", "Payment processing failed")

        if validation_errors:
            # Add validation error details
            return ctx.insert_as("error_code", "VALIDATION_ERROR").insert_as("error_message", f"Validation errors: {', '.join(validation_errors)}")

        if payment_status == "pending_approval":
            # Add approval workflow information
            return ctx.insert_as("requires_approval", True).insert_as("approval_threshold", 1000.00)

        return ctx.insert_as("error_code", None).insert_as("error_message", None)


class ErrorHandlingChain:
    """Chain that demonstrates comprehensive error handling."""

    def __init__(self):
        self.chain: Chain[OrderResult, OrderResult] = Chain()
        self.chain.add_link(ErrorHandlingLink(), "handle_errors")

    async def process(self, ctx: Context[OrderResult]) -> Context[OrderResult]:
        return await self.chain.run(ctx)


# =============================================================================
# PATTERN 4: PARALLEL PROCESSING WITH TYPE SAFETY
# =============================================================================

class InventoryCheckLink(Link[OrderValidated, OrderValidated]):
    """Check inventory for ordered items."""

    async def call(self, ctx: Context[OrderValidated]) -> Context[OrderValidated]:
        items = ctx.get("items") or []

        # Check inventory for each item
        inventory_status = []
        for item in items:
            product_id = item.get("product_id", "")
            quantity = item.get("quantity", 0)

            # Mock inventory check
            available = self._check_inventory(product_id)
            sufficient = available >= quantity

            inventory_status.append({
                "product_id": product_id,
                "requested": quantity,
                "available": available,
                "sufficient": sufficient
            })

        all_available = all(status["sufficient"] for status in inventory_status)

        return ctx.insert_as("inventory_status", inventory_status).insert_as("inventory_available", all_available)

    def _check_inventory(self, product_id: str) -> int:
        """Mock inventory lookup."""
        mock_inventory = {
            "PROD001": 50,
            "PROD002": 25,
            "PROD003": 0,  # Out of stock
        }
        return mock_inventory.get(product_id, 0)


class FraudCheckLink(Link[OrderValidated, OrderValidated]):
    """Perform fraud detection checks."""

    async def call(self, ctx: Context[OrderValidated]) -> Context[OrderValidated]:
        customer_id = ctx.get("customer_id") or ""
        total_amount = ctx.get("total_amount") or 0.0

        # Mock fraud detection
        fraud_score = self._calculate_fraud_score(customer_id, total_amount)
        is_suspicious = fraud_score > 0.7

        return ctx.insert_as("fraud_score", fraud_score).insert_as("is_suspicious", is_suspicious)

    def _calculate_fraud_score(self, customer_id: str, amount: float) -> float:
        """Mock fraud score calculation."""
        # Simulate fraud detection algorithm
        if amount > 500.00:
            return 0.8
        return 0.2


class ParallelValidationChain:
    """Chain that runs validation checks in parallel."""

    def __init__(self):
        self.chain: Chain[OrderValidated, OrderValidated] = Chain()

        # Add parallel validation links
        self.chain.add_link(InventoryCheckLink(), "inventory_check")
        self.chain.add_link(FraudCheckLink(), "fraud_check")

        # Both run in parallel, no dependencies between them

    async def process(self, ctx: Context[OrderValidated]) -> Context[OrderValidated]:
        return await self.chain.run(ctx)


# =============================================================================
# DEMONSTRATION FUNCTIONS
# =============================================================================

async def demonstrate_sequential_processing():
    """Demonstrate sequential processing pipeline."""

    print("=== PATTERN 1: SEQUENTIAL PROCESSING PIPELINE ===\n")

    # Sample order data
    order_data: OrderInput = {
        "order_id": "ORD001",
        "customer_id": "CUST001",
        "items": [
            {"product_id": "PROD001", "quantity": 2, "price": 25.00},
            {"product_id": "PROD002", "quantity": 1, "price": 50.00}
        ],
        "total_amount": 100.00
    }

    print(f"Input Order: {order_data}\n")

    # Process through the pipeline
    chain = SequentialProcessingChain()
    ctx = Context[OrderInput](order_data)

    result_ctx = await chain.process(ctx)
    result = result_ctx.to_dict()

    print("Processing Results:")
    print(f"  Customer: {result.get('customer_name')} ({result.get('customer_loyalty_tier')} tier)")
    print(f"  Tax: ${result.get('tax_amount')}")
    print(f"  Discount: ${result.get('discount_amount')}")
    print(f"  Final Amount: ${result.get('final_amount')}")
    print(f"  Status: {result.get('processing_status')}")
    print()


async def demonstrate_conditional_processing():
    """Demonstrate conditional processing based on order characteristics."""

    print("=== PATTERN 2: CONDITIONAL BRANCHING ===\n")

    test_cases = [
        {
            "name": "Valid Small Order",
            "data": {
                "order_id": "ORD002",
                "customer_id": "CUST002",
                "items": [{"product_id": "PROD001", "quantity": 1, "price": 25.00}],
                "total_amount": 25.00,
                "is_valid": True,
                "validation_errors": [],
                "customer_name": "Bob Smith",
                "customer_email": "bob@example.com",
                "customer_loyalty_tier": "Silver",
                "tax_amount": 2.13,
                "discount_amount": 1.25,
                "final_amount": 25.88,
                "processing_status": "completed"
            }
        },
        {
            "name": "High-Value Order",
            "data": {
                "order_id": "ORD003",
                "customer_id": "CUST001",
                "items": [{"product_id": "PROD002", "quantity": 20, "price": 50.00}],
                "total_amount": 1200.00,
                "is_valid": True,
                "validation_errors": [],
                "customer_name": "Alice Johnson",
                "customer_email": "alice@example.com",
                "customer_loyalty_tier": "Gold",
                "tax_amount": 102.00,
                "discount_amount": 120.00,
                "final_amount": 1182.00,
                "processing_status": "completed"
            }
        }
    ]

    for test_case in test_cases:
        print(f"--- {test_case['name']} ---")

        chain = ConditionalProcessingChain()
        ctx = Context[OrderProcessed](test_case["data"])

        result_ctx = await chain.process(ctx)
        result = result_ctx.to_dict()

        print(f"  Payment Status: {result.get('payment_status')}")
        print(f"  Fulfillment Status: {result.get('fulfillment_status')}")

        if result.get("requires_approval"):
            print(f"  Requires Approval: ${result.get('approval_threshold')} threshold")

        print()


async def demonstrate_parallel_processing():
    """Demonstrate parallel validation checks."""

    print("=== PATTERN 4: PARALLEL PROCESSING ===\n")

    order_data: OrderValidated = {
        "order_id": "ORD004",
        "customer_id": "CUST001",
        "items": [
            {"product_id": "PROD001", "quantity": 3, "price": 25.00},
            {"product_id": "PROD003", "quantity": 1, "price": 30.00}  # Out of stock
        ],
        "total_amount": 105.00,
        "is_valid": True,
        "validation_errors": []
    }

    print(f"Order Items: {order_data['items']}\n")

    # Run parallel validation
    chain = ParallelValidationChain()
    ctx = Context[OrderValidated](order_data)

    result_ctx = await chain.process(ctx)
    result = result_ctx.to_dict()

    print("Parallel Validation Results:")
    print(f"  Inventory Available: {result.get('inventory_available')}")
    print(f"  Is Suspicious: {result.get('is_suspicious')}")
    print(f"  Fraud Score: {result.get('fraud_score')}")

    print("\nInventory Details:")
    for status in result.get("inventory_status", []):
        print(f"  {status['product_id']}: {status['requested']} requested, {status['available']} available")

    print()


async def demonstrate_error_handling():
    """Demonstrate comprehensive error handling."""

    print("=== PATTERN 3: ERROR HANDLING ===\n")

    # Test case with validation errors
    error_case: OrderResult = {
        "order_id": "ORD005",
        "customer_id": "CUST001",
        "items": [],
        "total_amount": 0.0,
        "is_valid": False,
        "validation_errors": ["Order must have at least one item", "Total amount must be positive"],
        "customer_name": "",
        "customer_email": "",
        "customer_loyalty_tier": "Bronze",
        "tax_amount": 0.0,
        "discount_amount": 0.0,
        "final_amount": 0.0,
        "processing_status": "failed",
        "payment_status": "rejected",
        "fulfillment_status": "cancelled"
    }

    chain = ErrorHandlingChain()
    ctx = Context[OrderResult](error_case)

    result_ctx = await chain.process(ctx)
    result = result_ctx.to_dict()

    print("Error Handling Results:")
    print(f"  Error Code: {result.get('error_code')}")
    print(f"  Error Message: {result.get('error_message')}")
    print()


# =============================================================================
# MAIN DEMONSTRATION
# =============================================================================

async def main():
    """Run all pattern demonstrations."""

    print("🎯 CodeUChain Typed Workflow Patterns")
    print("=" * 50)
    print()

    await demonstrate_sequential_processing()
    await demonstrate_conditional_processing()
    await demonstrate_parallel_processing()
    await demonstrate_error_handling()

    print("=== SUMMARY OF TYPED WORKFLOW PATTERNS ===")
    print()
    print("1. SEQUENTIAL PROCESSING:")
    print("   • Type-safe step-by-step processing")
    print("   • Clear input/output contracts")
    print("   • Compile-time validation of data flow")
    print()
    print("2. CONDITIONAL BRANCHING:")
    print("   • Type-safe conditional logic")
    print("   • Different paths for different scenarios")
    print("   • Maintains type safety across branches")
    print()
    print("3. ERROR HANDLING:")
    print("   • Typed error information")
    print("   • Structured error responses")
    print("   • Type-safe error propagation")
    print()
    print("4. PARALLEL PROCESSING:")
    print("   • Independent validation checks")
    print("   • Type-safe concurrent operations")
    print("   • Aggregated results with full typing")
    print()
    print("These patterns enable building complex, type-safe business workflows!")


if __name__ == "__main__":
    asyncio.run(main())
