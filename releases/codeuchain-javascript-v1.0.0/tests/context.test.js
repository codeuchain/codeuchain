const { Context, MutableContext } = require('../core');

describe('Context', () => {
  describe('Immutable Context', () => {
    test('should create empty context', () => {
      const ctx = new Context();
      expect(ctx.get('nonexistent')).toBeUndefined();
      expect(ctx.keys()).toEqual([]);
    });

    test('should create context with initial data', () => {
      const data = { name: 'Alice', age: 30 };
      const ctx = new Context(data);

      expect(ctx.get('name')).toBe('Alice');
      expect(ctx.get('age')).toBe(30);
      expect(ctx.keys()).toEqual(['name', 'age']);
    });

    test('should return undefined for non-existent keys', () => {
      const ctx = new Context({ name: 'Alice' });
      expect(ctx.get('nonexistent')).toBeUndefined();
    });

    test('should check if key exists', () => {
      const ctx = new Context({ name: 'Alice' });
      expect(ctx.has('name')).toBe(true);
      expect(ctx.has('nonexistent')).toBe(false);
    });

    test('should return all keys', () => {
      const ctx = new Context({ name: 'Alice', age: 30, city: 'NYC' });
      const keys = ctx.keys();
      expect(keys).toContain('name');
      expect(keys).toContain('age');
      expect(keys).toContain('city');
      expect(keys).toHaveLength(3);
    });

    test('should insert new data immutably', () => {
      const ctx1 = new Context({ name: 'Alice' });
      const ctx2 = ctx1.insert('age', 30);

      // Original context unchanged
      expect(ctx1.get('age')).toBeUndefined();
      expect(ctx1.has('age')).toBe(false);

      // New context has the data
      expect(ctx2.get('age')).toBe(30);
      expect(ctx2.has('age')).toBe(true);
    });

    test('should merge contexts immutably', () => {
      const ctx1 = new Context({ name: 'Alice', age: 30 });
      const ctx2 = new Context({ city: 'NYC', country: 'USA' });
      const merged = ctx1.merge(ctx2);

      // Original contexts unchanged
      expect(ctx1.has('city')).toBe(false);
      expect(ctx2.has('name')).toBe(false);

      // Merged context has all data
      expect(merged.get('name')).toBe('Alice');
      expect(merged.get('age')).toBe(30);
      expect(merged.get('city')).toBe('NYC');
      expect(merged.get('country')).toBe('USA');
    });

    test('should convert to plain object', () => {
      const data = { name: 'Alice', age: 30 };
      const ctx = new Context(data);
      const obj = ctx.toObject();

      expect(obj).toEqual(data);
      expect(obj).not.toBe(data); // Should be a copy
    });

    test('should provide mutable version', () => {
      const ctx = new Context({ name: 'Alice' });
      const mutable = ctx.withMutation();

      expect(mutable).toBeInstanceOf(MutableContext);
      expect(mutable.get('name')).toBe('Alice');
    });

    test('should have string representation', () => {
      const ctx = new Context({ name: 'Alice' });
      const str = ctx.toString();
      expect(str).toContain('Context');
      expect(str).toContain('Alice');
    });
  });

  describe('Mutable Context', () => {
    test('should create mutable context', () => {
      const mutable = new MutableContext({ name: 'Alice' });
      expect(mutable.get('name')).toBe('Alice');
    });

    test('should allow in-place mutation', () => {
      const mutable = new MutableContext({ name: 'Alice' });
      mutable.set('age', 30);

      expect(mutable.get('age')).toBe(30);
      expect(mutable.has('age')).toBe(true);
    });

    test('should convert back to immutable', () => {
      const mutable = new MutableContext({ name: 'Alice' });
      mutable.set('age', 30);
      const immutable = mutable.toImmutable();

      expect(immutable).toBeInstanceOf(Context);
      expect(immutable.get('name')).toBe('Alice');
      expect(immutable.get('age')).toBe(30);

      // Further mutations don't affect immutable
      mutable.set('city', 'NYC');
      expect(immutable.has('city')).toBe(false);
    });

    test('should handle all data types', () => {
      const mutable = new MutableContext();

      mutable.set('string', 'hello');
      mutable.set('number', 42);
      mutable.set('boolean', true);
      mutable.set('array', [1, 2, 3]);
      mutable.set('object', { nested: 'value' });
      mutable.set('null', null);
      mutable.set('undefined', undefined);

      expect(mutable.get('string')).toBe('hello');
      expect(mutable.get('number')).toBe(42);
      expect(mutable.get('boolean')).toBe(true);
      expect(mutable.get('array')).toEqual([1, 2, 3]);
      expect(mutable.get('object')).toEqual({ nested: 'value' });
      expect(mutable.get('null')).toBeNull();
      expect(mutable.get('undefined')).toBeUndefined();
    });
  });

  describe('Static Factory Methods', () => {
    test('should create empty context', () => {
      const ctx = Context.empty();
      expect(ctx.keys()).toEqual([]);
    });

    test('should create context from data', () => {
      const data = { name: 'Alice' };
      const ctx = Context.from(data);
      expect(ctx.get('name')).toBe('Alice');
    });
  });

  describe('Immutability Guarantees', () => {
    test('should not allow direct mutation of internal data', () => {
      const ctx = new Context({ items: [1, 2, 3] });
      const items = ctx.get('items');

      // This should not affect the context
      if (Array.isArray(items)) {
        items.push(4);
      }

      expect(ctx.get('items')).toEqual([1, 2, 3]);
    });

    test('should return copies of complex objects', () => {
      const originalArray = [1, 2, 3];
      const ctx = new Context({ items: originalArray });
      const retrievedArray = ctx.get('items');

      expect(retrievedArray).toEqual(originalArray);
      expect(retrievedArray).not.toBe(originalArray); // Should be a copy
    });
  });
});