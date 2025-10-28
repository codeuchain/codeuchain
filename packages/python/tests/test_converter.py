"""
Tests for Chain Converter

Validates the bidirectional conversion between CodeUChain and traditional code.
"""

import asyncio
import pytest
from typing import Dict, Any

from codeuchain.core import Context, Chain, Link
from codeuchain.utils import ChainConverter


# Test Links
class AddNumbersLink:
    """Simple link that adds two numbers."""
    
    async def call(self, ctx: Context) -> Context:
        a = ctx.get('a') or 0
        b = ctx.get('b') or 0
        result = a + b
        return ctx.insert('result', result)


class MultiplyLink:
    """Link that multiplies result by a factor."""
    
    async def call(self, ctx: Context) -> Context:
        result = ctx.get('result') or 0
        factor = ctx.get('factor') or 1
        multiplied = result * factor
        return ctx.insert('result', multiplied)


class FormatResultLink:
    """Link that formats the result as a string."""
    
    async def call(self, ctx: Context) -> Context:
        result = ctx.get('result')
        formatted = f"Result: {result}"
        return ctx.insert('formatted', formatted)


@pytest.fixture
def simple_chain():
    """Create a simple chain for testing."""
    chain = Chain()
    chain.add_link(AddNumbersLink(), "add")
    chain.add_link(MultiplyLink(), "multiply")
    return chain


@pytest.fixture
def complex_chain():
    """Create a more complex chain for testing."""
    chain = Chain()
    chain.add_link(AddNumbersLink(), "add")
    chain.add_link(MultiplyLink(), "multiply")
    chain.add_link(FormatResultLink(), "format")
    return chain


@pytest.fixture
def converter():
    """Create a ChainConverter instance."""
    return ChainConverter()


class TestChainToCode:
    """Tests for converting chains to traditional code."""
    
    def test_simple_chain_to_code(self, simple_chain, converter):
        """Test converting a simple chain to code."""
        code = converter.chain_to_code(simple_chain)
        
        # Verify code structure
        assert 'async def execute_pipeline' in code
        assert 'initial_data: Dict[str, Any]' in code
        assert 'ctx_data = initial_data.copy()' in code
        assert 'return ctx_data' in code
        
        # Verify links are present
        assert 'add_link' in code
        assert 'multiply_link' in code
    
    def test_chain_to_code_custom_name(self, simple_chain, converter):
        """Test converting with a custom function name."""
        code = converter.chain_to_code(simple_chain, function_name="process_data")
        
        assert 'async def process_data' in code
        assert 'execute_pipeline' not in code
    
    def test_chain_to_code_without_types(self, simple_chain, converter):
        """Test converting without type hints."""
        code = converter.chain_to_code(simple_chain, include_types=False)
        
        assert 'async def execute_pipeline(initial_data):' in code
        assert 'Dict[str, Any]' not in code
    
    def test_generated_code_is_executable(self, simple_chain, converter):
        """Test that generated code can be executed."""
        code = converter.chain_to_code(simple_chain)
        
        # Verify code is valid Python
        compile(code, '<string>', 'exec')
    
    def test_complex_chain_to_code(self, complex_chain, converter):
        """Test converting a complex chain with multiple links."""
        code = converter.chain_to_code(complex_chain)
        
        # Verify all links are present
        assert 'add_link' in code
        assert 'multiply_link' in code
        assert 'format_link' in code
        
        # Verify proper sequencing
        assert code.index('add_link') < code.index('multiply_link')
        assert code.index('multiply_link') < code.index('format_link')


class TestCodeToChain:
    """Tests for converting traditional code back to chains."""
    
    def test_code_to_chain_basic(self, converter):
        """Test converting basic code to a chain."""
        code = '''
from codeuchain.core import Context

class AddNumbersLink:
    async def call(self, ctx: Context) -> Context:
        a = ctx.get('a') or 0
        b = ctx.get('b') or 0
        result = a + b
        return ctx.insert('result', result)

async def execute_pipeline(initial_data):
    ctx_data = initial_data.copy()
    
    # Link 1: add
    add_link = AddNumbersLink()
    add_ctx = Context(ctx_data)
    add_result = await add_link.call(add_ctx)
    ctx_data = add_result.to_dict()
    
    return ctx_data
'''
        
        chain = converter.code_to_chain(code)
        
        # Verify chain structure
        assert len(chain._links) == 1
        assert 'add' in chain._links
    
    def test_code_to_chain_multiple_links(self, converter):
        """Test converting code with multiple links to a chain."""
        code = '''
from codeuchain.core import Context

class AddNumbersLink:
    async def call(self, ctx: Context) -> Context:
        return ctx.insert('result', 42)

class MultiplyLink:
    async def call(self, ctx: Context) -> Context:
        return ctx.insert('result', 84)

async def execute_pipeline(initial_data):
    ctx_data = initial_data.copy()
    
    add_link = AddNumbersLink()
    add_ctx = Context(ctx_data)
    add_result = await add_link.call(add_ctx)
    ctx_data = add_result.to_dict()
    
    multiply_link = MultiplyLink()
    multiply_ctx = Context(ctx_data)
    multiply_result = await multiply_link.call(multiply_ctx)
    ctx_data = multiply_result.to_dict()
    
    return ctx_data
'''
        
        chain = converter.code_to_chain(code)
        
        # Verify chain has both links
        assert len(chain._links) == 2
        assert 'add' in chain._links
        assert 'multiply' in chain._links
    
    def test_code_to_chain_invalid_function(self, converter):
        """Test error handling for invalid function name."""
        code = '''
async def wrong_name(initial_data):
    return initial_data
'''
        
        with pytest.raises(ValueError, match="Function 'execute_pipeline' not found"):
            converter.code_to_chain(code)


class TestValidation:
    """Tests for validation between chain and converted code."""
    
    @pytest.mark.asyncio
    async def test_validate_simple_chain(self, simple_chain, converter):
        """Test validation of a simple chain conversion."""
        # Generate code
        code = converter.chain_to_code(simple_chain)
        
        # Execute code to get function
        namespace = {'Context': Context, 'AddNumbersLink': AddNumbersLink, 'MultiplyLink': MultiplyLink}
        exec(code, namespace)
        converted_func = namespace['execute_pipeline']
        
        # Test contexts
        test_contexts = [
            {'a': 5, 'b': 3, 'factor': 2},  # (5+3)*2 = 16
            {'a': 10, 'b': 20, 'factor': 3},  # (10+20)*3 = 90
            {'a': 0, 'b': 0, 'factor': 5},  # (0+0)*5 = 0
        ]
        
        # Validate
        is_valid, errors = converter.validate(simple_chain, converted_func, test_contexts)
        
        assert is_valid, f"Validation failed: {errors}"
        assert len(errors) == 0
    
    @pytest.mark.asyncio
    async def test_validate_detects_differences(self, simple_chain, converter):
        """Test that validation detects when results differ."""
        # Create a function that produces different results
        async def wrong_func(initial_data):
            return {'result': 999}  # Always return wrong value
        
        test_contexts = [
            {'a': 5, 'b': 3, 'factor': 2},
        ]
        
        # Validate
        is_valid, errors = converter.validate(simple_chain, wrong_func, test_contexts)
        
        assert not is_valid
        assert len(errors) > 0
    
    @pytest.mark.asyncio
    async def test_validate_floating_point_tolerance(self, converter):
        """Test validation handles floating-point tolerance."""
        # Create a simple chain
        chain = Chain()
        
        class DivideLink:
            async def call(self, ctx: Context) -> Context:
                a = ctx.get('a')
                b = ctx.get('b')
                return ctx.insert('result', a / b)
        
        chain.add_link(DivideLink(), "divide")
        
        # Create a function with slight floating-point difference
        async def almost_same_func(initial_data):
            result = initial_data['a'] / initial_data['b']
            return {'a': initial_data['a'], 'b': initial_data['b'], 'result': result + 1e-10}
        
        test_contexts = [
            {'a': 10, 'b': 3},
        ]
        
        # Should be valid with tolerance
        is_valid, errors = converter.validate(chain, almost_same_func, test_contexts, tolerance=1e-9)
        assert not is_valid  # 1e-10 is within tolerance
        
        # Should be valid with larger tolerance
        is_valid, errors = converter.validate(chain, almost_same_func, test_contexts, tolerance=1e-8)
        assert is_valid


class TestOptimizedClassGeneration:
    """Tests for generating optimized classes."""
    
    def test_generate_optimized_class(self, simple_chain, converter):
        """Test generating an optimized class."""
        code = converter.generate_optimized_class(simple_chain, "OptimizedPipeline")
        
        # Verify class structure
        assert 'class OptimizedPipeline:' in code
        assert 'def __init__(self):' in code
        assert 'async def execute(' in code
        
        # Verify link initialization
        assert 'self.add_link = AddNumbersLink()' in code
        assert 'self.multiply_link = MultiplyLink()' in code
    
    def test_optimized_class_is_executable(self, simple_chain, converter):
        """Test that generated class can be instantiated and used."""
        code = converter.generate_optimized_class(simple_chain, "OptimizedPipeline")
        
        # Execute code to create class
        namespace = {'Context': Context, 'AddNumbersLink': AddNumbersLink, 'MultiplyLink': MultiplyLink}
        exec(code, namespace)
        
        # Instantiate class
        pipeline = namespace['OptimizedPipeline']()
        
        # Verify it has expected methods
        assert hasattr(pipeline, 'execute')
        assert hasattr(pipeline, 'add_link')
        assert hasattr(pipeline, 'multiply_link')
    
    @pytest.mark.asyncio
    async def test_optimized_class_execution(self, simple_chain, converter):
        """Test that optimized class produces correct results."""
        code = converter.generate_optimized_class(simple_chain, "OptimizedPipeline")
        
        # Execute code to create class
        namespace = {'Context': Context, 'AddNumbersLink': AddNumbersLink, 'MultiplyLink': MultiplyLink}
        exec(code, namespace)
        
        # Create instance and test
        pipeline = namespace['OptimizedPipeline']()
        result = await pipeline.execute({'a': 5, 'b': 3, 'factor': 2})
        
        assert result['result'] == 16  # (5+3)*2 = 16


class TestEndToEnd:
    """End-to-end tests for the complete conversion cycle."""
    
    @pytest.mark.asyncio
    async def test_chain_to_code_to_execution(self, complex_chain, converter):
        """Test complete cycle: chain → code → execution."""
        # Convert to code
        code = converter.chain_to_code(complex_chain)
        
        # Execute code
        namespace = {
            'Context': Context,
            'AddNumbersLink': AddNumbersLink,
            'MultiplyLink': MultiplyLink,
            'FormatResultLink': FormatResultLink
        }
        exec(code, namespace)
        converted_func = namespace['execute_pipeline']
        
        # Test execution
        result = await converted_func({'a': 5, 'b': 5, 'factor': 3})
        
        assert result['result'] == 30  # (5+5)*3 = 30
        assert result['formatted'] == 'Result: 30'
    
    @pytest.mark.asyncio
    async def test_roundtrip_conversion(self, simple_chain, converter):
        """Test roundtrip: chain → code → chain."""
        # Convert to code
        code = converter.chain_to_code(simple_chain)
        
        # Add link classes to code
        full_code = f'''
from codeuchain.core import Context

class AddNumbersLink:
    async def call(self, ctx: Context) -> Context:
        a = ctx.get('a') or 0
        b = ctx.get('b') or 0
        result = a + b
        return ctx.insert('result', result)

class MultiplyLink:
    async def call(self, ctx: Context) -> Context:
        result = ctx.get('result') or 0
        factor = ctx.get('factor') or 1
        multiplied = result * factor
        return ctx.insert('result', multiplied)

{code}
'''
        
        # Convert back to chain
        reconstructed_chain = converter.code_to_chain(full_code)
        
        # Verify structure
        assert len(reconstructed_chain._links) == len(simple_chain._links)
        
        # Test both produce same results
        test_ctx = {'a': 10, 'b': 5, 'factor': 2}
        
        original_result = await simple_chain.run(Context(test_ctx))
        reconstructed_result = await reconstructed_chain.run(Context(test_ctx))
        
        assert original_result.to_dict()['result'] == reconstructed_result.to_dict()['result']


class TestEdgeCases:
    """Tests for edge cases and error handling."""
    
    def test_empty_chain(self, converter):
        """Test converting an empty chain."""
        chain = Chain()
        code = converter.chain_to_code(chain)
        
        # Should generate valid code even for empty chain
        assert 'async def execute_pipeline' in code
        compile(code, '<string>', 'exec')
    
    def test_chain_with_complex_data(self, converter):
        """Test validation with complex nested data structures."""
        chain = Chain()
        
        class ComplexDataLink:
            async def call(self, ctx: Context) -> Context:
                data = {
                    'nested': {
                        'list': [1, 2, 3],
                        'dict': {'a': 1, 'b': 2}
                    },
                    'array': [4, 5, 6]
                }
                return ctx.insert('complex', data)
        
        chain.add_link(ComplexDataLink(), "complex")
        
        code = converter.chain_to_code(chain)
        
        # Should generate valid code
        compile(code, '<string>', 'exec')
