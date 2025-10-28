"""
Chain Converter: Bidirectional CodeUChain Transformation

Converts between CodeUChain (chain-based) and traditional imperative code.
Enables zero-overhead execution while maintaining maintainability.

Features:
- CodeUChain → Traditional: Generates optimized imperative code with direct function calls
- Traditional → CodeUChain: Reconstructs chain definitions from imperative patterns
- Validation: Ensures both versions produce identical results
"""

import ast
import inspect
import textwrap
from typing import Any, Dict, List, Optional, Callable, Type
from dataclasses import dataclass

from ..core import Context, Chain, Link


@dataclass
class LinkInfo:
    """Information about a link in the chain."""
    name: str
    link_instance: Any
    call_method: Callable
    source_code: Optional[str] = None


class ChainConverter:
    """
    Bidirectional converter between CodeUChain and traditional imperative code.
    
    Usage:
        # Convert chain to traditional code
        converter = ChainConverter()
        traditional_code = converter.chain_to_code(my_chain)
        
        # Convert traditional code back to chain
        reconstructed_chain = converter.code_to_chain(traditional_code)
        
        # Validate both produce same results
        is_valid = converter.validate(my_chain, reconstructed_chain, test_contexts)
    """
    
    def __init__(self):
        self._indent = "    "
    
    def chain_to_code(
        self, 
        chain: Chain, 
        function_name: str = "execute_pipeline",
        include_types: bool = True
    ) -> str:
        """
        Convert a CodeUChain chain to optimized traditional imperative code.
        
        This generates zero-overhead code with direct function calls,
        eliminating the chain orchestration layer at runtime.
        
        Args:
            chain: The CodeUChain chain to convert
            function_name: Name for the generated function
            include_types: Whether to include type hints
            
        Returns:
            Python source code as a string
            
        Example:
            >>> chain = Chain()
            >>> chain.add_link(ValidateLink(), "validate")
            >>> chain.add_link(ProcessLink(), "process")
            >>> converter = ChainConverter()
            >>> code = converter.chain_to_code(chain)
            >>> print(code)
        """
        links_info = self._extract_links_info(chain)
        
        # Build the function
        lines = []
        
        # Add imports
        lines.append("# Auto-generated from CodeUChain")
        lines.append("# Zero-overhead traditional code")
        lines.append("from typing import Any, Dict")
        lines.append("")
        
        # Extract and add link implementations
        for link_info in links_info:
            if link_info.source_code:
                lines.append(link_info.source_code)
                lines.append("")
        
        # Function signature
        if include_types:
            lines.append(f"async def {function_name}(initial_data: Dict[str, Any]) -> Dict[str, Any]:")
        else:
            lines.append(f"async def {function_name}(initial_data):")
        
        lines.append(f'{self._indent}"""')
        lines.append(f"{self._indent}Zero-overhead execution of the chain logic.")
        lines.append(f"{self._indent}")
        lines.append(f"{self._indent}This function provides identical functionality to the original chain")
        lines.append(f"{self._indent}but with direct function calls for maximum performance.")
        lines.append(f'{self._indent}"""')
        
        # Initialize context
        lines.append(f"{self._indent}# Initialize context data")
        lines.append(f"{self._indent}ctx_data = initial_data.copy()")
        lines.append("")
        
        # Generate direct calls for each link
        for i, link_info in enumerate(links_info):
            lines.append(f"{self._indent}# Link {i+1}: {link_info.name}")
            
            # Create link instance
            class_name = link_info.link_instance.__class__.__name__
            lines.append(f"{self._indent}{link_info.name}_link = {class_name}()")
            
            # Create context and call
            lines.append(f"{self._indent}{link_info.name}_ctx = Context(ctx_data)")
            lines.append(f"{self._indent}{link_info.name}_result = await {link_info.name}_link.call({link_info.name}_ctx)")
            lines.append(f"{self._indent}ctx_data = {link_info.name}_result.to_dict()")
            lines.append("")
        
        # Return final context
        lines.append(f"{self._indent}return ctx_data")
        
        return "\n".join(lines)
    
    def _extract_links_info(self, chain: Chain) -> List[LinkInfo]:
        """Extract information about all links in the chain."""
        links_info = []
        
        for name, link in chain._links.items():
            # Get the call method
            call_method = getattr(link, 'call', None)
            
            # Try to get source code
            source_code = None
            try:
                source = inspect.getsource(link.__class__)
                source_code = textwrap.dedent(source)
            except (OSError, TypeError):
                # Can't get source for built-in or C-extension classes
                pass
            
            links_info.append(LinkInfo(
                name=name,
                link_instance=link,
                call_method=call_method,
                source_code=source_code
            ))
        
        return links_info
    
    def code_to_chain(
        self, 
        code: str,
        function_name: str = "execute_pipeline"
    ) -> Chain:
        """
        Convert traditional imperative code back to a CodeUChain chain.
        
        This analyzes the code structure and reconstructs the chain definition,
        enabling maintenance and modification using the CodeUChain framework.
        
        Args:
            code: Python source code to analyze
            function_name: Name of the function to convert
            
        Returns:
            A reconstructed CodeUChain chain
            
        Example:
            >>> code = '''
            ... async def execute_pipeline(initial_data):
            ...     ctx_data = initial_data.copy()
            ...     # Link 1: validate
            ...     validate_link = ValidateLink()
            ...     validate_ctx = Context(ctx_data)
            ...     validate_result = await validate_link.call(validate_ctx)
            ...     ctx_data = validate_result.to_dict()
            ...     return ctx_data
            ... '''
            >>> converter = ChainConverter()
            >>> chain = converter.code_to_chain(code)
        """
        # Parse the code
        tree = ast.parse(code)
        
        # Find the target function
        func_node = None
        for node in ast.walk(tree):
            if isinstance(node, ast.AsyncFunctionDef) and node.name == function_name:
                func_node = node
                break
        
        if not func_node:
            raise ValueError(f"Function '{function_name}' not found in code")
        
        # Extract link patterns from the function body
        links = self._extract_links_from_ast(func_node)
        
        # Build the chain
        chain = Chain()
        
        # Execute the code to get the link classes
        namespace = {}
        exec(code, namespace)
        
        # Add links to chain
        for link_name, link_class_name in links:
            if link_class_name in namespace:
                link_instance = namespace[link_class_name]()
                chain.add_link(link_instance, link_name)
        
        return chain
    
    def _extract_links_from_ast(self, func_node: ast.AsyncFunctionDef) -> List[tuple]:
        """Extract link information from the AST of a function."""
        links = []
        
        for node in ast.walk(func_node):
            # Look for patterns like: link_name_link = ClassName()
            if isinstance(node, ast.Assign):
                if len(node.targets) == 1:
                    target = node.targets[0]
                    if isinstance(target, ast.Name) and target.id.endswith('_link'):
                        if isinstance(node.value, ast.Call):
                            if isinstance(node.value.func, ast.Name):
                                link_name = target.id.replace('_link', '')
                                class_name = node.value.func.id
                                links.append((link_name, class_name))
        
        return links
    
    async def validate(
        self, 
        chain: Chain, 
        converted_func: Callable,
        test_contexts: List[Dict[str, Any]],
        tolerance: float = 1e-9
    ) -> tuple[bool, List[str]]:
        """
        Validate that chain and converted function produce identical results.
        
        Args:
            chain: Original CodeUChain chain
            converted_func: Converted traditional function
            test_contexts: List of test input contexts
            tolerance: Tolerance for floating-point comparisons
            
        Returns:
            Tuple of (is_valid, error_messages)
            
        Example:
            >>> chain = my_chain
            >>> code = converter.chain_to_code(chain)
            >>> exec(code, globals())
            >>> test_data = [{"x": 1}, {"x": 2}, {"x": 3}]
            >>> is_valid, errors = await converter.validate(chain, execute_pipeline, test_data)
        """
        errors = []
        
        for i, test_ctx in enumerate(test_contexts):
            try:
                # Run chain
                chain_result = await chain.run(Context(test_ctx))
                chain_data = chain_result.to_dict()
                
                # Run converted function
                func_result = await converted_func(test_ctx)
                
                # Compare results
                if not self._compare_dicts(chain_data, func_result, tolerance):
                    errors.append(
                        f"Test case {i}: Results differ\n"
                        f"  Chain result: {chain_data}\n"
                        f"  Function result: {func_result}"
                    )
            except Exception as e:
                errors.append(f"Test case {i}: Exception occurred: {str(e)}")
        
        return len(errors) == 0, errors
    
    def _compare_dicts(
        self, 
        dict1: Dict[str, Any], 
        dict2: Dict[str, Any],
        tolerance: float
    ) -> bool:
        """Compare two dictionaries with tolerance for floating-point values."""
        if set(dict1.keys()) != set(dict2.keys()):
            return False
        
        for key in dict1.keys():
            val1, val2 = dict1[key], dict2[key]
            
            # Handle floating-point comparison
            if isinstance(val1, (float, int)) and isinstance(val2, (float, int)):
                if abs(float(val1) - float(val2)) > tolerance:
                    return False
            # Handle nested dicts
            elif isinstance(val1, dict) and isinstance(val2, dict):
                if not self._compare_dicts(val1, val2, tolerance):
                    return False
            # Handle lists
            elif isinstance(val1, list) and isinstance(val2, list):
                if len(val1) != len(val2):
                    return False
                for v1, v2 in zip(val1, val2):
                    if isinstance(v1, dict) and isinstance(v2, dict):
                        if not self._compare_dicts(v1, v2, tolerance):
                            return False
                    elif v1 != v2:
                        return False
            # Direct comparison
            elif val1 != val2:
                return False
        
        return True
    
    def generate_optimized_class(
        self,
        chain: Chain,
        class_name: str = "OptimizedPipeline"
    ) -> str:
        """
        Generate an optimized class that encapsulates the chain logic.
        
        This creates a reusable class with zero-overhead execution,
        suitable for production use.
        
        Args:
            chain: The CodeUChain chain to convert
            class_name: Name for the generated class
            
        Returns:
            Python source code for the optimized class
            
        Example:
            >>> chain = my_chain
            >>> code = converter.generate_optimized_class(chain, "MyOptimizedPipeline")
            >>> exec(code)
            >>> pipeline = MyOptimizedPipeline()
            >>> result = await pipeline.execute({"input": "data"})
        """
        links_info = self._extract_links_info(chain)
        
        lines = []
        
        # Add imports
        lines.append("# Auto-generated optimized pipeline class")
        lines.append("from typing import Any, Dict")
        lines.append("")
        
        # Extract link class definitions
        for link_info in links_info:
            if link_info.source_code:
                lines.append(link_info.source_code)
                lines.append("")
        
        # Class definition
        lines.append(f"class {class_name}:")
        lines.append(f'{self._indent}"""')
        lines.append(f"{self._indent}Zero-overhead optimized pipeline.")
        lines.append(f"{self._indent}")
        lines.append(f"{self._indent}This class provides the same functionality as the original chain")
        lines.append(f"{self._indent}but with direct method calls for maximum performance.")
        lines.append(f'{self._indent}"""')
        lines.append("")
        
        # Constructor
        lines.append(f"{self._indent}def __init__(self):")
        lines.append(f"{self._indent}{self._indent}# Initialize link instances")
        for link_info in links_info:
            class_name_str = link_info.link_instance.__class__.__name__
            lines.append(f"{self._indent}{self._indent}self.{link_info.name}_link = {class_name_str}()")
        lines.append("")
        
        # Execute method
        lines.append(f"{self._indent}async def execute(self, initial_data: Dict[str, Any]) -> Dict[str, Any]:")
        lines.append(f'{self._indent}{self._indent}"""Execute the pipeline with zero overhead."""')
        lines.append(f"{self._indent}{self._indent}ctx_data = initial_data.copy()")
        lines.append("")
        
        for i, link_info in enumerate(links_info):
            lines.append(f"{self._indent}{self._indent}# Link {i+1}: {link_info.name}")
            lines.append(f"{self._indent}{self._indent}{link_info.name}_ctx = Context(ctx_data)")
            lines.append(f"{self._indent}{self._indent}{link_info.name}_result = await self.{link_info.name}_link.call({link_info.name}_ctx)")
            lines.append(f"{self._indent}{self._indent}ctx_data = {link_info.name}_result.to_dict()")
            lines.append("")
        
        lines.append(f"{self._indent}{self._indent}return ctx_data")
        
        return "\n".join(lines)
