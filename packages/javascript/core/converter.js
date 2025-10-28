/**
 * Chain Converter: Bidirectional CodeUChain Transformation
 *
 * Converts between CodeUChain (chain-based) and traditional imperative code.
 * Enables zero-overhead execution while maintaining maintainability.
 *
 * Features:
 * - CodeUChain → Traditional: Generates optimized imperative code with direct function calls
 * - Traditional → CodeUChain: Reconstructs chain definitions from imperative patterns
 * - Validation: Ensures both versions produce identical results
 */

const { Context, Chain, Link } = require('../core');

class ChainConverter {
  /**
   * Bidirectional converter between CodeUChain and traditional imperative code.
   *
   * @example
   * // Convert chain to traditional code
   * const converter = new ChainConverter();
   * const traditionalCode = converter.chainToCode(myChain);
   *
   * // Convert traditional code back to chain
   * const reconstructedChain = converter.codeToChain(traditionalCode);
   *
   * // Validate both produce same results
   * const isValid = await converter.validate(myChain, reconstructedChain, testContexts);
   */
  constructor() {
    this.indent = '  ';
  }

  /**
   * Convert a CodeUChain chain to optimized traditional imperative code.
   *
   * This generates zero-overhead code with direct function calls,
   * eliminating the chain orchestration layer at runtime.
   *
   * @param {Chain} chain - The CodeUChain chain to convert
   * @param {string} functionName - Name for the generated function
   * @param {boolean} includeJsDoc - Whether to include JSDoc comments
   * @param {boolean} includeClasses - Whether to include link class definitions
   * @returns {string} JavaScript source code as a string
   *
   * @example
   * const chain = new Chain();
   * chain.addLink(new ValidateLink(), 'validate');
   * chain.addLink(new ProcessLink(), 'process');
   * const converter = new ChainConverter();
   * const code = converter.chainToCode(chain);
   * console.log(code);
   */
  chainToCode(chain, functionName = 'executePipeline', includeJsDoc = true, includeClasses = false) {
    const linksInfo = this._extractLinksInfo(chain);

    const lines = [];

    // Add header comment
    lines.push('// Auto-generated from CodeUChain');
    lines.push('// Zero-overhead traditional code');
    lines.push('');

    // Optionally include link class implementations
    if (includeClasses) {
      for (const linkInfo of linksInfo) {
        if (linkInfo.sourceCode) {
          lines.push(linkInfo.sourceCode);
          lines.push('');
        }
      }
    }

    // Function signature
    if (includeJsDoc) {
      lines.push('/**');
      lines.push(' * Zero-overhead execution of the chain logic.');
      lines.push(' *');
      lines.push(' * This function provides identical functionality to the original chain');
      lines.push(' * but with direct function calls for maximum performance.');
      lines.push(' *');
      lines.push(' * @param {Object} initialData - Initial context data');
      lines.push(' * @returns {Promise<Object>} Final context data');
      lines.push(' */');
    }
    lines.push(`async function ${functionName}(initialData) {`);

    // Initialize context
    lines.push(`${this.indent}// Initialize context data`);
    lines.push(`${this.indent}let ctxData = { ...initialData };`);
    lines.push('');

    // Generate direct calls for each link
    let linkIndex = 0;
    for (const linkInfo of linksInfo) {
      linkIndex++;
      lines.push(`${this.indent}// Link ${linkIndex}: ${linkInfo.name}`);

      // Create link instance
      const className = linkInfo.linkInstance.constructor.name;
      lines.push(`${this.indent}const ${linkInfo.name}Link = new ${className}();`);

      // Create context and call
      lines.push(`${this.indent}const ${linkInfo.name}Ctx = new Context(ctxData);`);
      lines.push(`${this.indent}const ${linkInfo.name}Result = await ${linkInfo.name}Link.call(${linkInfo.name}Ctx);`);
      lines.push(`${this.indent}ctxData = ${linkInfo.name}Result.toObject();`);
      lines.push('');
    }

    // Return final context
    lines.push(`${this.indent}return ctxData;`);
    lines.push('}');

    return lines.join('\n');
  }

  /**
   * Extract information about all links in the chain.
   *
   * @private
   * @param {Chain} chain - The chain to extract from
   * @returns {Array<Object>} Array of link information objects
   */
  _extractLinksInfo(chain) {
    const linksInfo = [];

    for (const [name, link] of chain._links.entries()) {
      const linkInfo = {
        name: name,
        linkInstance: link,
        callMethod: link.call,
        sourceCode: this._extractSourceCode(link)
      };

      linksInfo.push(linkInfo);
    }

    return linksInfo;
  }

  /**
   * Extract source code for a link class.
   *
   * @private
   * @param {Link} link - The link to extract source from
   * @returns {string|null} Source code or null if unavailable
   */
  _extractSourceCode(link) {
    try {
      // Get the class source from its toString
      const className = link.constructor.name;
      const classCode = link.constructor.toString();

      // Format as a class declaration
      if (classCode.includes('class ')) {
        return classCode;
      } else {
        // It's a function constructor, convert to class syntax
        return `class ${className} {\n${classCode}\n}`;
      }
    } catch (error) {
      return null;
    }
  }

  /**
   * Convert traditional imperative code back to a CodeUChain chain.
   *
   * This analyzes the code structure and reconstructs the chain definition,
   * enabling maintenance and modification using the CodeUChain framework.
   *
   * Note: This is a simplified implementation that extracts link patterns
   * from code comments. For full AST-based parsing, consider using a
   * dedicated parser library.
   *
   * @param {string} code - JavaScript source code to analyze
   * @param {string} functionName - Name of the function to convert
   * @returns {Chain} A reconstructed CodeUChain chain
   *
   * @example
   * const code = `
   * async function executePipeline(initialData) {
   *   let ctxData = { ...initialData };
   *   // Link 1: validate
   *   const validateLink = new ValidateLink();
   *   const validateCtx = new Context(ctxData);
   *   const validateResult = await validateLink.call(validateCtx);
   *   ctxData = validateResult.toObject();
   *   return ctxData;
   * }
   * `;
   * const converter = new ChainConverter();
   * const chain = converter.codeToChain(code);
   */
  codeToChain(code, functionName = 'executePipeline') {
    // Extract link patterns using regex (simplified approach)
    const linkPattern = /const\s+(\w+)Link\s*=\s*new\s+(\w+)\(\)/g;
    const links = [];
    
    let match;
    while ((match = linkPattern.exec(code)) !== null) {
      const linkName = match[1];
      const className = match[2];
      links.push({ linkName, className });
    }

    // Build the chain
    const chain = new Chain();

    // Try to execute the code to get the link classes
    try {
      // Use Function constructor to evaluate code
      const evalFunc = new Function('Context', 'Chain', 'Link', `
        ${code}
        return { ${links.map(l => l.className).join(', ')} };
      `);

      const linkClasses = evalFunc(Context, Chain, Link);

      // Add links to chain
      for (const linkInfo of links) {
        if (linkClasses[linkInfo.className]) {
          const linkInstance = new linkClasses[linkInfo.className]();
          chain.addLink(linkInstance, linkInfo.linkName);
        }
      }
    } catch (error) {
      // If we can't evaluate, return empty chain structure
      console.warn(`Could not evaluate code: ${error.message}`);
    }

    return chain;
  }

  /**
   * Validate that chain and converted function produce identical results.
   *
   * @param {Chain} chain - Original CodeUChain chain
   * @param {Function} convertedFunc - Converted traditional function
   * @param {Array<Object>} testContexts - List of test input contexts
   * @param {number} tolerance - Tolerance for floating-point comparisons
   * @returns {Promise<Object>} Object with {isValid, errors}
   *
   * @example
   * const chain = myChain;
   * const code = converter.chainToCode(chain);
   * eval(code);
   * const testData = [{ x: 1 }, { x: 2 }, { x: 3 }];
   * const result = await converter.validate(chain, executePipeline, testData);
   * console.log(result.isValid, result.errors);
   */
  async validate(chain, convertedFunc, testContexts, tolerance = 1e-9) {
    const errors = [];

    for (let i = 0; i < testContexts.length; i++) {
      const testCtx = testContexts[i];

      try {
        // Run chain
        const chainResult = await chain.run(new Context(testCtx));
        const chainData = chainResult.toObject();

        // Run converted function
        const funcResult = await convertedFunc(testCtx);

        // Compare results
        if (!this._compareObjects(chainData, funcResult, tolerance)) {
          errors.push(
            `Test case ${i}: Results differ\n` +
            `  Chain result: ${JSON.stringify(chainData)}\n` +
            `  Function result: ${JSON.stringify(funcResult)}`
          );
        }
      } catch (error) {
        errors.push(`Test case ${i}: Exception occurred: ${error.message}`);
      }
    }

    return {
      isValid: errors.length === 0,
      errors: errors
    };
  }

  /**
   * Compare two objects with tolerance for floating-point values.
   *
   * @private
   * @param {Object} obj1 - First object to compare
   * @param {Object} obj2 - Second object to compare
   * @param {number} tolerance - Tolerance for floating-point comparisons
   * @returns {boolean} True if objects are equal within tolerance
   */
  _compareObjects(obj1, obj2, tolerance) {
    if (obj1 === obj2) return true;
    if (obj1 == null || obj2 == null) return false;
    if (typeof obj1 !== typeof obj2) return false;

    if (typeof obj1 === 'number' && typeof obj2 === 'number') {
      return Math.abs(obj1 - obj2) <= tolerance;
    }

    if (Array.isArray(obj1) && Array.isArray(obj2)) {
      if (obj1.length !== obj2.length) return false;
      for (let i = 0; i < obj1.length; i++) {
        if (!this._compareObjects(obj1[i], obj2[i], tolerance)) {
          return false;
        }
      }
      return true;
    }

    if (typeof obj1 === 'object' && typeof obj2 === 'object') {
      const keys1 = Object.keys(obj1);
      const keys2 = Object.keys(obj2);

      if (keys1.length !== keys2.length) return false;

      for (const key of keys1) {
        if (!keys2.includes(key)) return false;
        if (!this._compareObjects(obj1[key], obj2[key], tolerance)) {
          return false;
        }
      }

      return true;
    }

    return obj1 === obj2;
  }

  /**
   * Generate an optimized class that encapsulates the chain logic.
   *
   * This creates a reusable class with zero-overhead execution,
   * suitable for production use.
   *
   * @param {Chain} chain - The CodeUChain chain to convert
   * @param {string} className - Name for the generated class
   * @param {boolean} includeClasses - Whether to include link class definitions
   * @returns {string} JavaScript source code for the optimized class
   *
   * @example
   * const chain = myChain;
   * const code = converter.generateOptimizedClass(chain, 'MyOptimizedPipeline');
   * eval(code);
   * const pipeline = new MyOptimizedPipeline();
   * const result = await pipeline.execute({ input: 'data' });
   */
  generateOptimizedClass(chain, className = 'OptimizedPipeline', includeClasses = false) {
    const linksInfo = this._extractLinksInfo(chain);

    const lines = [];

    // Add header
    lines.push('// Auto-generated optimized pipeline class');
    lines.push('');

    // Optionally include link class definitions
    if (includeClasses) {
      for (const linkInfo of linksInfo) {
        if (linkInfo.sourceCode) {
          lines.push(linkInfo.sourceCode);
          lines.push('');
        }
      }
    }

    // Class definition
    lines.push(`class ${className} {`);
    lines.push(`${this.indent}/**`);
    lines.push(`${this.indent} * Zero-overhead optimized pipeline.`);
    lines.push(`${this.indent} *`);
    lines.push(`${this.indent} * This class provides the same functionality as the original chain`);
    lines.push(`${this.indent} * but with direct method calls for maximum performance.`);
    lines.push(`${this.indent} */`);
    lines.push('');

    // Constructor
    lines.push(`${this.indent}constructor() {`);
    lines.push(`${this.indent}${this.indent}// Initialize link instances`);
    for (const linkInfo of linksInfo) {
      const linkClassName = linkInfo.linkInstance.constructor.name;
      lines.push(`${this.indent}${this.indent}this.${linkInfo.name}Link = new ${linkClassName}();`);
    }
    lines.push(`${this.indent}}`);
    lines.push('');

    // Execute method
    lines.push(`${this.indent}/**`);
    lines.push(`${this.indent} * Execute the pipeline with zero overhead.`);
    lines.push(`${this.indent} * @param {Object} initialData - Initial context data`);
    lines.push(`${this.indent} * @returns {Promise<Object>} Final context data`);
    lines.push(`${this.indent} */`);
    lines.push(`${this.indent}async execute(initialData) {`);
    lines.push(`${this.indent}${this.indent}let ctxData = { ...initialData };`);
    lines.push('');

    let linkIndex = 0;
    for (const linkInfo of linksInfo) {
      linkIndex++;
      lines.push(`${this.indent}${this.indent}// Link ${linkIndex}: ${linkInfo.name}`);
      lines.push(`${this.indent}${this.indent}const ${linkInfo.name}Ctx = new Context(ctxData);`);
      lines.push(`${this.indent}${this.indent}const ${linkInfo.name}Result = await this.${linkInfo.name}Link.call(${linkInfo.name}Ctx);`);
      lines.push(`${this.indent}${this.indent}ctxData = ${linkInfo.name}Result.toObject();`);
      lines.push('');
    }

    lines.push(`${this.indent}${this.indent}return ctxData;`);
    lines.push(`${this.indent}}`);
    lines.push('}');

    return lines.join('\n');
  }
}

module.exports = { ChainConverter };
