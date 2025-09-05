using System;
using System.Threading.Tasks;

/// <summary>
/// Comprehensive generic examples program.
/// </summary>
public class GenericExamplesProgram
{
    public static async Task Main(string[] args)
    {
        if (args.Length > 0 && args[0] == "performance")
        {
            await GenericPerformanceComparison.RunComparisonAsync();
        }
        else if (args.Length > 0 && args[0] == "patterns")
        {
            AdvancedGenericPatterns.DemonstratePatterns();
        }
        else
        {
            Console.WriteLine("=== CodeUChain C# Generic Examples ===\n");

            // Run all examples
            await GenericContextExample.RunAsync();
            await GenericLinkExample.RunAsync();
            await GenericChainExample.RunAsync();
            AdvancedGenericPatterns.DemonstratePatterns();

            Console.WriteLine("Run with 'performance' argument for performance comparison");
            Console.WriteLine("Run with 'patterns' argument for advanced patterns only");
        }
    }
}