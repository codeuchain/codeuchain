using System;
using System.Threading.Tasks;

/// <summary>
/// Main program demonstrating both the math example and performance comparison.
/// </summary>
public class ExampleProgram
{
    public static async Task Main(string[] args)
    {
        if (args.Length > 0 && args[0] == "performance")
        {
            await PerformanceComparison.RunComparisonAsync();
        }
        else
        {
            await MathProcessingExample.RunAsync();
        }
    }
}