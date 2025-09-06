using System.Threading.Tasks;

/// <summary>
/// Legacy Processor: Implements old ILink interface
/// </summary>
public class LegacyProcessor : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var input = context.Get("input")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("output", input.ToUpper()));
    }
}

/// <summary>
/// Modern Processor: Implements old ILink interface with chaining
/// </summary>
public class ModernProcessor : ILink
{
    public ValueTask<Context> ProcessAsync(Context context)
    {
        var input = context.Get("input")?.ToString() ?? "";
        var output = context.Get("output")?.ToString() ?? "";
        return ValueTask.FromResult(context.Insert("output", input.ToUpper()).Insert("final", $"{output}-MODERN"));
    }
}