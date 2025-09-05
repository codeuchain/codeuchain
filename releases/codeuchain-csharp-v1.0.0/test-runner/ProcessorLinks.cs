using System.Threading.Tasks;

/// <summary>
/// Test Link: Untyped processor
/// </summary>
public class UntypedProcessorLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var processed = context.GetAny("processed")?.ToString() ?? "";
        return context.Insert("untyped", "processed");
    }
}

/// <summary>
/// Test Link: Typed processor
/// </summary>
public class TypedProcessorLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        return context.Insert("typed", "processed");
    }
}