using System.Threading.Tasks;

/// <summary>
/// Test Link: Validates data presence
/// </summary>
public class ValidationLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        if (!context.ContainsKey("data"))
            throw new InvalidOperationException("Missing data");
        return context.Insert("validated", true);
    }
}

/// <summary>
/// Test Link: Processes data to uppercase
/// </summary>
public class ProcessingLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var data = context.GetAny("data")?.ToString() ?? "";
        return context.Insert("processed", data.ToUpper());
    }
}

/// <summary>
/// Test Link: Formats processed data
/// </summary>
public class FormattingLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var data = context.GetAny("data")?.ToString() ?? "";
        var processed = context.GetAny("processed")?.ToString() ?? "";
        return context.Insert("formatted", $"[{processed}]");
    }
}