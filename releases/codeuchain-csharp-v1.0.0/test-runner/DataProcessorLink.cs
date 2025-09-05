using System.Threading.Tasks;

/// <summary>
/// Test Link: Processes data with multiplier
/// </summary>
public class DataProcessorLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var data = context.GetAny("data")?.ToString() ?? "";
        var multiplier = (int?)context.GetAny("multiplier") ?? 1;
        return Context<string>.Create(new Dictionary<string, object>
        {
            ["processed"] = data.ToUpper(),
            ["calculated"] = multiplier * 2
        });
    }
}