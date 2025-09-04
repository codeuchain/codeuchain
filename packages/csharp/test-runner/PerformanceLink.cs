using System.Threading.Tasks;

/// <summary>
/// Performance Link: Simulates processing work
/// </summary>
public class PerformanceLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var iterations = (int?)context.GetAny("iterations") ?? 10;
        var total = (int?)context.GetAny("total") ?? 0;

        // Simulate some processing
        for (int i = 0; i < iterations; i++)
        {
            total += 1;
            await Task.Delay(1); // Small delay to simulate work
        }

        // Preserve all existing data and update total
        var result = new Dictionary<string, object>();
        foreach (var key in context.Keys)
        {
            result[key] = context.GetAny(key);
        }
        result["total"] = total;
        return Context<string>.Create(result);
    }
}