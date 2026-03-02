using System.Threading.Tasks;

/// <summary>
/// Performance Link: Simulates processing work
/// </summary>
public class PerformanceLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var iterations = (int?)state.GetAny("iterations") ?? 10;
        var total = (int?)state.GetAny("total") ?? 0;

        // Simulate some processing
        for (int i = 0; i < iterations; i++)
        {
            total += 1;
            await Task.Delay(1); // Small delay to simulate work
        }

        // Preserve all existing data and update total
        var result = new Dictionary<string, object>();
        foreach (var key in state.Keys)
        {
            result[key] = state.GetAny(key);
        }
        result["total"] = total;
        return State<string>.Create(result);
    }
}