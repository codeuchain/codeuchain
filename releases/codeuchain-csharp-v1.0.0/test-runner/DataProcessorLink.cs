using System.Threading.Tasks;

/// <summary>
/// Test Link: Processes data with multiplier
/// </summary>
public class DataProcessorLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var data = state.GetAny("data")?.ToString() ?? "";
        var multiplier = (int?)state.GetAny("multiplier") ?? 1;
        return State<string>.Create(new Dictionary<string, object>
        {
            ["processed"] = data.ToUpper(),
            ["calculated"] = multiplier * 2
        });
    }
}