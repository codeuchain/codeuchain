using System.Threading.Tasks;

/// <summary>
/// Test Link: Untyped processor
/// </summary>
public class UntypedProcessorLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var processed = state.GetAny("processed")?.ToString() ?? "";
        return state.Insert("untyped", "processed");
    }
}

/// <summary>
/// Test Link: Typed processor
/// </summary>
public class TypedProcessorLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        return state.Insert("typed", "processed");
    }
}