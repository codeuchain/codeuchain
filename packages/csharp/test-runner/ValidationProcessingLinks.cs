using System.Threading.Tasks;

/// <summary>
/// Test Link: Validates data presence
/// </summary>
public class ValidationLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        if (!state.ContainsKey("data"))
            throw new InvalidOperationException("Missing data");
        return state.Insert("validated", true);
    }
}

/// <summary>
/// Test Link: Processes data to uppercase
/// </summary>
public class ProcessingLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var data = state.GetAny("data")?.ToString() ?? "";
        return state.Insert("processed", data.ToUpper());
    }
}

/// <summary>
/// Test Link: Formats processed data
/// </summary>
public class FormattingLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var data = state.GetAny("data")?.ToString() ?? "";
        var processed = state.GetAny("processed")?.ToString() ?? "";
        return state.Insert("formatted", $"[{processed}]");
    }
}