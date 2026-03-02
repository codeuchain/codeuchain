using System.Threading.Tasks;

/// <summary>
/// Double Value Link: Doubles numeric values
/// </summary>
public class DoubleValueLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var valueStr = state.GetAny("result")?.ToString() ?? state.GetAny("value")?.ToString() ?? state.GetAny("string")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return State<string>.Create(new Dictionary<string, object>
            {
                ["result"] = (value * 2).ToString()
            });
        }
        return state;
    }
}

/// <summary>
/// Object to String Link: Converts values to strings
/// </summary>
public class ObjectToStringLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var value = state.GetAny("value")?.ToString() ?? "0";
        return State<string>.Create(new Dictionary<string, object>
        {
            ["string"] = value
        });
    }
}

/// <summary>
/// Nested Chain Link: Wraps another chain
/// </summary>
public class NestedChainLink : IStateLink<string, string>
{
    private readonly Chain<string, string> _innerChain;

    public NestedChainLink(Chain<string, string> innerChain)
    {
        _innerChain = innerChain;
    }

    public async Task<State<string>> CallAsync(State<string> state)
    {
        return await _innerChain.RunAsync(state);
    }
}

/// <summary>
/// String to Object Link: Processes string results
/// </summary>
public class StringToObjectLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var value = state.GetAny("result")?.ToString() ?? "0";
        return State<string>.Create(new Dictionary<string, object>
        {
            ["final"] = value
        });
    }
}