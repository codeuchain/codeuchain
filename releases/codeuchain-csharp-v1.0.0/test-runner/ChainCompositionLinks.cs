using System.Threading.Tasks;

/// <summary>
/// Double Value Link: Doubles numeric values
/// </summary>
public class DoubleValueLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var valueStr = context.GetAny("result")?.ToString() ?? context.GetAny("value")?.ToString() ?? context.GetAny("string")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return Context<string>.Create(new Dictionary<string, object>
            {
                ["result"] = (value * 2).ToString()
            });
        }
        return context;
    }
}

/// <summary>
/// Object to String Link: Converts values to strings
/// </summary>
public class ObjectToStringLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var value = context.GetAny("value")?.ToString() ?? "0";
        return Context<string>.Create(new Dictionary<string, object>
        {
            ["string"] = value
        });
    }
}

/// <summary>
/// Nested Chain Link: Wraps another chain
/// </summary>
public class NestedChainLink : IContextLink<string, string>
{
    private readonly Chain<string, string> _innerChain;

    public NestedChainLink(Chain<string, string> innerChain)
    {
        _innerChain = innerChain;
    }

    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        return await _innerChain.RunAsync(context);
    }
}

/// <summary>
/// String to Object Link: Processes string results
/// </summary>
public class StringToObjectLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var value = context.GetAny("result")?.ToString() ?? "0";
        return Context<string>.Create(new Dictionary<string, object>
        {
            ["final"] = value
        });
    }
}