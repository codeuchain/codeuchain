using System.Threading.Tasks;

/// <summary>
/// Test Link: Doubles int values
/// </summary>
public class DoubleIntLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var valueStr = context.GetAny("result")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return Context<string>.Create(new Dictionary<string, object>
            {
                ["final"] = (value * 2).ToString()
            });
        }
        throw new InvalidOperationException("Cannot parse to int");
    }
}