using System.Threading.Tasks;

/// <summary>
/// Test Link: Doubles int values
/// </summary>
public class DoubleIntLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var valueStr = state.GetAny("result")?.ToString() ?? "0";
        if (int.TryParse(valueStr, out int value))
        {
            return State<string>.Create(new Dictionary<string, object>
            {
                ["final"] = (value * 2).ToString()
            });
        }
        throw new InvalidOperationException("Cannot parse to int");
    }
}