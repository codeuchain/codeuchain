using System.Threading.Tasks;

/// <summary>
/// Test Link: Converts string to int
/// </summary>
public class StringToIntLink : IStateLink<string, string>
{
    public async Task<State<string>> CallAsync(State<string> state)
    {
        var value = state.GetAny("value")?.ToString();
        if (int.TryParse(value, out int result))
        {
            return State<string>.Create(new Dictionary<string, object>
            {
                ["result"] = result.ToString()
            });
        }
        throw new InvalidOperationException("Cannot parse to int");
    }
}