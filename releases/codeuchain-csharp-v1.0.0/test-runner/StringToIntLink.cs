using System.Threading.Tasks;

/// <summary>
/// Test Link: Converts string to int
/// </summary>
public class StringToIntLink : IContextLink<string, string>
{
    public async Task<Context<string>> CallAsync(Context<string> context)
    {
        var value = context.GetAny("value")?.ToString();
        if (int.TryParse(value, out int result))
        {
            return Context<string>.Create(new Dictionary<string, object>
            {
                ["result"] = result.ToString()
            });
        }
        throw new InvalidOperationException("Cannot parse to int");
    }
}