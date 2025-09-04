# Installing FastestMCP CLI Globally

This guide provides multiple methods to install the FastestMCP CLI globally on your system, allowing you to use it from anywhere without adding it as a dependency to your projects.

## Prerequisites

- Python 3.10 or higher
- pip (Python package installer)

## Method 1: Using pipx (Recommended - Isolated Environment)

pipx is the recommended way to install Python CLI tools globally. It creates isolated environments for each package, preventing dependency conflicts.

### Step 1: Install pipx

```bash
pip install pipx
```

### Step 2: Ensure pipx is in your PATH

```bash
pipx ensurepath
```

**Note:** You may need to restart your terminal or source your shell configuration after this step.

### Step 3: Install FastestMCP

```bash
pipx install fastestmcp
```

### Step 4: Verify Installation

```bash
fastestmcp --help
```

You should see the help output showing available commands.

## Method 2: Traditional pip Global Install

If you prefer a traditional global installation, you can use pip with the `--user` flag.

### Install FastestMCP Globally

```bash
pip install --user fastestmcp
```

### Upgrade to Latest Version

```bash
pip install --user --upgrade fastestmcp
```

### Verify Installation

```bash
fastestmcp --help
```

## Method 3: Run from Cloned Repository

If you have the repository cloned locally, you can run FastestMCP directly without installation.

### Clone the Repository

```bash
git clone https://github.com/orchestrate-solutions/fastestmcp.git
cd fastestmcp
```

### Run Directly

```bash
# From the repository root
python -m fastestmcp.cli --help

# Or create an alias in your shell profile
alias fastestmcp="python -m fastestmcp.cli"
```

## Troubleshooting

### Command Not Found

If `fastestmcp` command is not found after installation:

1. **For pipx installations:** Ensure `~/.local/bin` is in your PATH
2. **For pip --user installations:** Ensure `~/.local/bin` or the user site-packages bin directory is in your PATH
3. **Restart your terminal** or source your shell configuration file

### Permission Errors

If you encounter permission errors with global installations:

- Use `pipx` (Method 1) - it doesn't require admin privileges
- Use `pip install --user` (Method 2) - installs to user directory
- Avoid `sudo pip install` as it can cause system conflicts

### PATH Issues on Windows

For Windows users, you may need to manually add directories to your PATH:

1. Open System Properties → Advanced → Environment Variables
2. Find the `PATH` variable in User variables
3. Add the appropriate directory:
   - For pipx: `C:\Users\<username>\.local\bin`
   - For pip --user: `C:\Users\<username>\AppData\Roaming\Python\Python3x\Scripts\`

### Verify Python Version

Ensure you're using Python 3.10 or higher:

```bash
python --version
```

### Check Installation Location

To see where fastestmcp is installed:

```bash
which fastestmcp  # On Unix/Linux/macOS
where fastestmcp  # On Windows
```

## Usage Examples

Once installed, you can use FastestMCP from anywhere:

```bash
# Generate a basic MCP server
fastestmcp server --level 1 --name myapp --transport stdio --structure mono

# Generate a weather API server
fastestmcp server --template weather --name myweather --structure structured

# Generate an OpenAPI server
fastestmcp server --type openapi --name api-server --transport http

# Generate a client
fastestmcp client --name myclient --apis 3 --integrations 2 --transport http --structure structured

# Get help
fastestmcp --help
```

## Updating

### Update with pipx

```bash
pipx upgrade fastestmcp
```

### Update with pip

```bash
pip install --user --upgrade fastestmcp
```

### Update from Repository

```bash
cd path/to/fastestmcp-repo
git pull
```

## Uninstalling

### Uninstall with pipx

```bash
pipx uninstall fastestmcp
```

### Uninstall with pip

```bash
pip uninstall fastestmcp
```

## Support

If you encounter issues:

1. Check that you're using Python 3.10+
2. Verify your PATH includes the correct directories
3. Try restarting your terminal
4. Check the [GitHub repository](https://github.com/orchestrate-solutions/fastestmcp) for latest updates

For additional help, visit the [FastestMCP documentation](https://github.com/orchestrate-solutions/fastestmcp/tree/main/docs).</content>
<filePath>c:\Users\19032917\Documents\github\fastestmcp-test\fastestmcp-repo\docs\developer\global-installation-guide.md
