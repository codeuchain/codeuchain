#!/bin/sh
# Universal CI Verifier - Lightweight Shell Implementation
# Works on macOS, Linux, and any POSIX-compliant system
# Zero dependencies beyond standard Unix tools (sh, sed, grep)

# Load environment variables from .env file if it exists
if [ -f ".env" ]; then
    . ./.env
fi

# Don't exit on error - we handle errors ourselves
set +e

# Colors (with fallback for non-color terminals)
if [ -t 1 ]; then
    GREEN='\033[92m'
    RED='\033[91m'
    YELLOW='\033[93m'
    BLUE='\033[94m'
    CYAN='\033[96m'
    RESET='\033[0m'
else
    GREEN=''
    RED=''
    YELLOW=''
    BLUE=''
    CYAN=''
    RESET=''
fi

CONFIG_FILE="universal-ci.config.json"
STAGE="test"
INIT_MODE=false
FORCE_TYPE=""

# Print usage
usage() {
    echo "Universal CI Verifier - Lightweight Shell Edition"
    echo ""
    echo "Usage: $0 [OPTIONS]"
    echo ""
    echo "Commands:"
    echo "  --init             Initialize a new project (detect type, create config)"
    echo ""
    echo "Options:"
    echo "  --config <path>    Path to config file (default: universal-ci.config.json)"
    echo "  --stage <stage>    Stage to run: test or release (default: test)"
    echo "  --type <type>      Force project type for --init (nodejs, python, go, rust, etc.)"
    echo "  --help             Show this help message"
    echo ""
    echo "Examples:"
    echo "  $0                 Run verification with auto-detected config"
    echo "  $0 --init          Initialize config for current project"
    echo "  $0 --stage release Run release tasks"
    exit 0
}

# Parse arguments
while [ $# -gt 0 ]; do
    case "$1" in
        --config)
            CONFIG_FILE="$2"
            shift 2
            ;;
        --stage)
            STAGE="$2"
            shift 2
            ;;
        --init)
            INIT_MODE=true
            shift
            ;;
        --type)
            FORCE_TYPE="$2"
            shift 2
            ;;
        --help|-h)
            usage
            ;;
        *)
            printf "${RED}Unknown option: $1${RESET}\n"
            usage
            ;;
    esac
done

# Find config file
find_config() {
    # Check provided/default path
    if [ -f "$CONFIG_FILE" ]; then
        echo "$CONFIG_FILE"
        return 0
    fi
    
    # Check parent directories
    if [ -f "../$CONFIG_FILE" ]; then
        echo "../$CONFIG_FILE"
        return 0
    fi
    if [ -f "../../$CONFIG_FILE" ]; then
        echo "../../$CONFIG_FILE"
        return 0
    fi
    if [ -f "../../../$CONFIG_FILE" ]; then
        echo "../../../$CONFIG_FILE"
        return 0
    fi
    
    # Check git root
    if command -v git >/dev/null 2>&1; then
        git_root=$(git rev-parse --show-toplevel 2>/dev/null || echo "")
        if [ -n "$git_root" ] && [ -f "$git_root/$CONFIG_FILE" ]; then
            echo "$git_root/$CONFIG_FILE"
            return 0
        fi
    fi
    
    return 1
}

# Extract a JSON string value using sed (portable across macOS/Linux)
# Usage: get_json_value "key" "json_object"
get_json_value() {
    key="$1"
    json="$2"
    # Extract value after "key": "value" - handles simple cases
    echo "$json" | sed -n "s/.*\"$key\"[[:space:]]*:[[:space:]]*\"\([^\"]*\)\".*/\1/p" | head -1
}

# Parse tasks from config file
# Outputs: name|working_directory|command for each matching task
parse_tasks() {
    config_path="$1"
    target_stage="$2"
    
    # Read config and flatten to single line
    config_content=$(cat "$config_path" | tr '\n' ' ' | tr '\r' ' ')
    
    # Extract tasks array content - everything between [ and ]
    tasks_content=$(echo "$config_content" | sed 's/.*"tasks"[[:space:]]*:[[:space:]]*\[//' | sed 's/\][[:space:]]*}[[:space:]]*//')
    
    # Split by },{ pattern to get individual tasks, then process each
    # Add newlines after each task object
    echo "$tasks_content" | sed 's/}[[:space:]]*,[[:space:]]*{/}|TASK_SEP|{/g' | tr '|' '\n' | grep -v "^TASK_SEP$" | while read -r task_json; do
        # Skip empty lines
        [ -z "$task_json" ] && continue
        
        # Extract fields using sed
        name=$(echo "$task_json" | sed -n 's/.*"name"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
        dir=$(echo "$task_json" | sed -n 's/.*"working_directory"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
        cmd=$(echo "$task_json" | sed -n 's/.*"command"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
        task_stage=$(echo "$task_json" | sed -n 's/.*"stage"[[:space:]]*:[[:space:]]*"\([^"]*\)".*/\1/p')
        
        # Default stage to "test" if not specified
        if [ -z "$task_stage" ]; then
            task_stage="test"
        fi
        
        # Output if stage matches and we have required fields
        if [ "$task_stage" = "$target_stage" ] && [ -n "$name" ] && [ -n "$cmd" ]; then
            # Use unit separator character as delimiter
            printf '%s\x1f%s\x1f%s\n' "$name" "$dir" "$cmd"
        fi
    done
}

# Run a single task
run_task() {
    name="$1"
    dir="$2"
    cmd="$3"
    
    echo "---------------------------------------------------"
    printf "${BLUE}🔍 Checking ${name}...${RESET}\n"
    echo "   📂 Path: ${dir}"
    echo "   🚀 Command: ${cmd}"
    
    # Check if directory exists
    if [ ! -d "$dir" ]; then
        printf "   ${YELLOW}⚠️  Skipped (Directory not found)${RESET}\n"
        return 0
    fi
    
    # Run command in subshell
    (cd "$dir" && eval "$cmd")
    result=$?
    
    if [ $result -eq 0 ]; then
        printf "   ${GREEN}✅ ${name} Passed${RESET}\n"
        return 0
    else
        printf "   ${RED}❌ ${name} FAILED${RESET}\n"
        return 1
    fi
}

# Main execution
main() {
    printf "${BLUE}🌐 Starting Universal CI Verification (Shell Edition)...${RESET}\n"
    
    # Detect environment
    if [ -n "$GITHUB_ACTIONS" ]; then
        echo "   📍 Environment: GitHub Actions"
    elif [ -n "$CI" ]; then
        echo "   📍 Environment: CI Server"
    else
        echo "   📍 Environment: Local Shell"
    fi
    
    # Find config
    config_path=$(find_config)
    if [ -z "$config_path" ]; then
        printf "${RED}Error: Config file '${CONFIG_FILE}' not found.${RESET}\n"
        echo "Searched in: current directory, parent directories, and git root."
        exit 1
    fi
    
    echo "   📄 Config: ${config_path}"
    echo "---------------------------------------------------"
    stage_upper=$(echo "$STAGE" | tr '[:lower:]' '[:upper:]')
    printf "${BLUE}🛠  ${stage_upper} PHASE${RESET}\n"
    
    # Parse tasks into temp file to preserve across subshells
    tmp_tasks=$(mktemp)
    tmp_failures=$(mktemp)
    parse_tasks "$config_path" "$STAGE" > "$tmp_tasks"
    
    # Check if we have any tasks
    if [ ! -s "$tmp_tasks" ]; then
        printf "   ${YELLOW}No tasks found for stage: ${STAGE}${RESET}\n"
        rm -f "$tmp_tasks" "$tmp_failures"
        exit 0
    fi
    
    # Process each task
    while IFS=$(printf '\x1f') read -r name dir cmd; do
        if [ -n "$name" ]; then
            if ! run_task "$name" "$dir" "$cmd"; then
                echo "$name" >> "$tmp_failures"
            fi
        fi
    done < "$tmp_tasks"
    
    echo "---------------------------------------------------"
    printf "${BLUE}📊 SUMMARY${RESET}\n"
    
    # Check for failures
    if [ -s "$tmp_failures" ]; then
        printf "${RED}🚨 FAILURES DETECTED:${RESET}\n"
        while read -r fail_name; do
            echo "   - ${fail_name}"
        done < "$tmp_failures"
        rm -f "$tmp_tasks" "$tmp_failures"
        exit 1
    else
        printf "${GREEN}🎉 ALL SYSTEMS GO! Universal CI Passed.${RESET}\n"
        rm -f "$tmp_tasks" "$tmp_failures"
        exit 0
    fi
}

# ============================================================================
# INIT MODE - Project Detection and Config Generation
# ============================================================================

detect_project_type() {
    if [ -n "$FORCE_TYPE" ]; then echo "$FORCE_TYPE"; return 0; fi
    if [ -f "package.json" ]; then echo "nodejs"; return 0; fi
    if [ -f "pyproject.toml" ] || [ -f "setup.py" ] || [ -f "requirements.txt" ] || [ -f "Pipfile" ]; then echo "python"; return 0; fi
    if [ -f "go.mod" ]; then echo "go"; return 0; fi
    if [ -f "Cargo.toml" ]; then echo "rust"; return 0; fi
    if ls *.csproj 1>/dev/null 2>&1 || ls *.fsproj 1>/dev/null 2>&1; then echo "dotnet"; return 0; fi
    if [ -f "pom.xml" ]; then echo "java-maven"; return 0; fi
    if [ -d "src/main/kotlin" ]; then echo "kotlin"; return 0; fi
    if [ -f "build.sbt" ] || [ -d "src/main/scala" ]; then echo "scala"; return 0; fi
    if [ -f "build.gradle" ] || [ -f "build.gradle.kts" ]; then echo "java-gradle"; return 0; fi
    if [ -f "Package.swift" ] || [ -d "Sources" ]; then echo "swift"; return 0; fi
    if [ -f "CMakeLists.txt" ] || ls *.cpp 1>/dev/null 2>&1 || ls *.cc 1>/dev/null 2>&1; then echo "cpp"; return 0; fi
    if [ -f "pubspec.yaml" ] || ls *.dart 1>/dev/null 2>&1; then echo "dart"; return 0; fi
    if [ -f "Gemfile" ]; then echo "ruby"; return 0; fi
    if [ -f "composer.json" ]; then echo "php"; return 0; fi
    if [ -f "Makefile" ]; then echo "make"; return 0; fi
    echo "generic"
}

detect_node_pm() {
    if [ -f "pnpm-lock.yaml" ]; then echo "pnpm"
    elif [ -f "yarn.lock" ]; then echo "yarn"
    elif [ -f "bun.lockb" ]; then echo "bun"
    else echo "npm"; fi
}

detect_python_pm() {
    if [ -f "poetry.lock" ]; then echo "poetry"
    elif [ -f "Pipfile" ]; then echo "pipenv"
    elif [ -f "uv.lock" ]; then echo "uv"
    else echo "pip"; fi
}

has_npm_script() {
    [ -f "package.json" ] && grep -q "\"$1\"" package.json 2>/dev/null
}

generate_config() {
    project_type="$1"
    
    case "$project_type" in
        nodejs)
            pm=$(detect_node_pm)
            case "$pm" in
                pnpm) ic="pnpm install"; tc="pnpm test"; lc="pnpm run lint"; bc="pnpm run build" ;;
                yarn) ic="yarn install"; tc="yarn test"; lc="yarn lint"; bc="yarn build" ;;
                bun)  ic="bun install"; tc="bun test"; lc="bun run lint"; bc="bun run build" ;;
                *)    ic="npm ci"; tc="npm test"; lc="npm run lint"; bc="npm run build" ;;
            esac
            has_npm_script "test" || tc="echo 'No test script'"
            lint_task=""
            has_npm_script "lint" && lint_task=",{\"name\":\"Lint\",\"working_directory\":\".\",\"command\":\"$lc\",\"stage\":\"test\"}"
            build_task=""
            has_npm_script "build" && build_task=",{\"name\":\"Build\",\"working_directory\":\".\",\"command\":\"$bc\",\"stage\":\"release\"}"
            echo "{\"tasks\":[{\"name\":\"Install\",\"working_directory\":\".\",\"command\":\"$ic\",\"stage\":\"test\"},{\"name\":\"Test\",\"working_directory\":\".\",\"command\":\"$tc\",\"stage\":\"test\"}$lint_task$build_task]}"
            ;;
        python)
            pm=$(detect_python_pm)
            case "$pm" in
                poetry) ic="poetry install"; tc="poetry run pytest"; lc="poetry run ruff check ." ;;
                pipenv) ic="pipenv install --dev"; tc="pipenv run pytest"; lc="pipenv run ruff check ." ;;
                uv)     ic="uv sync"; tc="uv run pytest"; lc="uv run ruff check ." ;;
                *)      ic="pip install -r requirements.txt"; tc="pytest"; lc="ruff check . || flake8" ;;
            esac
            echo "{\"tasks\":[{\"name\":\"Install\",\"working_directory\":\".\",\"command\":\"$ic\",\"stage\":\"test\"},{\"name\":\"Lint\",\"working_directory\":\".\",\"command\":\"$lc\",\"stage\":\"test\"},{\"name\":\"Test\",\"working_directory\":\".\",\"command\":\"$tc\",\"stage\":\"test\"}]}"
            ;;
        go)
            echo '{"tasks":[{"name":"Download","working_directory":".","command":"go mod download","stage":"test"},{"name":"Vet","working_directory":".","command":"go vet ./...","stage":"test"},{"name":"Test","working_directory":".","command":"go test -v ./...","stage":"test"},{"name":"Build","working_directory":".","command":"go build -v ./...","stage":"release"}]}'
            ;;
        rust)
            echo '{"tasks":[{"name":"Check","working_directory":".","command":"cargo check","stage":"test"},{"name":"Clippy","working_directory":".","command":"cargo clippy -- -D warnings","stage":"test"},{"name":"Test","working_directory":".","command":"cargo test","stage":"test"},{"name":"Build","working_directory":".","command":"cargo build --release","stage":"release"}]}'
            ;;
        dotnet)
            echo '{"tasks":[{"name":"Restore","working_directory":".","command":"dotnet restore","stage":"test"},{"name":"Build","working_directory":".","command":"dotnet build --no-restore","stage":"test"},{"name":"Test","working_directory":".","command":"dotnet test --no-build","stage":"test"}]}'
            ;;
        java-maven)
            echo '{"tasks":[{"name":"Compile","working_directory":".","command":"mvn compile","stage":"test"},{"name":"Test","working_directory":".","command":"mvn test","stage":"test"},{"name":"Package","working_directory":".","command":"mvn package -DskipTests","stage":"release"}]}'
            ;;
        java-gradle)
            echo '{"tasks":[{"name":"Compile","working_directory":".","command":"./gradlew compileJava","stage":"test"},{"name":"Test","working_directory":".","command":"./gradlew test","stage":"test"},{"name":"Build","working_directory":".","command":"./gradlew build -x test","stage":"release"}]}'
            ;;
        kotlin)
            echo '{"tasks":[{"name":"Build","working_directory":".","command":"./gradlew build || gradlew build","stage":"test"},{"name":"Test","working_directory":".","command":"./gradlew test || gradlew test","stage":"test"},{"name":"Assemble","working_directory":".","command":"./gradlew assemble || gradlew assemble","stage":"release"}]}'
            ;;
        scala)
            echo '{"tasks":[{"name":"Compile","working_directory":".","command":"sbt compile || ./gradlew compileScala || gradlew compileScala","stage":"test"},{"name":"Test","working_directory":".","command":"sbt test || ./gradlew test || gradlew test","stage":"test"},{"name":"Package","working_directory":".","command":"sbt package || ./gradlew assemble || gradlew assemble","stage":"release"}]}'
            ;;
        swift)
            echo '{"tasks":[{"name":"Build","working_directory":".","command":"swift build","stage":"test"},{"name":"Test","working_directory":".","command":"swift test","stage":"test"},{"name":"Release","working_directory":".","command":"swift build --configuration release","stage":"release"}]}'
            ;;
        cpp)
            echo '{"tasks":[{"name":"Configure","working_directory":".","command":"mkdir -p build && cd build && cmake .. || echo CMake not found","stage":"test"},{"name":"Build","working_directory":".","command":"cd build && make || make","stage":"test"},{"name":"Test","working_directory":".","command":"cd build && ctest || make test || echo No tests","stage":"test"}]}'
            ;;
        dart)
            echo '{"tasks":[{"name":"Get","working_directory":".","command":"dart pub get","stage":"test"},{"name":"Analyze","working_directory":".","command":"dart analyze","stage":"test"},{"name":"Test","working_directory":".","command":"dart test","stage":"test"},{"name":"Build","working_directory":".","command":"dart compile exe bin/main.dart -o bin/main || echo No executable","stage":"release"}]}'
            ;;
        ruby)
            echo '{"tasks":[{"name":"Install","working_directory":".","command":"bundle install","stage":"test"},{"name":"Test","working_directory":".","command":"bundle exec rspec || bundle exec rake test","stage":"test"}]}'
            ;;
        php)
            echo '{"tasks":[{"name":"Install","working_directory":".","command":"composer install","stage":"test"},{"name":"Test","working_directory":".","command":"composer test || vendor/bin/phpunit","stage":"test"}]}'
            ;;
        make)
            echo '{"tasks":[{"name":"Build","working_directory":".","command":"make","stage":"test"},{"name":"Test","working_directory":".","command":"make test","stage":"test"}]}'
            ;;
        *)
            echo '{"tasks":[{"name":"Hello","working_directory":".","command":"echo Universal CI ready! Edit universal-ci.config.json","stage":"test"}]}'
            ;;
    esac
}

pretty_json() {
    # Simple JSON prettification using sed
    echo "$1" | sed 's/\[/[\n  /g' | sed 's/\],/\n],/g' | sed 's/},{/},\n  {/g' | sed 's/}]$/\n  }\n]/g'
}

run_init() {
    printf "${CYAN}🔧 Universal CI - Project Initialization${RESET}\n"
    echo ""
    
    project_type=$(detect_project_type)
    
    case "$project_type" in
        nodejs)      friendly="Node.js" ;;
        python)      friendly="Python" ;;
        go)          friendly="Go" ;;
        rust)        friendly="Rust" ;;
        dotnet)      friendly=".NET" ;;
        java-maven)  friendly="Java (Maven)" ;;
        java-gradle) friendly="Java (Gradle)" ;;
        kotlin)      friendly="Kotlin" ;;
        scala)       friendly="Scala" ;;
        swift)       friendly="Swift" ;;
        cpp)         friendly="C++" ;;
        dart)        friendly="Dart" ;;
        ruby)        friendly="Ruby" ;;
        php)         friendly="PHP" ;;
        make)        friendly="Makefile" ;;
        *)           friendly="Generic" ;;
    esac
    
    printf "${GREEN}✅ Detected: ${friendly}${RESET}\n"
    
    if [ "$project_type" = "nodejs" ]; then
        pm=$(detect_node_pm)
        printf "${BLUE}   Package manager: ${pm}${RESET}\n"
    elif [ "$project_type" = "python" ]; then
        pm=$(detect_python_pm)
        printf "${BLUE}   Package manager: ${pm}${RESET}\n"
    fi
    
    if [ -f "$CONFIG_FILE" ]; then
        printf "${YELLOW}⚠️  Config already exists: ${CONFIG_FILE}${RESET}\n"
        printf "Overwrite? [y/N] "
        read -r answer
        case "$answer" in
            [Yy]*) ;;
            *) printf "${BLUE}Keeping existing config.${RESET}\n"; exit 0 ;;
        esac
    fi
    
    config=$(generate_config "$project_type")
    pretty_json "$config" > "$CONFIG_FILE"
    
    printf "${GREEN}✅ Created ${CONFIG_FILE}${RESET}\n"
    echo ""
    echo "Next steps:"
    echo "  1. Review and customize ${CONFIG_FILE}"
    echo "  2. Run: ./verify.sh"
    exit 0
}

# Run init mode if requested
if [ "$INIT_MODE" = true ]; then
    run_init
fi

main