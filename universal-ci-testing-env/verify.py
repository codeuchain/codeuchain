import json
import subprocess
import sys
import os
from dataclasses import dataclass
from typing import List
import argparse

# Colors for output
GREEN = '\033[92m'
RED = '\033[91m'
YELLOW = '\033[93m'
RESET = '\033[0m'

CONFIG_FILE = "universal-ci.config.json"

def get_config_path(provided_path: str = None) -> str:
    """
    Resolve config file path with multiple fallbacks:
    1. Provided path (--config flag)
    2. Current directory
    3. Root of repository (if running from subdirectory in GitHub Actions)
    4. Directory where this script is located
    """
    if provided_path:
        return provided_path
    
    # Check current directory first
    if os.path.exists(CONFIG_FILE):
        return CONFIG_FILE
    
    # Check parent directories (up to 3 levels for GitHub Actions)
    for i in range(3):
        parent_path = os.path.join("..", "*" * i, CONFIG_FILE)
        normalized_path = os.path.normpath(parent_path)
        if os.path.exists(normalized_path):
            return normalized_path
    
    # Check root of git repository
    try:
        result = subprocess.run(
            ["git", "rev-parse", "--show-toplevel"],
            capture_output=True,
            text=True
        )
        if result.returncode == 0:
            git_root = result.stdout.strip()
            repo_config = os.path.join(git_root, CONFIG_FILE)
            if os.path.exists(repo_config):
                return repo_config
    except Exception:
        pass
    
    # Default fallback
    return CONFIG_FILE

@dataclass
class Task:
    name: str
    working_directory: str
    command: str

def load_config(config_path: str = None) -> List[Task]:
    # Resolve the actual config path
    actual_path = get_config_path(config_path)
    
    if not os.path.exists(actual_path):
        print(f"{RED}Error: Config file '{actual_path}' not found.{RESET}")
        print(f"Searched in: {CONFIG_FILE}, parent directories, and git root.")
        print(f"Please create {CONFIG_FILE} in the root directory.")
        sys.exit(1)
        
    with open(config_path, 'r') as f:
        data = json.load(f)
        
    tasks = []
    for t in data.get("tasks", []):
        tasks.append(Task(
            name=t["name"],
            working_directory=t["working_directory"],
            command=t["command"]
        ))
    return tasks

def run_task(task: Task) -> bool:
    print(f"---------------------------------------------------")
    print(f"🔍 Checking {task.name}...")
    print(f"   📂 Path: {task.working_directory}")
    print(f"   🚀 Command: {task.command}")
    
    if not os.path.exists(task.working_directory):
        print(f"   {YELLOW}⚠️  Skipped (Directory not found){RESET}")
        return True
    
    # Run command
    try:
        # We need shell=True to handle && chaining
        result = subprocess.run(
            task.command, 
            cwd=task.working_directory, 
            shell=True, 
            stdout=sys.stdout,
            stderr=sys.stderr
        )
        
        if result.returncode == 0:
            print(f"   {GREEN}✅ {task.name} Passed{RESET}")
            return True
        else:
            print(f"   {RED}❌ {task.name} FAILED{RESET}")
            return False
            
    except Exception as e:
        print(f"   {RED}❌ Execution Error: {e}{RESET}")
        return False

def main():
    parser = argparse.ArgumentParser(description='Universal CI Verifier')
    parser.add_argument('--config', default=CONFIG_FILE, help='Path to config file')
    args = parser.parse_args()
    
    print("🌐 Starting Universal CI Verification (Config-Driven)...")
    
    # Determine environment
    if os.environ.get("GITHUB_ACTIONS"):
        print("   📍 Environment: GitHub Actions / Act")
    else:
        print("   📍 Environment: Local Shell")
        
    tasks = load_config(args.config)
    failures = []
    
    print("---------------------------------------------------")
    print("🛠  BUILD & TEST PHASE")
    
    for task in tasks:
        success = run_task(task)
        if not success:
            failures.append(task.name)

    print("---------------------------------------------------")
    print("📊 SUMMARY")
    
    if not failures:
        print(f"{GREEN}🎉 ALL SYSTEMS GO! Universal CI Passed.{RESET}")
        sys.exit(0)
    else:
        print(f"{RED}🚨 FAILURES DETECTED:{RESET}")
        for fail in failures:
            print(f"   - {fail}")
        sys.exit(1)

if __name__ == "__main__":
    main()
