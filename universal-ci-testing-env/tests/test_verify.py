import pytest
import json
import os
import tempfile
from unittest.mock import patch, MagicMock
from verify import load_config, run_task, Task, get_config_path

class TestConfigPathResolution:
    def test_config_in_current_directory(self, tmp_path, monkeypatch):
        # Change to temp directory
        monkeypatch.chdir(tmp_path)
        
        # Create config file
        config_data = {"tasks": [{"name": "Test", "working_directory": ".", "command": "echo test"}]}
        config_file = tmp_path / "universal-ci.config.json"
        config_file.write_text(json.dumps(config_data))
        
        # Test resolution
        path = get_config_path()
        assert os.path.exists(path)
        assert "universal-ci.config.json" in path
    
    def test_config_with_explicit_path(self, tmp_path):
        config_data = {"tasks": []}
        config_file = tmp_path / "custom-config.json"
        config_file.write_text(json.dumps(config_data))
        
        path = get_config_path(str(config_file))
        assert path == str(config_file)

class TestLoadConfig:
    def test_load_valid_config(self):
        # Create a temporary config file
        config_data = {
            "tasks": [
                {
                    "name": "Test Task",
                    "working_directory": ".",
                    "command": "echo test"
                }
            ]
        }
        
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            json.dump(config_data, f)
            config_path = f.name
        
        try:
            tasks = load_config(config_path)
            assert len(tasks) == 1
            assert tasks[0].name == "Test Task"
            assert tasks[0].working_directory == "."
            assert tasks[0].command == "echo test"
        finally:
            os.unlink(config_path)
    
    def test_load_missing_config(self):
        with pytest.raises(SystemExit):
            load_config("nonexistent.json")

class TestRunTask:
    @patch('subprocess.run')
    def test_run_task_success(self, mock_run):
        mock_run.return_value.returncode = 0
        
        task = Task("Success Task", ".", "echo success")
        result = run_task(task)
        
        assert result is True
        mock_run.assert_called_once()
    
    @patch('subprocess.run')
    def test_run_task_failure(self, mock_run):
        mock_run.return_value.returncode = 1
        
        task = Task("Fail Task", ".", "exit 1")
        result = run_task(task)
        
        assert result is False
        mock_run.assert_called_once()
    
    @patch('os.path.exists')
    def test_run_task_missing_directory(self, mock_exists):
        mock_exists.return_value = False
        
        task = Task("Missing Dir Task", "nonexistent", "echo test")
        result = run_task(task)
        
        assert result is True  # Should skip gracefully
        mock_exists.assert_called_once_with("nonexistent")

class TestIntegration:
    def test_full_verification_with_test_config(self):
        # This would require setting up a test environment
        # For now, just test that the script can be imported
        import verify
        assert hasattr(verify, 'main')
        assert hasattr(verify, 'load_config')
        assert hasattr(verify, 'run_task')

    def test_github_actions_scenario(self, tmp_path, monkeypatch):
        """Test scenario where verify.py is run from GitHub Actions checkout"""
        # Create a mock repository structure
        repo_root = tmp_path / "repo"
        repo_root.mkdir()
        
        # Create config at repo root
        config_data = {
            "tasks": [
                {
                    "name": "Echo Test",
                    "working_directory": ".",
                    "command": "echo 'GitHub Actions test'"
                }
            ]
        }
        config_file = repo_root / "universal-ci.config.json"
        config_file.write_text(json.dumps(config_data))
        
        # Create the universal-ci-testing-env subdirectory
        ci_env_dir = repo_root / "universal-ci-testing-env"
        ci_env_dir.mkdir()
        
        # Change to the ci_env directory to simulate running from there
        monkeypatch.chdir(ci_env_dir)
        
        # Now verify.py should find the config
        path = get_config_path()
        assert os.path.exists(path)
        assert "universal-ci.config.json" in path