# Goal-Pattern
Scans, Analyses, Scheduled and Automatically Completes Work

A Prolog-based daemon service that runs continuously to scan, plan, and report on work patterns and goals.

## Features

- **Always-on daemon**: Runs continuously with configurable tick intervals
- **Persistent state**: Automatically saves state to disk
- **Structured logging**: Comprehensive logging with multiple levels
- **Modular architecture**: Plugin system for future integrations
- **Clean shutdown**: Graceful shutdown with state preservation

## Requirements

- SWI-Prolog 9.0.4 or higher

## Installation

1. Install SWI-Prolog:
```bash
sudo apt-get install swi-prolog
```

2. Clone the repository:
```bash
git clone https://github.com/luciangreen/Goal-Pattern.git
cd Goal-Pattern
```

## Usage

### Starting the Daemon

Run the daemon using the CLI script:
```bash
bin/lucian_planner
```

The daemon will:
- Load configuration from `config/config.json`
- Initialize logging to `logs/daemon.log`
- Start the main loop with periodic ticks
- Log tick events every 60 seconds (configurable)
- Save state periodically

### Stopping the Daemon

To stop the daemon cleanly:
1. Press `Ctrl+C` to interrupt
2. Or call `daemon_stop/0` from the Prolog prompt

The daemon will automatically save state before exiting.

### Running Tests

Run the test suite:
```bash
swipl -q -s tests/run_tests.pl -g run_tests
```

## Configuration

Edit `config/config.json` to customize daemon behavior:

```json
{
  "daemon": {
    "tick_interval_seconds": 60,
    "state_save_interval_ticks": 5,
    "log_level": "info"
  },
  "state": {
    "file_path": "data/state.json"
  },
  "log": {
    "file_path": "logs/daemon.log",
    "console_output": true
  },
  "modules": {
    "enabled": []
  }
}
```

### Configuration Options

- `tick_interval_seconds`: How often the daemon runs a tick (default: 60)
- `state_save_interval_ticks`: How many ticks between state saves (default: 5)
- `log_level`: Logging level - `debug`, `info`, `warn`, or `error` (default: `info`)
- `state.file_path`: Where to save persistent state (default: `data/state.json`)
- `log.file_path`: Where to save log files (default: `logs/daemon.log`)
- `log.console_output`: Whether to also log to console (default: `true`)
- `modules.enabled`: List of enabled plugin modules (default: `[]`)

## Architecture

### Module Structure

```
src/
├── daemon.pl          # Main daemon loop (daemon_start, daemon_stop, daemon_tick)
├── config.pl          # Configuration loading and validation
├── state.pl           # Persistent state management
├── log.pl             # Structured logging
├── api.pl             # Internal API (scan, plan, report)
└── modules/           # Plugin modules directory

tests/
├── run_tests.pl       # Test runner
└── test_daemon.pl     # Daemon tests

bin/
└── lucian_planner     # CLI entry point

config/
└── config.json        # Configuration file
```

### Key Functions

#### Daemon Control
- `daemon_start/0`: Start the daemon loop
- `daemon_stop/0`: Stop the daemon and save state
- `daemon_tick/0`: Execute one tick of the main loop

#### Configuration
- `load_config/1`: Load configuration from file
- `get_config/2`: Get configuration value by path
- `validate_config/1`: Validate configuration structure

#### State Management
- `init_state/0`: Initialize or load state
- `save_state/0`: Save current state to disk
- `get_state/2`: Get state value
- `set_state/2`: Set state value

#### Logging
- `log_info/1`: Log informational message
- `log_warn/1`: Log warning message
- `log_error/1`: Log error message
- `log_debug/1`: Log debug message

#### API (Stubs)
- `source_scan/2`: Scan a source for items
- `planner_recommendations/2`: Generate planning recommendations
- `generate_report/1`: Generate a report

## Development

### Adding New Modules

1. Create a new module in `src/modules/`
2. Implement the standard interface predicates
3. Add module to `config.json` enabled list
4. Module will be loaded automatically on daemon start

### Adding Tests

1. Create test file in `tests/`
2. Add to `tests/run_tests.pl`
3. Run with `swipl -q -s tests/run_tests.pl -g run_tests`

## License

See LICENSE file for details.
