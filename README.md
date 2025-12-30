# Goal-Pattern
Scans, Analyses, Schedules and Automatically Completes Work

A Prolog-based daemon service that runs continuously to scan, plan, and report on work patterns and goals.

## Features

- **Always-on daemon**: Runs continuously with configurable tick intervals
- **Persistent state**: Automatically saves state to disk
- **Structured logging**: Comprehensive logging with multiple levels
- **Modular architecture**: Plugin system for future integrations
- **Clean shutdown**: Graceful shutdown with state preservation
- **Unified data model**: Goals, work items, schedule events, and time blocks with validation
- **Work tracking**: Track algorithms (Prolog clauses) and philosophies (essays) with status transitions
- **Schedule integration**: Support for calendar events with context tags (home, travel, sauna, etc.)
- **Safety margin**: +10% safety margin for LLM-generated work to ensure accuracy

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

## Data Model

The system uses a unified data model to track goals, work artifacts, schedule events, and time blocks.

### Goals

Goals define targets for work completion (algorithms or philosophies):

```prolog
% Create a weekly algorithm goal
create_goal(
    goal1,                          % ID
    algorithms,                     % Type: algorithms or philosophies
    140,                            % Target count
    _{period: weekly},             % Time window
    strict,                         % Strictness: strict, adaptive, or flexible
    _{description: "Weekly target"}, % Metadata
    Goal
).

% Add goal to state
add_goal(Goal).
```

### Work Items

Work items track actual work completed with status transitions:

```prolog
% Create a work item
create_work_item(
    work1,                          % ID
    algorithm_clause,               % Type: algorithm_clause, philosophy_essay, draft, spec, note
    '/path/to/file.pl',            % Origin (filepath or source)
    complete,                       % Status: draft → partial → complete → submitted
    15,                             % Count (clauses for algorithms, words for essays)
    _{created: "2025-01-01T10:00:00", modified: "2025-01-01T12:00:00"}, % Timestamps
    [home, work],                   % Context tags
    0.95,                           % Confidence (0.0-1.0)
    WorkItem
).

% Add to state
add_work_item(WorkItem).

% Calculate score (with optional 10% safety margin for LLM work)
work_item_score(WorkItem, true, Score).  % Score = count × confidence × 0.9
```

### Schedule Events

Schedule events track calendar items with context:

```prolog
% Create a schedule event
create_schedule_event(
    event1,                         % ID
    "2025-01-01T09:00:00",         % Start time (ISO 8601)
    "2025-01-01T10:00:00",         % End time
    "Morning workout",              % Title
    "Home",                         % Location
    [home, exercise],               % Tags (home, friend_house, sauna, seminar, travel, etc.)
    calendar_ics,                   % Source
    0.95,                           % Attendance confidence (0.0-1.0)
    Event
).

add_schedule_event(Event).
```

### Time Blocks

Time blocks categorize time by activity type and impact:

```prolog
% Create a time block
create_time_block(
    "2025-01-01T14:00:00",         % Start
    "2025-01-01T16:00:00",         % End
    category(work),                 % Category: work, rest, play, or travel
    0.7,                            % Fatigue cost
    0.3,                            % Recovery cost
    0.9,                            % Confidence
    Block
).

add_time_block(Block).
```

### Status Transitions

Work items follow defined status transitions:
- `draft` → `partial` → `complete` → `submitted`
- Direct `draft` → `complete` is allowed

### Safety Margin

The +10% safety margin rule ensures accuracy when counting LLM-generated work:
- If an LLM suggests 100 units completed, the system counts 90 units
- This prevents over-counting potentially incorrect or incomplete work

```prolog
apply_safety_margin(100, Adjusted).  % Adjusted = 90
```

## Architecture

### Module Structure

```
src/
├── daemon.pl          # Main daemon loop (daemon_start, daemon_stop, daemon_tick)
├── config.pl          # Configuration loading and validation
├── state.pl           # Persistent state management with entity serialization
├── log.pl             # Structured logging
├── api.pl             # Internal API (scan, plan, report)
├── model.pl           # Unified data model (goals, work items, events, blocks)
├── validate.pl        # Validation predicates for model integrity
└── modules/           # Plugin modules directory

tests/
├── run_tests.pl       # Test runner
├── test_daemon.pl     # Daemon tests
└── test_model.pl      # Data model tests

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
- `add_goal/1`: Add a goal to state
- `add_work_item/1`: Add a work item to state
- `add_schedule_event/1`: Add a schedule event to state
- `add_time_block/1`: Add a time block to state
- `get_goals/1`: Retrieve all goals from state
- `get_work_items/1`: Retrieve all work items from state
- `get_schedule_events/1`: Retrieve all schedule events from state
- `get_time_blocks/1`: Retrieve all time blocks from state

#### Logging
- `log_info/1`: Log informational message
- `log_warn/1`: Log warning message
- `log_error/1`: Log error message
- `log_debug/1`: Log debug message

#### API (Stubs)
- `source_scan/2`: Scan a source for items
- `planner_recommendations/2`: Generate planning recommendations
- `generate_report/1`: Generate a report

#### Data Model (Spec 2)
- `goal/6`: Goal structure (id, type, target_count, time_window, strictness, metadata)
- `work_item/8`: Work item structure (id, type, origin, status, count, timestamps, tags, confidence)
- `schedule_event/8`: Schedule event structure (id, start, end, title, location, tags, source, attendance_confidence)
- `time_block/6`: Time block structure (start, end, category, fatigue_cost, recovery_cost, confidence)
- `create_goal/7`: Create a new goal
- `create_work_item/9`: Create a new work item
- `create_schedule_event/9`: Create a new schedule event
- `create_time_block/7`: Create a new time block
- `work_item_score/3`: Calculate work item score with optional safety margin
- `apply_safety_margin/2`: Apply +10% safety margin to counts

#### Validation
- `validate_goal/1`: Validate goal structure
- `validate_work_item/1`: Validate work item structure
- `validate_schedule_event/1`: Validate schedule event structure
- `validate_time_block/1`: Validate time block structure
- `validate_timestamp/1`: Validate timestamp format (ISO 8601 or dict)
- `validate_time_range/2`: Validate time range (start ≤ end)
- `validate_nonnegative/1`: Validate non-negative numbers
- `validate_probability/1`: Validate probability values (0.0-1.0)

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
