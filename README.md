# Goal-Pattern
Scans, Analyses, Schedules and Automatically Completes Work

A Prolog-based daemon service that runs continuously to scan, plan, and report on work patterns and goals.

## Features

- **Always-on daemon**: Runs continuously with configurable tick intervals
- **Persistent state**: Automatically saves state to disk
- **Structured logging**: Comprehensive logging with multiple levels
- **Modular architecture**: Plugin system for future integrations
- **Clean shutdown**: Graceful shutdown with state preservation
- **Disk scanning**: Scans directories for algorithms (Prolog files) and philosophies (essays)
- **Calendar integration**: Imports schedule events from ICS files with intelligent tagging
- **Gmail integration**: Extracts work completion evidence and schedule changes from sent emails
- **Event tagging**: Automatically tags events based on configurable keyword rules
- **Evidence extraction**: Automatically detects work completion statements using configurable regex patterns
- **Duplicate prevention**: Prevents duplicate events and work items on re-import
- **Progress tracking**: Track weekly progress against goals with backlog computation
- **Productivity forecasting**: Estimate productivity and forecast catch-up feasibility
- **Comprehensive reporting**: Generate reports on progress, backlog, and feasibility
- **Intelligent planner**: Recommends optimal work blocks based on schedule, fatigue, and goals
- **Preference system**: Customizable preferences for goal modes, work hours, and fatigue thresholds
- **Reminder engine**: Terminal-based notifications for work blocks and events
- **Schedule editing**: Add, remove, or move time blocks with user overrides

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

### Generating Reports

Generate progress and backlog reports:
```bash
# View current week's progress
bin/lucian_report week

# View today's progress (same as week)
bin/lucian_report today

# View backlog from past weeks
bin/lucian_report backlog
```

Reports will show:
- Weekly progress against goals (algorithms and philosophies)
- Completion percentages and visual progress bars
- Backlog accumulation over time
- Catch-up feasibility forecasts with confidence levels
- Estimated productivity and required effort

### Planning and Scheduling

View today's optimal work schedule:
```bash
# Display today's plan with suggested work blocks
bin/lucian_plan today

# Show help
bin/lucian_plan help
```

The planner will:
- Display current schedule events
- Suggest optimal work blocks based on available time
- Consider fatigue levels and preferences
- Provide reasoning for each suggestion
- Show work needed to meet weekly goals

Edit your schedule:
```bash
# List current schedule
bin/lucian_edit_schedule list

# Add a new work block (time, duration in minutes, type)
bin/lucian_edit_schedule add 14:00 90 algorithms

# Remove a block by ID
bin/lucian_edit_schedule remove user_block_12345

# Move a block to a new time
bin/lucian_edit_schedule move event_xyz 15:30

# Show help
bin/lucian_edit_schedule help
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

### Calendar Configuration

The calendar module supports importing schedule events from ICS files:

```json
{
  "calendar": {
    "ics_files": ["path/to/calendar.ics"],
    "tag_rules_file": "config/tag_rules.json",
    "bridge_helper": ""
  }
}
```

- `ics_files`: List of ICS file paths to import events from
- `tag_rules_file`: Path to JSON file containing tagging rules (default: `config/tag_rules.json`)
- `bridge_helper`: Path to native calendar bridge helper (Phase B, optional)

### Gmail Configuration

The Gmail module extracts work completion evidence and schedule changes from sent emails:

```json
{
  "gmail": {
    "helper_script": "src/helpers/gmail_fetch.py",
    "to_address": "recipient@example.com",
    "days_back": 7,
    "max_results": 100,
    "evidence_rules_file": "config/evidence_rules.json",
    "dry_run": false
  }
}
```

- `helper_script`: Path to Python helper script for Gmail API authentication
- `to_address`: Filter emails by recipient address (optional, leave empty for all sent emails)
- `days_back`: Number of days back to fetch emails (default: 7)
- `max_results`: Maximum number of emails to fetch (default: 100)
- `evidence_rules_file`: Path to JSON file containing evidence extraction patterns
- `dry_run`: Set to `true` to test with sample data without calling Gmail API

#### Gmail API Setup

To use the Gmail integration:

1. **Enable Gmail API** in Google Cloud Console:
   - Go to [Google Cloud Console](https://console.cloud.google.com/)
   - Create a new project or select an existing one
   - Enable the Gmail API for your project
   - Create OAuth 2.0 credentials (Desktop application type)
   - Download the credentials JSON file

2. **Install Python dependencies**:
   ```bash
   pip install google-auth google-auth-oauthlib google-auth-httplib2 google-api-python-client
   ```

3. **Configure credentials**:
   - Save the downloaded credentials as `config/gmail_credentials.json`
   - First run will prompt for authentication via browser
   - Token will be saved to `config/gmail_token.json` for future use

4. **Test the integration**:
   ```bash
   # Test with dry-run mode (uses sample data)
   python3 src/helpers/gmail_fetch.py --dry-run
   
   # Fetch actual emails
   python3 src/helpers/gmail_fetch.py --days=7 --max=50
   ```

#### Evidence Extraction Rules

The `config/evidence_rules.json` file defines patterns for extracting work completion and schedule change evidence from email text. The system includes:

**Work Completion Patterns** (5+ patterns):
- Detects "completed X algorithms/philosophies" statements
- Detects "submitted X algorithms/essays" statements
- Detects "finished X algorithms" statements
- Detects "wrote X Prolog clauses" statements
- Detects "delivered/sent X algorithms" statements

**Schedule Change Patterns** (4 patterns):
- Detects event cancellations
- Detects event rescheduling
- Detects "moved X to Y" statements
- Detects new event scheduling

Each pattern includes:
- `name`: Unique identifier for the pattern
- `pattern`: Regular expression for matching
- `type`: Type of evidence (completion, submission, cancellation, reschedule, addition)
- `confidence`: Confidence score (0.0 to 1.0)
- `description`: Human-readable description

Example pattern:
```json
{
  "name": "completed_work",
  "pattern": "(?i)completed\\s+(?:(\\d+)\\s+)?(?:.*?\\s+)?(algorithm|philosophy|essay|clause)s?",
  "type": "completion",
  "confidence": 0.9,
  "description": "Detects 'completed X algorithms/philosophies' statements"
}
```

You can customize these patterns to match your email writing style.

### Disk Scan Configuration

The disk scan module scans directories for algorithms and philosophy files:

- `disk_scan.paths`: List of directories to scan with type (algorithms/philosophies)
- `disk_scan.thresholds`: Completion thresholds for different work types

### Preferences Configuration

Customize planning and scheduling behavior:

```json
{
  "preferences": {
    "goal_mode": "adaptive",
    "quiet_hours_start": 22,
    "quiet_hours_end": 7,
    "minimum_rest_minutes": 15,
    "maximum_work_streak_minutes": 120,
    "minimum_sleep_hours": 7,
    "work_day_start": 8,
    "work_day_end": 20,
    "fatigue_threshold": 0.7,
    "recovery_rate": 0.5,
    "enjoyment_weight": 0.3,
    "travel_prep_buffer_minutes": 30,
    "post_travel_recovery_minutes": 15,
    "notifications_enabled": true,
    "notification_lead_minutes": 5,
    "work_block_min_duration": 30,
    "work_block_preferred_duration": 90
  }
}
```

Key preference options:
- `goal_mode`: `strict` (push to meet 100% of goals) or `adaptive` (flexible goal adjustment)
- `quiet_hours_start/end`: Hours when no notifications should be sent (24-hour format)
- `work_day_start/end`: Preferred work hours (24-hour format)
- `fatigue_threshold`: Fatigue level (0.0-1.0) when rest is recommended
- `work_block_min_duration`: Minimum work block duration in minutes
- `work_block_preferred_duration`: Preferred work block duration in minutes

## Architecture

### Module Structure

```
src/
├── daemon.pl          # Main daemon loop (daemon_start, daemon_stop, daemon_tick)
├── config.pl          # Configuration loading and validation
├── state.pl           # Persistent state management
├── log.pl             # Structured logging
├── api.pl             # Internal API (scan, plan, report)
├── model.pl           # Data model (goals, work items, schedule events, time blocks)
├── validate.pl        # Model validation
├── progress.pl        # Weekly progress and backlog tracking
├── productivity.pl    # Productivity estimation and forecasting
├── report.pl          # Report generation and formatting
├── planner.pl         # Intelligent planning and scheduling engine
├── preferences.pl     # User preference system
├── remind.pl          # Reminder and notification engine
└── modules/           # Plugin modules directory
    ├── disk_scan.pl       # Local disk scanning for algorithms and essays
    ├── calendar_ics.pl    # ICS calendar file parsing
    ├── calendar_bridge.pl # Native calendar integration stub (Phase B)
    └── gmail.pl           # Gmail sent mail ingestion and evidence extraction

tests/
├── run_tests.pl           # Test runner
├── test_daemon.pl         # Daemon tests
├── test_model.pl          # Model tests
├── test_disk_scan.pl      # Disk scan tests
├── test_calendar_ics.pl   # Calendar ICS tests
├── test_progress.pl       # Progress tracking and forecasting tests
├── test_planner.pl        # Planner, preferences, and reminder tests
└── test_gmail.pl          # Gmail integration tests

bin/
├── lucian_planner     # CLI entry point for daemon
├── lucian_report      # CLI entry point for reports
├── lucian_plan        # CLI entry point for day planning
└── lucian_edit_schedule  # CLI entry point for schedule editing

src/helpers/
└── gmail_fetch.py     # Gmail API authentication and fetching helper

config/
├── config.json        # Main configuration file
├── tag_rules.json     # Event tagging rules
└── evidence_rules.json # Work evidence extraction patterns

examples/
└── sample_calendar.ics  # Sample ICS file for testing
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

#### Calendar Module
- `import_ics_file/1`: Import events from an ICS file
- `import_events/0`: Import all configured ICS files
- `parse_ics_file/2`: Parse ICS file and extract events
- `apply_event_tags/3`: Apply tags to events based on keyword rules
- `determine_attendance_confidence/2`: Determine attendance confidence from event status

#### Disk Scan Module
- `scan_directories/0`: Scan all configured directories
- `scan_directory/1`: Scan a single directory
- `count_prolog_clauses/2`: Count clauses in a Prolog file
- `count_words/2`: Count words in a text file
- `classify_completion/3`: Classify completion status based on count

#### Gmail Module
- `import_gmail_data/0`: Import Gmail data using configured settings
- `fetch_gmail_messages/0`: Fetch Gmail messages using default configuration
- `fetch_gmail_messages/1`: Fetch Gmail messages with custom options
- `parse_gmail_json/2`: Parse Gmail JSON output from helper script
- `extract_work_evidence/3`: Extract work completion evidence from email text
- `extract_schedule_changes/3`: Extract schedule change evidence from email text
- `apply_evidence_rules/3`: Apply configured evidence rules to text
- `normalize_work_type/2`: Normalize work type names to standard types

#### Progress Tracking Module
- `current_week/1`: Get current ISO 8601 week number
- `week_range/3`: Get timestamp range for a given week
- `weekly_progress/2`: Compute weekly progress against goals
- `count_completed_work/4`: Count completed work items in a time range
- `backlog/3`: Compute backlog from one week to another
- `compute_backlog/4`: Compute backlog for each work type

#### Productivity Module
- `estimate_productivity/2`: Estimate units per hour for a work type
- `historical_productivity/3`: Calculate productivity from historical data
- `available_work_time/3`: Calculate available work time in a week
- `forecast_achievable_units/4`: Forecast achievable units in future weeks
- `feasibility_forecast/4`: Determine if catch-up is feasible

#### Report Module
- `report_today/0`: Generate today's progress report
- `report_week/0`: Generate current week's progress report
- `report_week/1`: Generate specific week's progress report
- `report_backlog/0`: Generate backlog report
- `report_backlog/2`: Generate backlog report for specific range

#### Planner Module
- `plan_today/1`: Generate today's plan with work block suggestions
- `plan_day/2`: Plan a specific day
- `suggest_work_blocks/3`: Suggest optimal work blocks based on available time
- `find_available_slots/3`: Find available time slots in schedule
- `score_time_slot/4`: Score a time slot based on fatigue and preferences
- `calculate_fatigue/3`: Calculate fatigue model for a day
- `edit_schedule/3`: Add, remove, or move schedule blocks

#### Preferences Module
- `get_preference/2`: Get a user preference value
- `set_preference/2`: Set a user preference
- `load_preferences/0`: Load preferences from configuration
- `validate_preference/2`: Validate a preference value
- `in_quiet_hours/1`: Check if timestamp is in quiet hours
- `in_work_day/1`: Check if timestamp is within work day

#### Reminder Module
- `schedule_reminder/4`: Schedule a reminder for future time
- `trigger_reminder/1`: Trigger a scheduled reminder
- `get_pending_reminders/1`: Get all pending reminders
- `get_due_reminders/2`: Get reminders due before specified time
- `cancel_reminder/1`: Cancel a scheduled reminder
- `reminder_history/2`: Get history of triggered reminders

## Usage Examples

### Importing Calendar Events

To import events from an ICS file, add the file path to your config:

```json
{
  "calendar": {
    "ics_files": ["examples/sample_calendar.ics"]
  }
}
```

Then run:
```prolog
?- calendar_ics:import_events.
```

Events will be automatically tagged based on keywords in the title and location. For example:
- "Weekly Sauna Session" → tagged with `sauna`
- "AI Seminar" → tagged with `seminar`
- "Travel to Home" → tagged with `travel`, `home`

### Customizing Event Tags

Edit `config/tag_rules.json` to customize how events are tagged:

```json
{
  "event_tags": {
    "sauna": ["sauna", "spa"],
    "work": ["work", "office", "meeting"],
    "rest": ["rest", "vacation", "break"]
  }
}
```

### Tracking Progress and Goals

Add goals to track your work progress:

```prolog
?- use_module(src/model).
?- use_module(src/state).

% Initialize state
?- state:init_state.

% Create goals
?- model:create_goal(weekly_algs, algorithms, 100, week, strict, _{}, AlgGoal),
   state:add_goal(AlgGoal).
   
?- model:create_goal(weekly_phils, philosophies, 7, week, strict, _{}, PhilGoal),
   state:add_goal(PhilGoal).

% View weekly progress
?- use_module(src/report).
?- report:report_week.
```

Generate progress reports from the command line:

```bash
# View current week progress
bin/lucian_report week

# View backlog and catch-up feasibility
bin/lucian_report backlog
```

The report will show:
- Completed work vs. weekly targets
- Visual progress bars
- Backlog accumulation
- Catch-up feasibility with confidence levels
- Productivity estimates

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
