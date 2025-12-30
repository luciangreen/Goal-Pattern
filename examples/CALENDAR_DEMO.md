# Calendar Module Demo

This demonstrates the calendar ICS import functionality for Spec 04.

## Overview

The calendar module provides:
- ICS file parsing for schedule events
- Automatic event tagging based on keywords
- Attendance confidence determination
- Duplicate event prevention

## Usage

### 1. Configure ICS files to import

Edit `config/config.json`:

```json
{
  "calendar": {
    "ics_files": ["examples/sample_calendar.ics"]
  }
}
```

### 2. Import events

```prolog
?- use_module('src/modules/calendar_ics').
?- use_module('src/state').
?- use_module('src/config').

% Initialize
?- config:load_config('config/config.json').
?- state:init_state.

% Import events
?- calendar_ics:import_events.

% View imported events
?- state:get_schedule_events(Events), 
   forall(
       member(schedule_event(ID, Start, End, Title, Location, Tags, Source, Conf), Events),
       format('~w: ~w at ~w (Tags: ~w, Confidence: ~w)~n', [ID, Title, Location, Tags, Conf])
   ).
```

## Example Output

When importing the sample calendar (`examples/sample_calendar.ics`), you'll see:

```
ics_sample-event-001@example.com: Team Meeting at Office Meeting Room A (Tags: [seminar,work], Confidence: 0.9)
ics_sample-event-002@example.com: Weekly Sauna Session at Wellness Center Sauna (Tags: [sauna], Confidence: 0.9)
ics_sample-event-003@example.com: AI Seminar - Deep Learning Advances at University Hall 101 (Tags: [seminar], Confidence: 0.3)
ics_sample-event-004@example.com: Haircut at Downtown Barber Shop (Tags: [haircut], Confidence: 0.9)
ics_sample-event-005@example.com: Travel to Home for Holidays at Airport Terminal 2 (Tags: [home,travel], Confidence: 0.9)
```

## Event Tagging

Events are automatically tagged based on keyword rules in `config/tag_rules.json`:

- **Sauna**: Keywords like "sauna", "spa"
- **Seminar**: Keywords like "seminar", "conference", "workshop", "meeting"
- **Work**: Keywords like "work", "office", "project"
- **Travel**: Keywords like "travel", "flight", "airport"
- **Haircut**: Keywords like "haircut", "barber", "salon"
- **Home**: Keywords like "home"
- **Rest**: Keywords like "rest", "vacation", "break"

## Attendance Confidence

Confidence is determined from the ICS STATUS field:
- **CONFIRMED** → 0.9
- **TENTATIVE** → 0.3
- **CANCELLED** → 0.0
- **Default** → 0.5

## Phase B: Native Integration

The `calendar_bridge.pl` module provides a stub interface for future native calendar integration (e.g., macOS EventKit).

When implemented, it will:
1. Fetch events directly from the system calendar
2. Support date range queries
3. Output events in JSON format
4. Integrate seamlessly with the same tagging and storage logic

## Testing

Run the calendar module tests:

```bash
swipl -q -s tests/run_tests.pl -g run_tests
```

All tests should pass, including:
- ICS parsing
- Multiple event handling
- Event tagging
- Attendance confidence
- Datetime parsing
- Event storage and updates
- Duplicate prevention
- Malformed ICS handling
