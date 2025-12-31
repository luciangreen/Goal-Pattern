# Spec 02 Implementation Report

## Unified Data Model (goals, work artifacts, schedule, events, patterns)

**Status:** ✅ FULLY IMPLEMENTED

## Overview

Spec 2 defines a canonical internal model for goals, work done, schedules, places, and patterns. This implementation provides a complete, validated data model with serialization support.

## Implementation Details

### Files Created/Modified

1. **src/model.pl** - Core data model definitions
   - Goal management (goal/6)
   - Work item management (work_item/8)
   - Schedule event management (schedule_event/8)
   - Time block management (time_block/6)
   - Status transitions and safety margin rules

2. **src/validate.pl** - Validation predicates
   - Entity validation
   - Timestamp validation
   - Range validation
   - Type validation (nonnegative, probability, count)

3. **src/state.pl** - Serialization support
   - Conversion between structures and dicts
   - JSON serialization/deserialization
   - State management for all entities

4. **tests/test_model.pl** - Comprehensive test suite
   - 60+ tests covering all data structures
   - Validation tests
   - Serialization tests

### Data Structures Implemented

#### 1. Goal (goal/6)
```prolog
goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata)
```
- **ID**: Unique atom identifier
- **Type**: `algorithms` or `philosophies`
- **TargetCount**: Non-negative number of target items
- **TimeWindow**: Atom (e.g., `week`, `month`)
- **Strictness**: `strict` or `adaptive`
- **Metadata**: Dict with additional info

#### 2. Work Item (work_item/8)
```prolog
work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence)
```
- **ID**: Unique atom identifier
- **Type**: `algorithm` or `philosophy`
- **Origin**: Filepath or source identifier
- **Status**: `draft`, `partial`, `complete`, or `submitted`
- **Count**: Wordcount (essays) or clause count (algorithms)
- **Timestamps**: Dict with created, modified, completed times
- **Tags**: List of classification tags
- **Confidence**: 0.0 to 1.0

#### 3. Schedule Event (schedule_event/8)
```prolog
schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence)
```
- **ID**: Unique atom identifier
- **Start/End**: Unix timestamps
- **Title**: Event title (atom or string)
- **Location**: Location string
- **Tags**: List of tags (travel, sauna, friend_house, home, seminar, etc.)
- **Source**: `ics`, `gmail`, `manual`, or `bridge`
- **AttendanceConfidence**: 0.0 to 1.0

#### 4. Time Block (time_block/6)
```prolog
time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)
```
- **Start/End**: Unix timestamps
- **Category**: `rest`, `work`, `play`, or `travel`
- **FatigueCost**: 0.0 to 1.0 (how much fatigue this adds)
- **RecoveryCost**: 0.0 to 1.0 (how much recovery time needed)
- **Confidence**: 0.0 to 1.0

### Status Transitions

Valid transitions implemented:
- `draft` → `partial`
- `draft` → `complete`
- `partial` → `complete`
- `complete` → `submitted`

### Safety Margin Rule

The +10% safety margin rule (for LLM work) is implemented:
```prolog
apply_safety_margin(Count, AdjustedCount) :-
    AdjustedCount is Count * 1.1.
```

### Validation Predicates

All validation predicates are implemented in `src/validate.pl`:
- `validate_timestamp/1` - Validates Unix timestamps (non-negative)
- `validate_timestamp_range/2` - Validates time ranges (start < end)
- `validate_nonnegative/1` - Validates non-negative numbers
- `validate_probability/1` - Validates probabilities (0.0 to 1.0)
- `validate_count/1` - Validates non-negative integers
- `validate_entity/1` - Validates complete entities

### Serialization

All entities can be serialized to/from JSON via `src/state.pl`:
- Goals stored in `state.goals`
- Work items stored in `state.work_items`
- Schedule events stored in `state.schedule_events`
- Time blocks can be serialized (support available)

## Acceptance Criteria

✅ **All acceptance criteria met:**

1. ✅ You can assert example goals + work items + schedule events and serialize/deserialize them.
   - Implemented via `state:add_goal/1`, `state:add_work_item/1`, `state:add_schedule_event/1`
   - Retrieval via `state:get_goals/1`, `state:get_work_items/1`, `state:get_schedule_events/1`
   - JSON serialization via `state:save_state/0` and `state:load_state/0`

2. ✅ Validation catches malformed timestamps and negative counts.
   - `validate_timestamp/1` rejects negative timestamps
   - `validate_count/1` rejects negative counts
   - `validate_probability/1` rejects out-of-range probabilities
   - All entity validators enforce non-negative counts and valid time ranges

## Test Results

All 60+ tests pass:
- ✅ Goal creation and validation (6 tests)
- ✅ Work item creation and validation (8 tests)
- ✅ Status transitions (10 tests)
- ✅ Schedule event creation and validation (5 tests)
- ✅ Time block creation and validation (8 tests)
- ✅ Safety margin calculations (4 tests)
- ✅ Validation predicates (17 tests)
- ✅ Serialization (4 tests)

Run tests with:
```bash
swipl -q -s tests/run_tests.pl -g run_tests
```

## Usage Examples

### Creating and Storing Entities

```prolog
% Initialize state
?- state:init_state.

% Create a goal
?- model:create_goal(weekly_algorithms, algorithms, 100, week, strict, _{}, Goal),
   state:add_goal(Goal).

% Create a work item
?- get_time(Now),
   Timestamps = _{created: Now, modified: Now},
   model:create_work_item(work1, algorithm, '/path/to/file.pl', draft, 50, 
                          Timestamps, [prolog, test], 0.95, WorkItem),
   state:add_work_item(WorkItem).

% Create a schedule event
?- get_time(Start),
   End is Start + 3600,
   model:create_schedule_event(event1, Start, End, 'Meeting', 'Office', 
                                [work], manual, 0.9, Event),
   state:add_schedule_event(Event).

% Save state to disk
?- state:save_state.
```

### Validating Entities

```prolog
% Validate a goal
?- model:create_goal(g1, algorithms, 80, week, strict, _{}, Goal),
   model:validate_goal(Goal).
true.

% Validate a work item
?- get_time(T),
   model:create_work_item(w1, algorithm, '/test.pl', draft, 10, 
                          _{created: T}, [], 0.8, WorkItem),
   model:validate_work_item(WorkItem).
true.
```

### Status Transitions

```prolog
% Update work item status
?- model:create_work_item(w1, algorithm, '/test.pl', draft, 10, 
                          _{created: 0}, [], 0.8, WorkItem1),
   model:update_work_item_status(WorkItem1, WorkItem2),
   WorkItem2 = work_item(_, _, _, Status, _, _, _, _).
Status = partial.
```

## Integration with Other Specs

This data model is used by:
- **Spec 3** (Disk Scan) - Creates and updates work_item entities
- **Spec 4** (Calendar) - Creates and updates schedule_event entities
- **Spec 6** (Progress Tracking) - Uses goals and work_items for tracking
- **Spec 7** (Statistics) - Analyzes schedule_event and time_block patterns
- **Spec 8** (Planner) - Uses time_block and schedule_event data

## Notes

- All structures use Unix timestamps for date/time representation
- Floating-point arithmetic may introduce minor precision differences (e.g., 110.00000000000001 vs 110)
- The model supports extensibility via the Metadata dict in goals
- Tags are flexible lists that can be customized per use case
- Confidence scores enable probabilistic reasoning about data quality

## Conclusion

Spec 2 is fully implemented and tested. All data structures are in place, validation works correctly, and serialization/deserialization to JSON is functional. The model provides a solid foundation for the remaining specifications.
