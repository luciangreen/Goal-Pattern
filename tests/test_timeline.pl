% test_timeline.pl - Tests for timeline generation
:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- use_module('../src/config').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/log').
:- use_module('../src/timeline').

% ============================================================================
% Timeline Building Tests
% ============================================================================

:- begin_tests(timeline).

test(build_empty_timeline, [true(Timeline = [])]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Build timeline with no events
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    timeline:build_timeline(Start, End, Timeline).

test(build_timeline_with_single_event, [true]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create a 1-hour event
    get_time(Now),
    Start is Now,
    End is Now + 3600,
    model:create_schedule_event(event1, Start, End, 'Test Event', 'Home', 
                               [home, work], ics, 0.8, Event),
    state:add_schedule_event(Event),
    
    % Build timeline
    timeline:build_timeline(Start, End, Timeline),
    length(Timeline, L),
    L > 0.

test(event_overlaps_minute, [true]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create event from minute 100 to 200
    StartStamp is 100 * 60,
    EndStamp is 200 * 60,
    model:create_schedule_event(event1, StartStamp, EndStamp, 'Test', 'Home',
                               [home], ics, 1.0, Event),
    state:add_schedule_event(Event),
    
    % Check minute 150 overlaps
    timeline:event_overlaps_minute(Event, 150).

test(event_not_overlaps_minute, [fail]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create event from minute 100 to 200
    StartStamp is 100 * 60,
    EndStamp is 200 * 60,
    model:create_schedule_event(event1, StartStamp, EndStamp, 'Test', 'Home',
                               [home], ics, 1.0, Event),
    state:add_schedule_event(Event),
    
    % Check minute 250 does not overlap
    timeline:event_overlaps_minute(Event, 250).

test(category_from_tag_mapping, [true]) :-
    timeline:category_from_tag(travel, travel),
    timeline:category_from_tag(sauna, rest),
    timeline:category_from_tag(friend_house, play),
    timeline:category_from_tag(home, work).

test(event_to_minutes, [true(length(Minutes, 61))]) :-
    % Event spanning 61 minutes (minute 0 to 60 inclusive)
    StartStamp is 0,
    EndStamp is 60 * 60,
    model:create_schedule_event(event1, StartStamp, EndStamp, 'Test', 'Home',
                               [home], ics, 1.0, Event),
    timeline:event_to_minutes(Event, Minutes).

% ============================================================================
% Work Outcome Window Tests
% ============================================================================

test(work_outcome_windows_empty, [true(Windows = [])]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Build empty timeline
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    timeline:build_timeline(Start, End, Timeline),
    
    % No work items
    timeline:work_outcome_windows(Timeline, [], Windows).

test(work_outcome_window_with_completed_item, [true(length(Windows, 1))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create a completed work item
    get_time(Now),
    CompletedStamp is Now - 1800,  % 30 minutes ago
    Timestamps = _{created: Now - 7200, modified: CompletedStamp, completed: CompletedStamp},
    model:create_work_item(work1, algorithm, 'test.pl', complete, 10,
                          Timestamps, [test], 0.9, WorkItem),
    
    % Create timeline with some events before completion
    Start is Now - 7200,  % 2 hours before now
    End is Now,
    
    % Add a schedule event during work window
    EventStart is CompletedStamp - 3600,
    EventEnd is CompletedStamp,
    model:create_schedule_event(event1, EventStart, EventEnd, 'Work Session', 'Home',
                               [home, work], ics, 1.0, Event),
    state:add_schedule_event(Event),
    
    timeline:build_timeline(Start, End, Timeline),
    
    % Get work outcome windows
    timeline:work_outcome_windows(Timeline, [WorkItem], Windows).

test(get_minute_categories, [true(member(work, Categories))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create event with work tag
    get_time(Now),
    Start is Now,
    End is Now + 3600,
    model:create_schedule_event(event1, Start, End, 'Work', 'Home',
                               [home, work], ics, 1.0, Event),
    state:add_schedule_event(Event),
    
    % Build timeline
    timeline:build_timeline(Start, End, Timeline),
    
    % Get categories
    timeline:get_minute_categories(Start, End, Timeline, Categories).

:- end_tests(timeline).
