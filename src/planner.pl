% planner.pl - Planning function for optimal work/rest/play scheduling
:- module(planner, [
    plan_day/2,
    plan_today/1,
    suggest_work_blocks/3,
    calculate_fatigue/3,
    find_available_slots/3,
    score_time_slot/4,
    edit_schedule/3
]).

:- use_module(library(aggregate)).
:- use_module(state).
:- use_module(model).
:- use_module(log).
:- use_module(preferences).
:- use_module(progress).
:- use_module(productivity).

% ============================================================================
% Main Planning Functions
% ============================================================================

% Plan a full day
% plan_day(Date, Plan)
% Returns: plan(Date, WorkBlocks, Reasons)
plan_day(Date, plan(Date, WorkBlocks, Fatigue, Reasons)) :-
    % Get date range
    date_to_timestamp_range(Date, DayStart, DayEnd),
    
    % Get existing schedule events for the day
    state:get_schedule_events(AllEvents),
    filter_events_in_range(AllEvents, DayStart, DayEnd, _DayEvents),
    
    % Calculate current backlog and goals
    progress:current_week(Week),
    progress:weekly_progress(Week, Progress),
    progress:backlog(Week, Week, Backlog),
    
    % Calculate fatigue model for the day
    calculate_fatigue(DayStart, DayEnd, Fatigue),
    
    % Find available time slots
    find_available_slots(DayStart, DayEnd, AvailableSlots),
    
    % Suggest work blocks based on backlog, fatigue, and preferences
    suggest_work_blocks(AvailableSlots, Progress, WorkBlocks),
    
    % Generate reasons for suggestions
    generate_plan_reasons(WorkBlocks, Progress, Backlog, Fatigue, Reasons),
    
    length(WorkBlocks, BlockCount),
    format(atom(Msg), 'Generated day plan with ~w work blocks', [BlockCount]),
    log:log_info(Msg).

% Plan today
plan_today(Plan) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    date_time_value(date, DateTime, Date),
    plan_day(Date, Plan).

% ============================================================================
% Available Time Slot Detection
% ============================================================================

% Find available time slots in a day (not occupied by events)
find_available_slots(DayStart, DayEnd, AvailableSlots) :-
    % Get schedule events
    state:get_schedule_events(AllEvents),
    filter_events_in_range(AllEvents, DayStart, DayEnd, DayEvents),
    
    % Convert events to occupied time ranges
    maplist(event_to_time_range, DayEvents, OccupiedRanges),
    
    % Sort occupied ranges by start time
    sort(OccupiedRanges, SortedOccupied),
    
    % Find gaps between occupied times
    find_gaps(DayStart, DayEnd, SortedOccupied, Gaps),
    
    % Filter gaps based on work day and quiet hours preferences
    include(is_viable_work_slot, Gaps, ViableSlots),
    
    % Convert to available slots with metadata
    maplist(gap_to_slot, ViableSlots, AvailableSlots).

% Convert event to time range
event_to_time_range(schedule_event(_, Start, End, _, _, _, _, _), range(Start, End)).

% Find gaps between occupied ranges
find_gaps(Start, End, [], [range(Start, End)]) :- !.

find_gaps(Start, End, [range(First, _End)|_], []) :-
    First =< Start,
    !.

find_gaps(Start, End, [range(OccStart, OccEnd)|Rest], Gaps) :-
    (OccStart > Start ->
        % There's a gap before first occupied range
        find_gaps(OccEnd, End, Rest, RestGaps),
        Gaps = [range(Start, OccStart)|RestGaps]
    ;
        % No gap before, continue with rest
        find_gaps(OccEnd, End, Rest, Gaps)
    ).

% Check if a time slot is viable for work
is_viable_work_slot(range(Start, End)) :-
    % Must be at least minimum duration
    preferences:get_preference(work_block_min_duration, MinDuration),
    Duration is (End - Start) / 60,
    Duration >= MinDuration,
    
    % Check if in work day hours
    preferences:in_work_day(Start),
    
    % Check not entirely in quiet hours
    \+ preferences:in_quiet_hours(Start).

% Convert gap to slot with metadata
gap_to_slot(range(Start, End), slot(Start, End, Duration, Score)) :-
    Duration is (End - Start) / 60,
    score_time_slot(Start, End, _, Score).

% ============================================================================
% Work Block Suggestion
% ============================================================================

% Suggest work blocks based on available slots and goals
suggest_work_blocks(AvailableSlots, Progress, WorkBlocks) :-
    % Sort slots by score (best first)
    sort(4, @>=, AvailableSlots, SortedSlots),
    
    % Extract backlog from progress
    extract_backlog_needs(Progress, AlgoNeeded, PhilNeeded),
    
    % Allocate work blocks to slots
    allocate_work_blocks(SortedSlots, AlgoNeeded, PhilNeeded, WorkBlocks).

% Extract backlog needs from progress
extract_backlog_needs(weekly_progress(_, algorithms(Done, Target), philosophies(PhilDone, PhilTarget)), 
                      AlgoNeeded, PhilNeeded) :-
    AlgoNeeded is max(0, Target - Done),
    PhilNeeded is max(0, PhilTarget - PhilDone).

% Allocate work blocks to available slots
allocate_work_blocks([], _, _, []) :- !.

allocate_work_blocks(_, AlgoNeeded, PhilNeeded, []) :-
    AlgoNeeded =< 0,
    PhilNeeded =< 0,
    !.

allocate_work_blocks([slot(Start, _End, Duration, Score)|RestSlots], 
                     AlgoNeeded, PhilNeeded, 
                     [WorkBlock|RestBlocks]) :-
    % Decide work type based on need
    (AlgoNeeded > PhilNeeded ->
        WorkType = algorithms,
        NewAlgoNeeded is AlgoNeeded - 1,
        NewPhilNeeded = PhilNeeded
    ;
        WorkType = philosophies,
        NewAlgoNeeded = AlgoNeeded,
        NewPhilNeeded is PhilNeeded - 1
    ),
    
    % Create work block
    preferences:get_preference(work_block_preferred_duration, PreferredDuration),
    ActualDuration is min(Duration, PreferredDuration),
    BlockEnd is Start + (ActualDuration * 60),
    
    WorkBlock = work_block(Start, BlockEnd, WorkType, ActualDuration, Score, recommended),
    
    % Continue with remaining slots
    allocate_work_blocks(RestSlots, NewAlgoNeeded, NewPhilNeeded, RestBlocks).

allocate_work_blocks([_|RestSlots], AlgoNeeded, PhilNeeded, RestBlocks) :-
    % Skip this slot and try next
    allocate_work_blocks(RestSlots, AlgoNeeded, PhilNeeded, RestBlocks).

% ============================================================================
% Time Slot Scoring
% ============================================================================

% Score a time slot based on fatigue, preferences, and historical productivity
score_time_slot(Start, End, Context, Score) :-
    % Base score
    BaseScore = 1.0,
    
    % Fatigue penalty (lower score for high fatigue times)
    estimate_fatigue_at_time(Start, Fatigue),
    FatiguePenalty is Fatigue * 0.5,
    
    % Time of day preference (morning vs afternoon vs evening)
    time_of_day_score(Start, TimeScore),
    
    % Duration bonus (prefer longer blocks up to preferred duration)
    Duration is (End - Start) / 60,
    preferences:get_preference(work_block_preferred_duration, PreferredDuration),
    DurationScore is min(1.0, Duration / PreferredDuration),
    
    % Combined score
    Score is BaseScore + TimeScore + DurationScore - FatiguePenalty,
    
    Context = _{fatigue: Fatigue, time_score: TimeScore, duration_score: DurationScore}.

% Score based on time of day (prefer morning/late morning)
time_of_day_score(Timestamp, Score) :-
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(hour, DateTime, Hour),
    (Hour >= 9, Hour < 12 ->
        Score = 0.8  % Best: late morning
    ; Hour >= 8, Hour < 9 ->
        Score = 0.6  % Good: early morning
    ; Hour >= 14, Hour < 17 ->
        Score = 0.5  % OK: afternoon
    ;
        Score = 0.2  % Less ideal: early morning or evening
    ).

% ============================================================================
% Fatigue Model
% ============================================================================

% Calculate fatigue throughout a day
calculate_fatigue(DayStart, DayEnd, FatigueModel) :-
    % Simple fatigue model: increases with work, decreases with rest
    % For now, use a simplified version
    
    state:get_schedule_events(AllEvents),
    filter_events_in_range(AllEvents, DayStart, DayEnd, DayEvents),
    
    % Calculate fatigue at key points
    get_preference(fatigue_threshold, Threshold),
    get_preference(recovery_rate, RecoveryRate),
    
    FatigueModel = _{
        start_fatigue: 0.2,  % Assume low fatigue at day start
        threshold: Threshold,
        recovery_rate: RecoveryRate,
        events: DayEvents
    }.

% Estimate fatigue at a specific time
estimate_fatigue_at_time(Timestamp, Fatigue) :-
    % Simple estimation: based on time of day
    % Morning: low fatigue, evening: higher fatigue
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(hour, DateTime, Hour),
    (Hour < 8 ->
        Fatigue = 0.1
    ; Hour < 12 ->
        Fatigue = 0.2
    ; Hour < 16 ->
        Fatigue is 0.2 + ((Hour - 12) * 0.1)
    ; Hour < 20 ->
        Fatigue is 0.5 + ((Hour - 16) * 0.1)
    ;
        Fatigue = 0.8
    ).

% ============================================================================
% Schedule Editing
% ============================================================================

% Edit schedule with user overrides
% Actions: add_block, remove_block, move_block
edit_schedule(add_block, Block, UpdatedSchedule) :-
    % Add a user-defined time block
    model:validate_time_block(Block),
    state:get_schedule_events(_Events),
    
    % Create a schedule event from the block
    time_block(Start, End, Category, _Fatigue, _Recovery, Confidence) = Block,
    
    % Generate unique ID
    get_time(Now),
    format(atom(ID), 'user_block_~w', [Now]),
    
    % Create event
    model:create_schedule_event(ID, Start, End, 
                                'User-defined block', '', 
                                [user_override, Category], 
                                manual, Confidence, Event),
    
    state:add_schedule_event(Event),
    
    state:get_schedule_events(UpdatedSchedule),
    format(atom(Msg), 'Added user block: ~w', [ID]),
    log:log_info(Msg).

edit_schedule(remove_block, BlockID, UpdatedSchedule) :-
    % Remove a block by ID
    state:get_schedule_events(Events),
    exclude(event_has_id(BlockID), Events, FilteredEvents),
    
    % Update state (this is simplified - would need proper state update)
    format(atom(Msg), 'Removed block: ~w', [BlockID]),
    log:log_info(Msg),
    UpdatedSchedule = FilteredEvents.

edit_schedule(move_block, move(BlockID, NewStart, NewEnd), UpdatedSchedule) :-
    % Move a block to new time
    state:get_schedule_events(Events),
    
    % Find and update the block
    (select(Event, Events, RestEvents),
     event_has_id(BlockID, Event) ->
        Event = schedule_event(ID, _, _, Title, Location, Tags, Source, Confidence),
        model:create_schedule_event(ID, NewStart, NewEnd, Title, Location, 
                                    Tags, Source, Confidence, UpdatedEvent),
        UpdatedSchedule = [UpdatedEvent|RestEvents],
        format(atom(Msg), 'Moved block: ~w', [BlockID]),
        log:log_info(Msg)
    ;
        format(atom(Msg), 'Block not found: ~w', [BlockID]),
        log:log_error(Msg),
        UpdatedSchedule = Events
    ).

event_has_id(ID, schedule_event(ID, _, _, _, _, _, _, _)).

% ============================================================================
% Plan Reasoning
% ============================================================================

% Generate human-readable reasons for plan
generate_plan_reasons(WorkBlocks, Progress, _Backlog, Fatigue, Reasons) :-
    length(WorkBlocks, BlockCount),
    
    % Extract backlog info
    extract_backlog_needs(Progress, AlgoNeeded, PhilNeeded),
    
    % Build reasons list
    Reasons = [
        reason(work_blocks_suggested, BlockCount),
        reason(algorithms_needed, AlgoNeeded),
        reason(philosophies_needed, PhilNeeded),
        reason(fatigue_model, Fatigue)
    ].

% ============================================================================
% Utility Functions
% ============================================================================

% Convert date to timestamp range (start and end of day)
date_to_timestamp_range(date(Y, M, D), DayStart, DayEnd) :-
    date_time_stamp(date(Y, M, D, 0, 0, 0, 0, -, -), DayStart),
    DayEnd is DayStart + 86400.

% Filter events within a time range
filter_events_in_range(Events, RangeStart, RangeEnd, FilteredEvents) :-
    include(event_in_range(RangeStart, RangeEnd), Events, FilteredEvents).

event_in_range(RangeStart, RangeEnd, schedule_event(_, Start, End, _, _, _, _, _)) :-
    Start < RangeEnd,
    End > RangeStart.
