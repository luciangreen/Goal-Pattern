% state.pl - Persistent state management
:- module(state, [
    init_state/0,
    load_state/0,
    save_state/0,
    get_state/2,
    set_state/2,
    update_state/2,
    cleanup_state/0,
    clear_state/0,
    % Entity management
    add_goal/1,
    get_goals/1,
    add_work_item/1,
    get_work_items/1,
    add_schedule_event/1,
    get_schedule_events/1,
    add_time_block/1,
    get_time_blocks/1,
    % Serialization helpers
    goal_to_dict/2,
    dict_to_goal/2,
    work_item_to_dict/2,
    dict_to_work_item/2,
    schedule_event_to_dict/2,
    dict_to_schedule_event/2,
    time_block_to_dict/2,
    dict_to_time_block/2
]).

:- use_module(library(http/json)).
:- use_module(config).
:- use_module(log).
:- use_module(model).

% Dynamic facts to store state
:- dynamic daemon_state/1.

% Initialize state
init_state :-
    retractall(daemon_state(_)),
    
    % Try to load existing state
    (catch(load_state, _, fail) -> 
        true  % State loaded from disk
    ;
        % Initialize with empty state
        EmptyState = _{
            tick_count: 0,
            last_tick_time: null,
            start_time: null,
            modules: _{},
            goals: [],
            work_items: [],
            schedule_events: [],
            time_blocks: []
        },
        assertz(daemon_state(EmptyState))
    ).

% Load state from disk
load_state :-
    get_config([state, file_path], StatePath),
    exists_file(StatePath),
    !,
    open(StatePath, read, Stream),
    json:json_read_dict(Stream, State),
    close(Stream),
    retractall(daemon_state(_)),
    assertz(daemon_state(State)).

load_state :-
    % File doesn't exist, that's okay
    fail.

% Save state to disk
save_state :-
    daemon_state(State),
    get_config([state, file_path], StatePath),
    
    % Ensure state directory exists
    file_directory_name(StatePath, StateDir),
    (exists_directory(StateDir) -> true ; make_directory_path(StateDir)),
    
    % Write state to file
    open(StatePath, write, Stream),
    json:json_write_dict(Stream, State, [width(0)]),
    close(Stream),
    !.

% Get state value
get_state(Key, Value) :-
    daemon_state(State),
    get_dict(Key, State, Value),
    !.

% Set state value
set_state(Key, Value) :-
    daemon_state(State),
    put_dict(Key, State, Value, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)),
    !.

% Update state with a dict
update_state(Updates, NewState) :-
    daemon_state(State),
    merge_dicts(Updates, State, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)).

% Merge two dicts (Updates override State)
merge_dicts(Updates, State, Result) :-
    dict_pairs(Updates, _, UpdatePairs),
    dict_pairs(State, Tag, StatePairs),
    append(StatePairs, UpdatePairs, AllPairs),
    list_to_set(AllPairs, UniquePairs),
    dict_pairs(Result, Tag, UniquePairs).

% Cleanup state
cleanup_state :-
    save_state,
    retractall(daemon_state(_)).

% Clear state (for testing)
clear_state :-
    retractall(daemon_state(_)).

% ============================================================================
% ENTITY MANAGEMENT
% ============================================================================

% Add a goal to state
add_goal(Goal) :-
    goal_to_dict(Goal, GoalDict),
    get_state(goals, Goals),
    append(Goals, [GoalDict], NewGoals),
    set_state(goals, NewGoals).

% Get all goals from state
get_goals(Goals) :-
    get_state(goals, GoalDicts),
    maplist(dict_to_goal, GoalDicts, Goals).

% Add a work item to state
add_work_item(WorkItem) :-
    work_item_to_dict(WorkItem, WorkItemDict),
    get_state(work_items, WorkItems),
    append(WorkItems, [WorkItemDict], NewWorkItems),
    set_state(work_items, NewWorkItems).

% Get all work items from state
get_work_items(WorkItems) :-
    get_state(work_items, WorkItemDicts),
    maplist(dict_to_work_item, WorkItemDicts, WorkItems).

% Add a schedule event to state
add_schedule_event(Event) :-
    schedule_event_to_dict(Event, EventDict),
    get_state(schedule_events, Events),
    append(Events, [EventDict], NewEvents),
    set_state(schedule_events, NewEvents).

% Get all schedule events from state
get_schedule_events(Events) :-
    get_state(schedule_events, EventDicts),
    maplist(dict_to_schedule_event, EventDicts, Events).

% Add a time block to state
add_time_block(Block) :-
    time_block_to_dict(Block, BlockDict),
    get_state(time_blocks, Blocks),
    append(Blocks, [BlockDict], NewBlocks),
    set_state(time_blocks, NewBlocks).

% Get all time blocks from state
get_time_blocks(Blocks) :-
    get_state(time_blocks, BlockDicts),
    maplist(dict_to_time_block, BlockDicts, Blocks).

% ============================================================================
% SERIALIZATION HELPERS
% ============================================================================

% Convert goal to dict for JSON serialization
goal_to_dict(goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata), Dict) :-
    Dict = _{
        id: Id,
        type: Type,
        target_count: TargetCount,
        time_window: TimeWindow,
        strictness: Strictness,
        metadata: Metadata
    }.

% Convert dict to goal structure
dict_to_goal(Dict, goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata)) :-
    get_dict(id, Dict, Id),
    get_dict(type, Dict, Type),
    get_dict(target_count, Dict, TargetCount),
    get_dict(time_window, Dict, TimeWindow),
    get_dict(strictness, Dict, Strictness),
    get_dict(metadata, Dict, Metadata).

% Convert work_item to dict for JSON serialization
work_item_to_dict(work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence), Dict) :-
    Dict = _{
        id: Id,
        type: Type,
        origin: Origin,
        status: Status,
        count: Count,
        timestamps: Timestamps,
        tags: Tags,
        confidence: Confidence
    }.

% Convert dict to work_item structure
dict_to_work_item(Dict, work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence)) :-
    get_dict(id, Dict, Id),
    get_dict(type, Dict, Type),
    get_dict(origin, Dict, Origin),
    get_dict(status, Dict, Status),
    get_dict(count, Dict, Count),
    get_dict(timestamps, Dict, Timestamps),
    get_dict(tags, Dict, Tags),
    get_dict(confidence, Dict, Confidence).

% Convert schedule_event to dict for JSON serialization
schedule_event_to_dict(schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence), Dict) :-
    Dict = _{
        id: Id,
        start: Start,
        end: End,
        title: Title,
        location: Location,
        tags: Tags,
        source: Source,
        attendance_confidence: AttendanceConfidence
    }.

% Convert dict to schedule_event structure
dict_to_schedule_event(Dict, schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence)) :-
    get_dict(id, Dict, Id),
    get_dict(start, Dict, Start),
    get_dict(end, Dict, End),
    get_dict(title, Dict, Title),
    get_dict(location, Dict, Location),
    get_dict(tags, Dict, Tags),
    get_dict(source, Dict, Source),
    get_dict(attendance_confidence, Dict, AttendanceConfidence).

% Convert time_block to dict for JSON serialization
time_block_to_dict(time_block(Start, End, category(Category), FatigueCost, RecoveryCost, Confidence), Dict) :-
    Dict = _{
        start: Start,
        end: End,
        category: Category,
        fatigue_cost: FatigueCost,
        recovery_cost: RecoveryCost,
        confidence: Confidence
    }.

% Convert dict to time_block structure
dict_to_time_block(Dict, time_block(Start, End, category(Category), FatigueCost, RecoveryCost, Confidence)) :-
    get_dict(start, Dict, Start),
    get_dict(end, Dict, End),
    get_dict(category, Dict, Category),
    get_dict(fatigue_cost, Dict, FatigueCost),
    get_dict(recovery_cost, Dict, RecoveryCost),
    get_dict(confidence, Dict, Confidence).
