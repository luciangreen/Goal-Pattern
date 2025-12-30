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
    % Work item management
    add_work_item/1,
    get_work_items/1,
    update_work_item/2,
    find_work_item/2,
    % Goal management
    add_goal/1,
    get_goals/1,
    % Schedule event management
    add_schedule_event/1,
    get_schedule_events/1
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
            work_items: [],
            goals: [],
            schedule_events: []
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
% Work Item Management
% ============================================================================

% Convert work_item structure to dict
work_item_to_dict(work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence),
                  _{id: ID, type: Type, origin: Origin, status: Status, 
                    count: Count, timestamps: Timestamps, tags: Tags, confidence: Confidence}).

% Convert dict to work_item structure
dict_to_work_item(_{id: ID, type: Type, origin: Origin, status: Status,
                    count: Count, timestamps: Timestamps, tags: Tags, confidence: Confidence},
                  work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence)).

% Add a work item to state
add_work_item(WorkItem) :-
    model:validate_work_item(WorkItem),
    work_item_to_dict(WorkItem, Dict),
    daemon_state(State),
    (get_dict(work_items, State, Items) -> true ; Items = []),
    append(Items, [Dict], NewItems),
    put_dict(work_items, State, NewItems, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)),
    !.

% Get all work items
get_work_items(WorkItems) :-
    daemon_state(State),
    get_dict(work_items, State, Items),
    maplist(dict_to_work_item, Items, WorkItems),
    !.

get_work_items([]).

% Find a work item by ID
find_work_item(ID, WorkItem) :-
    get_work_items(Items),
    member(work_item(ID, _, _, _, _, _, _, _) = WorkItem, Items),
    !.

% Update a work item (replace by ID)
update_work_item(ID, NewWorkItem) :-
    model:validate_work_item(NewWorkItem),
    NewWorkItem = work_item(ID, _, _, _, _, _, _, _),
    daemon_state(State),
    get_dict(work_items, State, Items),
    work_item_to_dict(NewWorkItem, NewDict),
    % Replace the item with matching ID
    exclude(is_item_with_id(ID), Items, FilteredItems),
    append(FilteredItems, [NewDict], UpdatedItems),
    put_dict(work_items, State, UpdatedItems, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)),
    !.

% Helper to check if dict has given ID
is_item_with_id(ID, Dict) :-
    get_dict(id, Dict, ID).

% ============================================================================
% Goal Management
% ============================================================================

% Convert goal structure to dict
goal_to_dict(goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata),
             _{id: ID, type: Type, target_count: TargetCount, 
               time_window: TimeWindow, strictness: Strictness, metadata: Metadata}).

% Convert dict to goal structure
dict_to_goal(_{id: ID, type: Type, target_count: TargetCount,
               time_window: TimeWindow, strictness: Strictness, metadata: Metadata},
             goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata)).

% Add a goal to state
add_goal(Goal) :-
    model:validate_goal(Goal),
    goal_to_dict(Goal, Dict),
    daemon_state(State),
    (get_dict(goals, State, Goals) -> true ; Goals = []),
    append(Goals, [Dict], NewGoals),
    put_dict(goals, State, NewGoals, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)),
    !.

% Get all goals
get_goals(Goals) :-
    daemon_state(State),
    (get_dict(goals, State, GoalDicts) -> true ; GoalDicts = []),
    maplist(dict_to_goal, GoalDicts, Goals),
    !.

get_goals([]).

% ============================================================================
% Schedule Event Management
% ============================================================================

% Convert schedule_event structure to dict
schedule_event_to_dict(schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence),
                       _{id: ID, start: Start, end: End, title: Title, 
                         location: Location, tags: Tags, source: Source, 
                         attendance_confidence: AttendanceConfidence}).

% Convert dict to schedule_event structure
dict_to_schedule_event(_{id: ID, start: Start, end: End, title: Title,
                         location: Location, tags: Tags, source: Source,
                         attendance_confidence: AttendanceConfidence},
                       schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence)).

% Add a schedule event to state
add_schedule_event(Event) :-
    model:validate_schedule_event(Event),
    schedule_event_to_dict(Event, Dict),
    daemon_state(State),
    (get_dict(schedule_events, State, Events) -> true ; Events = []),
    append(Events, [Dict], NewEvents),
    put_dict(schedule_events, State, NewEvents, NewState),
    retractall(daemon_state(_)),
    assertz(daemon_state(NewState)),
    !.

% Get all schedule events
get_schedule_events(Events) :-
    daemon_state(State),
    (get_dict(schedule_events, State, EventDicts) -> true ; EventDicts = []),
    maplist(dict_to_schedule_event, EventDicts, Events),
    !.

get_schedule_events([]).
