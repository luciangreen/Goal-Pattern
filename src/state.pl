% state.pl - Persistent state management
:- module(state, [
    init_state/0,
    load_state/0,
    save_state/0,
    get_state/2,
    set_state/2,
    update_state/2,
    cleanup_state/0,
    clear_state/0
]).

:- use_module(library(http/json)).
:- use_module(config).
:- use_module(log).

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
            modules: _{}
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
