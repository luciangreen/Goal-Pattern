% test_daemon.pl - Tests for daemon functionality
:- begin_tests(daemon).

:- use_module('../src/config').
:- use_module('../src/log').
:- use_module('../src/state').
:- use_module('../src/daemon').

test(config_load) :-
    % Test that config loads successfully
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    config:get_config([daemon, tick_interval_seconds], TickInterval),
    assertion(integer(TickInterval)),
    assertion(TickInterval > 0),
    !.

test(config_validation) :-
    % Test config validation
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    config:current_config(Config),
    config:validate_config(Config),
    !.

test(state_initialization) :-
    % Test state initialization
    % Clean up any existing state first
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    config:get_config([state, file_path], StatePath),
    (exists_file(StatePath) -> delete_file(StatePath) ; true),
    state:clear_state,
    
    % Now initialize fresh state
    state:init_state,
    state:get_state(tick_count, Count),
    assertion(Count =:= 0),
    !.

test(state_persistence) :-
    % Test state save/load
    state:init_state,
    state:set_state(tick_count, 42),
    state:save_state,
    
    % Clear state and reload
    state:clear_state,
    state:load_state,
    state:get_state(tick_count, Count),
    assertion(Count =:= 42),
    !.

test(tick_execution) :-
    % Test that tick increments counter
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    log:init_log,
    state:init_state,
    
    % Get initial tick count
    state:get_state(tick_count, InitialCount),
    
    % Execute a tick
    daemon:daemon_tick,
    
    % Check tick count increased
    state:get_state(tick_count, NewCount),
    assertion(NewCount =:= InitialCount + 1),
    
    % Cleanup
    log:close_log,
    !.

test(state_periodic_save) :-
    % Test that state saves periodically
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    log:init_log,
    state:init_state,
    
    % Get save interval
    config:get_config([daemon, state_save_interval_ticks], SaveInterval),
    
    % Execute ticks up to save interval
    forall(between(1, SaveInterval, _), daemon:daemon_tick),
    
    % State should be saved (file should exist)
    config:get_config([state, file_path], StatePath),
    assertion(exists_file(StatePath)),
    
    % Cleanup
    log:close_log,
    !.

:- end_tests(daemon).
