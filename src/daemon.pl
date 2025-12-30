% daemon.pl - Main daemon loop
:- module(daemon, [
    daemon_start/0,
    daemon_stop/0,
    daemon_tick/0
]).

:- use_module(config).
:- use_module(log).
:- use_module(state).
:- use_module(api).

% Dynamic facts for daemon control
:- dynamic daemon_running/0.
:- dynamic tick_counter/1.

% Start the daemon
daemon_start :-
    % Check if already running
    (daemon_running ->
        format('Daemon is already running~n'),
        !
    ;
        true
    ),
    
    % Load configuration
    config_path(ConfigPath),
    format('Loading configuration from: ~w~n', [ConfigPath]),
    load_config(ConfigPath),
    
    % Initialize logging
    init_log,
    log_info('Daemon starting...'),
    
    % Initialize state
    init_state,
    log_info('State initialized'),
    
    % Record start time
    get_time(StartTime),
    set_state(start_time, StartTime),
    
    % Mark daemon as running
    assertz(daemon_running),
    assertz(tick_counter(0)),
    
    log_info('Daemon started successfully'),
    format('Daemon started. Press Ctrl+C and call daemon_stop/0 to stop.~n'),
    
    % Enter main loop
    daemon_loop.

% Stop the daemon
daemon_stop :-
    (daemon_running ->
        log_info('Daemon stopping...'),
        
        % Save state before shutdown
        save_state,
        log_info('State saved'),
        
        % Close log
        close_log,
        
        % Clear running flag
        retractall(daemon_running),
        retractall(tick_counter(_)),
        
        format('Daemon stopped cleanly.~n'),
        halt(0)
    ;
        format('Daemon is not running.~n')
    ).

% Main daemon loop
daemon_loop :-
    daemon_running,
    !,
    
    % Execute tick
    daemon_tick,
    
    % Get tick interval
    get_config([daemon, tick_interval_seconds], TickInterval),
    
    % Sleep until next tick
    sleep(TickInterval),
    
    % Continue loop
    daemon_loop.

daemon_loop :-
    % Daemon stopped
    log_info('Daemon loop exited'),
    !.

% Execute one tick
daemon_tick :-
    % Increment tick counter
    (tick_counter(Count) ->
        (NewCount is Count + 1,
         retractall(tick_counter(_)),
         assertz(tick_counter(NewCount)))
    ;
        (NewCount = 1,
         assertz(tick_counter(NewCount)))
    ),
    
    % Update state
    get_time(CurrentTime),
    get_state(tick_count, OldTickCount),
    NewTickCount is OldTickCount + 1,
    set_state(tick_count, NewTickCount),
    set_state(last_tick_time, CurrentTime),
    
    % Log tick
    format(atom(Msg), 'Tick #~w at ~w', [NewTickCount, CurrentTime]),
    log_info(Msg),
    
    % Periodic state save
    get_config([daemon, state_save_interval_ticks], SaveInterval),
    (NewTickCount mod SaveInterval =:= 0 ->
        (save_state,
         log_info('Periodic state save completed'),
         !)
    ;
        true
    ),
    
    % Call API functions (stubs for now)
    api:source_scan(all, _Items),
    state:get_state(tick_count, TickState),
    api:planner_recommendations(TickState, _Recs),
    
    format('Tick #~w completed~n', [NewTickCount]),
    !.

% Handle Ctrl+C gracefully
:- on_signal(int, _, daemon_stop).
