% log.pl - Structured logging
:- module(log, [
    init_log/0,
    log_info/1,
    log_warn/1,
    log_error/1,
    log_debug/1,
    close_log/0
]).

:- use_module(config).
:- dynamic log_stream/1.
:- dynamic log_level/1.

% Initialize logging
init_log :-
    retractall(log_stream(_)),
    retractall(log_level(_)),
    
    % Set default log level
    (get_config([daemon, log_level], LevelStr) -> atom_string(Level, LevelStr) ; Level = info),
    assertz(log_level(Level)),
    
    % Get log file path
    (get_config([log, file_path], LogPath) -> true ; LogPath = 'logs/daemon.log'),
    
    % Ensure log directory exists
    file_directory_name(LogPath, LogDir),
    (exists_directory(LogDir) -> true ; make_directory_path(LogDir)),
    
    % Open log file
    open(LogPath, append, Stream),
    assertz(log_stream(Stream)),
    
    % Log initialization
    get_time(Time),
    format_time(atom(Timestamp), '%Y-%m-%d %H:%M:%S', Time),
    format(Stream, '[~w] [INFO] Log initialized~n', [Timestamp]),
    flush_output(Stream).

% Close logging
close_log :-
    (log_stream(Stream) ->
        (log_info('Log closing'),
         close(Stream),
         retractall(log_stream(_)))
    ; true).

% Log at different levels
log_info(Message) :-
    log_message(info, 'INFO', Message).

log_warn(Message) :-
    log_message(warn, 'WARN', Message).

log_error(Message) :-
    log_message(error, 'ERROR', Message).

log_debug(Message) :-
    log_message(debug, 'DEBUG', Message).

% Internal logging predicate
log_message(Level, LevelStr, Message) :-
    should_log(Level),
    !,
    get_time(Time),
    format_time(atom(Timestamp), '%Y-%m-%d %H:%M:%S', Time),
    
    % Log to file
    (log_stream(Stream) ->
        (format(Stream, '[~w] [~w] ~w~n', [Timestamp, LevelStr, Message]),
         flush_output(Stream))
    ; true),
    
    % Log to console if enabled
    (get_config([log, console_output], true) ->
        format('[~w] [~w] ~w~n', [Timestamp, LevelStr, Message])
    ; true).

log_message(_, _, _) :-
    % Always succeed even if we don't log
    !.

% Check if message should be logged based on level
should_log(error) :- !.
should_log(warn) :- log_level(Level), member(Level, [warn, info, debug]), !.
should_log(info) :- log_level(Level), member(Level, [info, debug]), !.
should_log(debug) :- log_level(debug), !.
should_log(_) :- fail.
