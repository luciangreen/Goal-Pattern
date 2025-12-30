% config.pl - Configuration loading and validation
:- module(config, [
    load_config/1,
    get_config/2,
    validate_config/1,
    config_path/1
]).

:- use_module(library(http/json)).

% Dynamic facts to store configuration
:- dynamic current_config/1.

% Default config path
config_path('config/config.json') :- !.

% Load configuration from file
load_config(FilePath) :-
    exists_file(FilePath),
    !,
    open(FilePath, read, Stream),
    json:json_read_dict(Stream, Config),
    close(Stream),
    retractall(current_config(_)),
    assertz(current_config(Config)),
    validate_config(Config).

load_config(FilePath) :-
    format(atom(Msg), 'Config file not found: ~w', [FilePath]),
    throw(error(config_error(file_not_found), Msg)).

% Get configuration value by path
get_config(Path, Value) :-
    current_config(Config),
    get_config_value(Path, Config, Value),
    !.

get_config_value([Key], Dict, Value) :-
    get_dict(Key, Dict, Value).

get_config_value([Key|Rest], Dict, Value) :-
    get_dict(Key, Dict, SubDict),
    get_config_value(Rest, SubDict, Value).

% Validate configuration
validate_config(Config) :-
    % Check daemon configuration
    get_dict(daemon, Config, Daemon),
    get_dict(tick_interval_seconds, Daemon, TickInterval),
    integer(TickInterval),
    TickInterval > 0,
    
    % Check state configuration
    get_dict(state, Config, State),
    get_dict(file_path, State, _StatePath),
    
    % Check log configuration
    get_dict(log, Config, Log),
    get_dict(file_path, Log, _LogPath),
    
    % Check modules configuration
    get_dict(modules, Config, Modules),
    get_dict(enabled, Modules, EnabledList),
    is_list(EnabledList),
    !.

validate_config(Config) :-
    format('Invalid configuration: ~w~n', [Config]),
    fail.
