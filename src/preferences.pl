% preferences.pl - User preference system for planning and scheduling
:- module(preferences, [
    get_preference/2,
    set_preference/2,
    load_preferences/0,
    save_preferences/0,
    default_preference/2,
    validate_preference/2
]).

:- use_module(config).
:- use_module(log).

% Dynamic facts to store preferences
:- dynamic user_preference/2.

% ============================================================================
% Default Preferences
% ============================================================================

% Goal mode: strict | adaptive
default_preference(goal_mode, adaptive).

% Quiet hours (no notifications/work suggestions)
default_preference(quiet_hours_start, 22).  % 10 PM
default_preference(quiet_hours_end, 7).      % 7 AM

% Minimum rest between work sessions (minutes)
default_preference(minimum_rest_minutes, 15).

% Maximum continuous work streak (minutes)
default_preference(maximum_work_streak_minutes, 120).

% Minimum sleep hours per day
default_preference(minimum_sleep_hours, 7).

% Work day start/end (hours)
default_preference(work_day_start, 8).
default_preference(work_day_end, 20).

% Fatigue threshold (0.0-1.0, when to suggest rest)
default_preference(fatigue_threshold, 0.7).

% Recovery rate (fatigue points per hour of rest)
default_preference(recovery_rate, 0.5).

% Enjoyment weight in planning (0.0-1.0)
default_preference(enjoyment_weight, 0.3).

% Travel preparation buffer (minutes before event)
default_preference(travel_prep_buffer_minutes, 30).

% Post-travel recovery buffer (minutes after travel)
default_preference(post_travel_recovery_minutes, 15).

% Enable notifications
default_preference(notifications_enabled, true).

% Notification lead time (minutes before suggested work block)
default_preference(notification_lead_minutes, 5).

% Work block minimum duration (minutes)
default_preference(work_block_min_duration, 30).

% Work block preferred duration (minutes)
default_preference(work_block_preferred_duration, 90).

% ============================================================================
% Preference Access
% ============================================================================

% Get a preference value
get_preference(Key, Value) :-
    user_preference(Key, Value),
    !.

get_preference(Key, Value) :-
    default_preference(Key, Value),
    !.

get_preference(Key, _) :-
    format(atom(Msg), 'Unknown preference key: ~w', [Key]),
    log:log_error(Msg),
    fail.

% Set a preference value
set_preference(Key, Value) :-
    validate_preference(Key, Value),
    !,
    retractall(user_preference(Key, _)),
    assertz(user_preference(Key, Value)),
    format(atom(Msg), 'Preference set: ~w = ~w', [Key, Value]),
    log:log_info(Msg).

set_preference(Key, Value) :-
    format(atom(Msg), 'Invalid preference value: ~w = ~w', [Key, Value]),
    log:log_error(Msg),
    fail.

% ============================================================================
% Preference Validation
% ============================================================================

% Validate preference values
validate_preference(goal_mode, Mode) :-
    member(Mode, [strict, adaptive]).

validate_preference(quiet_hours_start, Hour) :-
    integer(Hour),
    Hour >= 0,
    Hour =< 23.

validate_preference(quiet_hours_end, Hour) :-
    integer(Hour),
    Hour >= 0,
    Hour =< 23.

validate_preference(minimum_rest_minutes, Minutes) :-
    integer(Minutes),
    Minutes >= 0.

validate_preference(maximum_work_streak_minutes, Minutes) :-
    integer(Minutes),
    Minutes > 0.

validate_preference(minimum_sleep_hours, Hours) :-
    number(Hours),
    Hours >= 0,
    Hours =< 24.

validate_preference(work_day_start, Hour) :-
    integer(Hour),
    Hour >= 0,
    Hour =< 23.

validate_preference(work_day_end, Hour) :-
    integer(Hour),
    Hour >= 0,
    Hour =< 23.

validate_preference(fatigue_threshold, Threshold) :-
    number(Threshold),
    Threshold >= 0.0,
    Threshold =< 1.0.

validate_preference(recovery_rate, Rate) :-
    number(Rate),
    Rate >= 0.0.

validate_preference(enjoyment_weight, Weight) :-
    number(Weight),
    Weight >= 0.0,
    Weight =< 1.0.

validate_preference(travel_prep_buffer_minutes, Minutes) :-
    integer(Minutes),
    Minutes >= 0.

validate_preference(post_travel_recovery_minutes, Minutes) :-
    integer(Minutes),
    Minutes >= 0.

validate_preference(notifications_enabled, Value) :-
    member(Value, [true, false]).

validate_preference(notification_lead_minutes, Minutes) :-
    integer(Minutes),
    Minutes >= 0.

validate_preference(work_block_min_duration, Minutes) :-
    integer(Minutes),
    Minutes > 0.

validate_preference(work_block_preferred_duration, Minutes) :-
    integer(Minutes),
    Minutes > 0.

% ============================================================================
% Persistence
% ============================================================================

% Load preferences from configuration
load_preferences :-
    log:log_info('Loading user preferences...'),
    
    % Try to load from config
    (get_config([preferences], PrefsDict) ->
        dict_pairs(PrefsDict, _, Pairs),
        maplist(load_preference_pair, Pairs)
    ;
        log:log_info('No preferences found in config, using defaults')
    ),
    !.

load_preferences :-
    log:log_warn('Failed to load preferences, using defaults').

% Load a single preference key-value pair
load_preference_pair(Key-Value) :-
    (validate_preference(Key, Value) ->
        retractall(user_preference(Key, _)),
        assertz(user_preference(Key, Value))
    ;
        format(atom(Msg), 'Invalid preference in config: ~w = ~w', [Key, Value]),
        log:log_warn(Msg)
    ).

% Save preferences to configuration (would require config update - stub for now)
save_preferences :-
    log:log_info('Saving user preferences...'),
    % This would update config.json - for now, preferences are volatile
    % unless stored in config file manually
    findall(Key-Value, user_preference(Key, Value), Pairs),
    length(Pairs, Count),
    format(atom(Msg), 'Would save ~w preferences (stub)', [Count]),
    log:log_info(Msg).

% ============================================================================
% Utility Predicates
% ============================================================================

% Check if current time is in quiet hours
in_quiet_hours(Timestamp) :-
    get_preference(quiet_hours_start, Start),
    get_preference(quiet_hours_end, End),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(hour, DateTime, Hour),
    (Start < End ->
        % Normal case: quiet hours don't cross midnight
        Hour >= Start, Hour < End
    ;
        % Quiet hours cross midnight
        (Hour >= Start ; Hour < End)
    ).

% Check if timestamp is within work day
in_work_day(Timestamp) :-
    get_preference(work_day_start, Start),
    get_preference(work_day_end, End),
    stamp_date_time(Timestamp, DateTime, local),
    date_time_value(hour, DateTime, Hour),
    Hour >= Start,
    Hour < End.
