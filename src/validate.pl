% validate.pl - Validation predicates for model integrity
:- module(validate, [
    validate_goal/1,
    validate_work_item/1,
    validate_schedule_event/1,
    validate_time_block/1,
    validate_timestamp/1,
    validate_time_range/2,
    validate_nonnegative/1,
    validate_probability/1
]).

:- use_module(model).

% ============================================================================
% BASIC VALIDATORS
% ============================================================================

% Validate timestamp format (ISO 8601 or timestamp dict)
validate_timestamp(Timestamp) :-
    (atom(Timestamp) ; string(Timestamp)),
    % Check if it's a valid ISO 8601 format (simplified check)
    (atom(Timestamp) -> atom_length(Timestamp, Len) ; string_length(Timestamp, Len)),
    Len >= 10,  % At least YYYY-MM-DD
    !.

validate_timestamp(Timestamp) :-
    is_dict(Timestamp),
    get_dict(year, Timestamp, Year),
    get_dict(month, Timestamp, Month),
    get_dict(day, Timestamp, Day),
    integer(Year),
    integer(Month),
    integer(Day),
    Year >= 1900,
    Month >= 1,
    Month =< 12,
    Day >= 1,
    Day =< 31,
    !.

validate_timestamp(null) :- !.  % null timestamps are valid for optional fields

% Validate time range (start must be before or equal to end)
validate_time_range(Start, End) :-
    validate_timestamp(Start),
    validate_timestamp(End),
    % If both are atoms or strings (ISO 8601), simple lexicographic comparison works
    ((atom(Start) ; string(Start)), (atom(End) ; string(End)) ->
        Start @=< End
    ;
        true  % For dict timestamps, we trust the structure for now
    ),
    !.

% Validate non-negative number
validate_nonnegative(Value) :-
    number(Value),
    Value >= 0,
    !.

% Validate probability (0.0 to 1.0)
validate_probability(Value) :-
    number(Value),
    Value >= 0.0,
    Value =< 1.0,
    !.

% ============================================================================
% GOAL VALIDATION
% ============================================================================

% Validate a goal structure
validate_goal(goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata)) :-
    % Id must be present (atom or integer)
    (atom(Id) ; integer(Id)),
    !,
    
    % Type must be valid
    model:goal_type(Type),
    
    % TargetCount must be positive integer
    integer(TargetCount),
    TargetCount > 0,
    
    % TimeWindow must be a dict or atom
    (is_dict(TimeWindow) ; atom(TimeWindow)),
    
    % Strictness must be valid
    model:goal_strictness(Strictness),
    
    % Metadata must be a dict
    is_dict(Metadata).

% ============================================================================
% WORK ITEM VALIDATION
% ============================================================================

% Validate a work item structure
validate_work_item(work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence)) :-
    % Id must be present
    (atom(Id) ; integer(Id)),
    !,
    
    % Type must be valid
    model:work_item_type(Type),
    
    % Origin must be present
    (atom(Origin) ; is_dict(Origin)),
    
    % Status must be valid
    model:work_item_status(Status),
    
    % Count must be non-negative
    validate_nonnegative(Count),
    
    % Timestamps must be a dict with valid timestamps
    validate_timestamps_dict(Timestamps),
    
    % Tags must be a list
    is_list(Tags),
    
    % Confidence must be a probability
    validate_probability(Confidence).

% Validate timestamps dictionary
validate_timestamps_dict(Timestamps) :-
    is_dict(Timestamps),
    (get_dict(created, Timestamps, Created) ->
        validate_timestamp(Created)
    ;
        true
    ),
    (get_dict(modified, Timestamps, Modified) ->
        validate_timestamp(Modified)
    ;
        true
    ),
    (get_dict(completed, Timestamps, Completed) ->
        validate_timestamp(Completed)
    ;
        true
    ),
    !.

% ============================================================================
% SCHEDULE EVENT VALIDATION
% ============================================================================

% Validate a schedule event structure
validate_schedule_event(schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence)) :-
    % Id must be present
    (atom(Id) ; integer(Id)),
    !,
    
    % Validate time range
    validate_time_range(Start, End),
    
    % Title must be present
    (atom(Title) ; string(Title)),
    
    % Location can be atom, string, or dict
    (atom(Location) ; string(Location) ; is_dict(Location) ; Location = null),
    
    % Tags must be a list
    is_list(Tags),
    
    % Source must be present
    atom(Source),
    
    % AttendanceConfidence must be a probability
    validate_probability(AttendanceConfidence).

% ============================================================================
% TIME BLOCK VALIDATION
% ============================================================================

% Validate a time block structure
validate_time_block(time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)) :-
    % Validate time range
    validate_time_range(Start, End),
    
    % Category must be valid
    model:time_block_category(Category),
    
    % FatigueCost must be non-negative
    validate_nonnegative(FatigueCost),
    
    % RecoveryCost must be non-negative
    validate_nonnegative(RecoveryCost),
    
    % Confidence must be a probability
    validate_probability(Confidence),
    !.
