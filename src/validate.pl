% validate.pl - Validation predicates for model integrity
:- module(validate, [
    validate_all_entities/1,
    validate_entity/1,
    validate_timestamp/1,
    validate_timestamp_range/2,
    validate_nonnegative/1,
    validate_probability/1,
    validate_count/1
]).

:- use_module(model).

% ============================================================================
% Entity Validation
% ============================================================================

% Validate all entities in a list
validate_all_entities([]).
validate_all_entities([Entity|Rest]) :-
    validate_entity(Entity),
    validate_all_entities(Rest).

% Validate a single entity (delegates to model-specific validators)
validate_entity(goal(_, _, _, _, _, _) = Goal) :-
    model:validate_goal(Goal).

validate_entity(work_item(_, _, _, _, _, _, _, _) = WorkItem) :-
    model:validate_work_item(WorkItem).

validate_entity(schedule_event(_, _, _, _, _, _, _, _) = Event) :-
    model:validate_schedule_event(Event).

validate_entity(time_block(_, _, _, _, _, _) = Block) :-
    model:validate_time_block(Block).

% ============================================================================
% Basic Type Validators
% ============================================================================

% Validate timestamp (must be a positive number representing Unix time)
validate_timestamp(Timestamp) :-
    number(Timestamp),
    Timestamp >= 0,
    !.

% Validate timestamp range (start must be before end)
validate_timestamp_range(Start, End) :-
    validate_timestamp(Start),
    validate_timestamp(End),
    Start < End,
    !.

% Validate nonnegative number
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

% Validate count (nonnegative integer)
validate_count(Count) :-
    integer(Count),
    Count >= 0,
    !.
