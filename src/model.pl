% model.pl - Unified data model for goals, work artifacts, schedule, events, patterns
:- module(model, [
    % Goal predicates
    create_goal/7,
    validate_goal/1,
    
    % Work item predicates
    create_work_item/9,
    validate_work_item/1,
    update_work_item_status/2,
    
    % Schedule event predicates
    create_schedule_event/9,
    validate_schedule_event/1,
    
    % Time block predicates
    create_time_block/7,
    validate_time_block/1,
    
    % Status transition predicates
    valid_status_transition/2,
    next_status/2,
    
    % Helper predicates
    work_item_is_complete/1,
    apply_safety_margin/2
]).

% ============================================================================
% Goal Management
% ============================================================================

% goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata)
% Type: algorithms | philosophies
% TargetCount: target number of items
% TimeWindow: week | month | etc.
% Strictness: strict | adaptive
% Metadata: dict with additional info

create_goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata, 
            goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata)) :-
    atom(ID),
    member(Type, [algorithms, philosophies]),
    number(TargetCount),
    TargetCount >= 0,
    atom(TimeWindow),
    member(Strictness, [strict, adaptive]),
    is_dict(Metadata),
    !.

validate_goal(goal(ID, Type, TargetCount, TimeWindow, Strictness, Metadata)) :-
    atom(ID),
    member(Type, [algorithms, philosophies]),
    number(TargetCount),
    TargetCount >= 0,
    atom(TimeWindow),
    member(Strictness, [strict, adaptive]),
    is_dict(Metadata),
    !.

% ============================================================================
% Work Item Management
% ============================================================================

% work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence)
% Type: algorithm | philosophy
% Origin: filepath or source identifier
% Status: draft | partial | complete | submitted
% Count: wordcount for essays, clause count for algorithms
% Timestamps: dict with created, modified, completed times
% Tags: list of classification tags
% Confidence: 0.0 to 1.0

create_work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence,
                 work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence)) :-
    atom(ID),
    member(Type, [algorithm, philosophy]),
    (atom(Origin); string(Origin)),
    member(Status, [draft, partial, complete, submitted]),
    number(Count),
    Count >= 0,
    is_dict(Timestamps),
    is_list(Tags),
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    !.

validate_work_item(work_item(ID, Type, Origin, Status, Count, Timestamps, Tags, Confidence)) :-
    atom(ID),
    member(Type, [algorithm, philosophy]),
    (atom(Origin); string(Origin)),
    member(Status, [draft, partial, complete, submitted]),
    number(Count),
    Count >= 0,
    is_dict(Timestamps),
    is_list(Tags),
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    !.

% Update work item status
update_work_item_status(work_item(ID, Type, Origin, OldStatus, Count, Timestamps, Tags, Confidence),
                        work_item(ID, Type, Origin, NewStatus, Count, NewTimestamps, Tags, Confidence)) :-
    valid_status_transition(OldStatus, NewStatus),
    get_time(CurrentTime),
    put_dict(last_status_change, Timestamps, CurrentTime, NewTimestamps),
    !.

% ============================================================================
% Status Transitions
% ============================================================================

% Valid status transitions
valid_status_transition(draft, partial) :- !.
valid_status_transition(draft, complete) :- !.
valid_status_transition(partial, complete) :- !.
valid_status_transition(complete, submitted) :- !.

% Get next logical status
next_status(draft, partial).
next_status(partial, complete).
next_status(complete, submitted).

% Check if work item is complete (excludes items that need safety margin)
work_item_is_complete(work_item(_, _, _, Status, _, _, _, _)) :-
    member(Status, [complete, submitted]),
    !.

% ============================================================================
% Schedule Event Management
% ============================================================================

% schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence)
% Start, End: timestamps
% Title: event title
% Location: location string
% Tags: list of tags (travel, sauna, friend_house, home, seminar, etc.)
% Source: ics | gmail | manual
% AttendanceConfidence: 0.0 to 1.0

create_schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence,
                      schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence)) :-
    atom(ID),
    number(Start),
    number(End),
    Start < End,
    (atom(Title); string(Title)),
    (atom(Location); string(Location)),
    is_list(Tags),
    member(Source, [ics, gmail, manual, bridge]),
    number(AttendanceConfidence),
    AttendanceConfidence >= 0.0,
    AttendanceConfidence =< 1.0,
    !.

validate_schedule_event(schedule_event(ID, Start, End, Title, Location, Tags, Source, AttendanceConfidence)) :-
    atom(ID),
    number(Start),
    number(End),
    Start < End,
    (atom(Title); string(Title)),
    (atom(Location); string(Location)),
    is_list(Tags),
    member(Source, [ics, gmail, manual, bridge]),
    number(AttendanceConfidence),
    AttendanceConfidence >= 0.0,
    AttendanceConfidence =< 1.0,
    !.

% ============================================================================
% Time Block Management
% ============================================================================

% time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)
% Start, End: timestamps
% Category: rest | work | play | travel
% FatigueCost: 0.0 to 1.0 (how much fatigue this adds)
% RecoveryCost: 0.0 to 1.0 (how much recovery time needed)
% Confidence: 0.0 to 1.0

create_time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence,
                  time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)) :-
    number(Start),
    number(End),
    Start < End,
    member(Category, [rest, work, play, travel]),
    number(FatigueCost),
    FatigueCost >= 0.0,
    FatigueCost =< 1.0,
    number(RecoveryCost),
    RecoveryCost >= 0.0,
    RecoveryCost =< 1.0,
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    !.

validate_time_block(time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)) :-
    number(Start),
    number(End),
    Start < End,
    member(Category, [rest, work, play, travel]),
    number(FatigueCost),
    FatigueCost >= 0.0,
    FatigueCost =< 1.0,
    number(RecoveryCost),
    RecoveryCost >= 0.0,
    RecoveryCost =< 1.0,
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    !.

% ============================================================================
% Safety Margin Rules (LLM +10% rule)
% ============================================================================

% Apply 10% safety margin to count
apply_safety_margin(Count, AdjustedCount) :-
    number(Count),
    AdjustedCount is Count * 1.1.
