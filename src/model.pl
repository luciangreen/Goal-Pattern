% model.pl - Unified data model for goals, work artifacts, schedule, events, and patterns
:- module(model, [
    % Goal predicates
    goal/6,
    create_goal/7,
    goal_type/1,
    goal_strictness/1,
    
    % Work item predicates
    work_item/8,
    create_work_item/9,
    work_item_type/1,
    work_item_status/1,
    status_transition/2,
    is_complete_status/1,
    
    % Schedule event predicates
    schedule_event/8,
    create_schedule_event/9,
    
    % Time block predicates
    time_block/6,
    create_time_block/7,
    time_block_category/1,
    
    % Scoring predicates
    work_item_score/3,
    apply_safety_margin/2,
    is_done_with_margin/1
]).

:- dynamic goal/6.
:- dynamic work_item/8.
:- dynamic schedule_event/8.
:- dynamic time_block/6.

% ============================================================================
% GOAL MODEL
% ============================================================================
% goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata)
%
% Id: unique identifier (atom or integer)
% Type: 'algorithms' or 'philosophies'
% TargetCount: integer, number of units expected
% TimeWindow: dict with start/end dates or duration (e.g., 'weekly', 'daily')
% Strictness: 'strict' | 'adaptive' | 'flexible'
% Metadata: dict with additional properties
%
% Example:
% goal(g1, algorithms, 140, _{period: weekly}, strict, _{description: "Weekly algorithm target"})

% Valid goal types
goal_type(algorithms).
goal_type(philosophies).

% Valid strictness levels
goal_strictness(strict).
goal_strictness(adaptive).
goal_strictness(flexible).

% Create a new goal
create_goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata, Goal) :-
    goal_type(Type),
    goal_strictness(Strictness),
    integer(TargetCount),
    TargetCount > 0,
    Goal = goal(Id, Type, TargetCount, TimeWindow, Strictness, Metadata).

% ============================================================================
% WORK ITEM MODEL
% ============================================================================
% work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence)
%
% Id: unique identifier
% Type: 'algorithm_clause' | 'philosophy_essay' | 'draft' | 'spec'
% Origin: filepath or source identifier (email, etc.)
% Status: 'draft' | 'partial' | 'complete' | 'submitted'
% Count: wordcount for essays, clause count for algorithms
% Timestamps: dict with created/modified/completed times
% Tags: list of context tags (home, friend_house, sauna, etc.)
% Confidence: float 0.0-1.0 indicating detection confidence
%
% Example:
% work_item(w1, algorithm_clause, '/path/to/file.pl', complete, 15, 
%           _{created: "2025-01-01T10:00:00", modified: "2025-01-01T12:00:00"},
%           [home, work], 0.95)

% Valid work item types
work_item_type(algorithm_clause).
work_item_type(philosophy_essay).
work_item_type(draft).
work_item_type(spec).
work_item_type(note).

% Valid statuses
work_item_status(draft).
work_item_status(partial).
work_item_status(complete).
work_item_status(submitted).

% Status transitions (valid state changes)
status_transition(draft, partial).
status_transition(partial, complete).
status_transition(complete, submitted).
status_transition(draft, complete).  % Direct completion possible

% Check if status is complete
is_complete_status(complete).
is_complete_status(submitted).

% Create a new work item
create_work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence, WorkItem) :-
    work_item_type(Type),
    work_item_status(Status),
    integer(Count),
    Count >= 0,
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    is_list(Tags),
    WorkItem = work_item(Id, Type, Origin, Status, Count, Timestamps, Tags, Confidence).

% ============================================================================
% SCHEDULE EVENT MODEL
% ============================================================================
% schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence)
%
% Id: unique identifier
% Start: timestamp (ISO 8601 or timestamp dict)
% End: timestamp
% Title: event title/summary
% Location: location string or dict
% Tags: list of tags (travel, sauna, friend_house, seminar, etc.)
% Source: source of event (calendar, email, manual)
% AttendanceConfidence: float 0.0-1.0 (likelihood of attendance)
%
% Example:
% schedule_event(e1, "2025-01-01T09:00:00", "2025-01-01T10:00:00",
%                "Morning workout", "Home", [home, exercise], calendar_ics, 0.95)

% Create a new schedule event
create_schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence, Event) :-
    is_list(Tags),
    number(AttendanceConfidence),
    AttendanceConfidence >= 0.0,
    AttendanceConfidence =< 1.0,
    Event = schedule_event(Id, Start, End, Title, Location, Tags, Source, AttendanceConfidence).

% ============================================================================
% TIME BLOCK MODEL
% ============================================================================
% time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence)
%
% Start: timestamp
% End: timestamp
% Category: category(Type) where Type is rest|work|play|travel
% FatigueCost: float representing energy expenditure
% RecoveryCost: float representing time needed to recover
% Confidence: float 0.0-1.0
%
% Example:
% time_block("2025-01-01T14:00:00", "2025-01-01T16:00:00",
%            category(work), 0.7, 0.3, 0.9)

% Valid time block categories
time_block_category(category(rest)).
time_block_category(category(work)).
time_block_category(category(play)).
time_block_category(category(travel)).

% Create a new time block
create_time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence, Block) :-
    time_block_category(Category),
    number(FatigueCost),
    FatigueCost >= 0.0,
    number(RecoveryCost),
    RecoveryCost >= 0.0,
    number(Confidence),
    Confidence >= 0.0,
    Confidence =< 1.0,
    Block = time_block(Start, End, Category, FatigueCost, RecoveryCost, Confidence).

% ============================================================================
% SCORING PREDICATES
% ============================================================================

% Apply 10% safety margin reduction to a count (for LLM-generated work)
% Safety margin ensures we don't over-count work that might be mistaken
% by reducing the count by 10%
apply_safety_margin(Count, AdjustedCount) :-
    number(Count),
    AdjustedCount is Count * 0.9.  % Reduce by 10% for safety

% Calculate work item score (considering safety margin for certain sources)
% work_item_score(+WorkItem, +UseSafetyMargin, -Score)
work_item_score(work_item(_, _, _, Status, Count, _, _, Confidence), UseSafetyMargin, Score) :-
    is_complete_status(Status),
    !,
    (UseSafetyMargin = true ->
        apply_safety_margin(Count, AdjustedCount)
    ;
        AdjustedCount = Count
    ),
    Score is AdjustedCount * Confidence.

work_item_score(work_item(_, _, _, partial, Count, _, _, Confidence), UseSafetyMargin, Score) :-
    !,
    (UseSafetyMargin = true ->
        apply_safety_margin(Count, AdjustedCount)
    ;
        AdjustedCount = Count
    ),
    % Partial work counts for 50% of its value
    Score is AdjustedCount * Confidence * 0.5.

work_item_score(work_item(_, _, _, draft, _, _, _, _), _, 0) :-
    % Drafts don't count toward completion
    !.

% Check if work item is done with safety margin applied
is_done_with_margin(WorkItem) :-
    work_item_score(WorkItem, true, Score),
    Score > 0.
