% test_model.pl - Tests for unified data model
:- begin_tests(model).

:- use_module('../src/model').
:- use_module('../src/validate').
:- use_module('../src/state').
:- use_module('../src/config').

% ============================================================================
% GOAL TESTS
% ============================================================================

test(goal_creation_valid) :-
    % Test creating a valid goal
    create_goal(
        goal1,
        algorithms,
        140,
        _{period: weekly},
        strict,
        _{description: "Weekly algorithm target"},
        Goal
    ),
    assertion(Goal = goal(goal1, algorithms, 140, _, strict, _)),
    !.

test(goal_creation_invalid_type, [fail]) :-
    % Test that invalid type fails
    create_goal(
        goal2,
        invalid_type,
        100,
        _{period: weekly},
        strict,
        _{},
        _Goal
    ).

test(goal_creation_negative_count, [fail]) :-
    % Test that negative count fails
    create_goal(
        goal3,
        algorithms,
        -10,
        _{period: weekly},
        strict,
        _{},
        _Goal
    ).

test(goal_validation) :-
    % Test goal validation
    create_goal(
        goal4,
        philosophies,
        7,
        _{period: weekly},
        adaptive,
        _{description: "Weekly essay target"},
        Goal
    ),
    assertion(validate_goal(Goal)),
    !.

% ============================================================================
% WORK ITEM TESTS
% ============================================================================

test(work_item_creation_valid) :-
    % Test creating a valid work item
    create_work_item(
        work1,
        algorithm_clause,
        '/path/to/file.pl',
        complete,
        15,
        _{created: "2025-01-01T10:00:00", modified: "2025-01-01T12:00:00"},
        [home, work],
        0.95,
        WorkItem
    ),
    assertion(WorkItem = work_item(work1, algorithm_clause, _, complete, 15, _, _, 0.95)),
    !.

test(work_item_status_transitions) :-
    % Test valid status transitions
    assertion(status_transition(draft, partial)),
    assertion(status_transition(partial, complete)),
    assertion(status_transition(complete, submitted)),
    assertion(status_transition(draft, complete)),
    !.

test(work_item_invalid_status, [fail]) :-
    % Test that invalid status fails
    create_work_item(
        work2,
        algorithm_clause,
        '/path/to/file.pl',
        invalid_status,
        10,
        _{created: "2025-01-01T10:00:00"},
        [],
        0.9,
        _WorkItem
    ).

test(work_item_validation) :-
    % Test work item validation
    create_work_item(
        work3,
        philosophy_essay,
        '/essays/essay1.md',
        partial,
        500,
        _{created: "2025-01-01T09:00:00", modified: "2025-01-01T11:00:00"},
        [home],
        0.85,
        WorkItem
    ),
    assertion(validate_work_item(WorkItem)),
    !.

test(work_item_negative_count, [fail]) :-
    % Test that negative count fails validation
    WorkItem = work_item(work4, algorithm_clause, '/path.pl', draft, -5, _{}, [], 0.9),
    validate_work_item(WorkItem).

% ============================================================================
% SCORING TESTS
% ============================================================================

test(safety_margin_application) :-
    % Test 10% safety margin
    apply_safety_margin(100, Adjusted),
    assertion(Adjusted =:= 90),
    !.

test(work_item_score_complete) :-
    % Test scoring for complete item without safety margin
    create_work_item(
        work5,
        algorithm_clause,
        '/path.pl',
        complete,
        100,
        _{created: "2025-01-01T10:00:00"},
        [],
        1.0,
        WorkItem
    ),
    work_item_score(WorkItem, false, Score),
    assertion(Score =:= 100),
    !.

test(work_item_score_complete_with_margin) :-
    % Test scoring for complete item with safety margin
    create_work_item(
        work6,
        algorithm_clause,
        '/path.pl',
        complete,
        100,
        _{created: "2025-01-01T10:00:00"},
        [],
        1.0,
        WorkItem
    ),
    work_item_score(WorkItem, true, Score),
    assertion(Score =:= 90),
    !.

test(work_item_score_partial) :-
    % Test scoring for partial item (50% of value)
    create_work_item(
        work7,
        algorithm_clause,
        '/path.pl',
        partial,
        100,
        _{created: "2025-01-01T10:00:00"},
        [],
        1.0,
        WorkItem
    ),
    work_item_score(WorkItem, false, Score),
    assertion(Score =:= 50),
    !.

test(work_item_score_draft) :-
    % Test scoring for draft (should be 0)
    create_work_item(
        work8,
        algorithm_clause,
        '/path.pl',
        draft,
        100,
        _{created: "2025-01-01T10:00:00"},
        [],
        1.0,
        WorkItem
    ),
    work_item_score(WorkItem, false, Score),
    assertion(Score =:= 0),
    !.

test(work_item_score_with_confidence) :-
    % Test that confidence affects score
    create_work_item(
        work9,
        algorithm_clause,
        '/path.pl',
        complete,
        100,
        _{created: "2025-01-01T10:00:00"},
        [],
        0.8,
        WorkItem
    ),
    work_item_score(WorkItem, false, Score),
    assertion(Score =:= 80),
    !.

% ============================================================================
% SCHEDULE EVENT TESTS
% ============================================================================

test(schedule_event_creation) :-
    % Test creating a valid schedule event
    create_schedule_event(
        event1,
        "2025-01-01T09:00:00",
        "2025-01-01T10:00:00",
        "Morning workout",
        "Home",
        [home, exercise],
        calendar_ics,
        0.95,
        Event
    ),
    assertion(Event = schedule_event(event1, _, _, "Morning workout", "Home", _, calendar_ics, 0.95)),
    !.

test(schedule_event_validation) :-
    % Test schedule event validation
    create_schedule_event(
        event2,
        "2025-01-01T14:00:00",
        "2025-01-01T16:00:00",
        "Afternoon work",
        "Office",
        [work],
        calendar_ics,
        1.0,
        Event
    ),
    assertion(validate_schedule_event(Event)),
    !.

test(schedule_event_invalid_confidence, [fail]) :-
    % Test that invalid confidence fails
    create_schedule_event(
        event3,
        "2025-01-01T09:00:00",
        "2025-01-01T10:00:00",
        "Event",
        "Location",
        [],
        source,
        1.5,  % Invalid confidence > 1.0
        _Event
    ).

% ============================================================================
% TIME BLOCK TESTS
% ============================================================================

test(time_block_creation) :-
    % Test creating a valid time block
    create_time_block(
        "2025-01-01T14:00:00",
        "2025-01-01T16:00:00",
        category(work),
        0.7,
        0.3,
        0.9,
        Block
    ),
    assertion(Block = time_block(_, _, category(work), 0.7, 0.3, 0.9)),
    !.

test(time_block_validation) :-
    % Test time block validation
    create_time_block(
        "2025-01-01T08:00:00",
        "2025-01-01T09:00:00",
        category(rest),
        0.1,
        0.8,
        0.95,
        Block
    ),
    assertion(validate_time_block(Block)),
    !.

test(time_block_categories) :-
    % Test all valid categories
    assertion(time_block_category(category(rest))),
    assertion(time_block_category(category(work))),
    assertion(time_block_category(category(play))),
    assertion(time_block_category(category(travel))),
    !.

test(time_block_invalid_category, [fail]) :-
    % Test that invalid category fails
    time_block_category(category(invalid)).

% ============================================================================
% VALIDATION TESTS
% ============================================================================

test(validate_timestamp_iso8601) :-
    % Test ISO 8601 timestamp validation
    assertion(validate_timestamp("2025-01-01T10:00:00")),
    assertion(validate_timestamp("2025-12-31")),
    !.

test(validate_timestamp_dict) :-
    % Test dict timestamp validation
    assertion(validate_timestamp(_{year: 2025, month: 1, day: 1})),
    !.

test(validate_timestamp_null) :-
    % Test null timestamp validation
    assertion(validate_timestamp(null)),
    !.

test(validate_time_range_valid) :-
    % Test valid time range
    assertion(validate_time_range("2025-01-01T09:00:00", "2025-01-01T10:00:00")),
    !.

test(validate_nonnegative_valid) :-
    % Test non-negative validation
    assertion(validate_nonnegative(0)),
    assertion(validate_nonnegative(100)),
    assertion(validate_nonnegative(0.5)),
    !.

test(validate_nonnegative_invalid, [fail]) :-
    % Test that negative fails validation
    validate_nonnegative(-1).

test(validate_probability_valid) :-
    % Test probability validation
    assertion(validate_probability(0.0)),
    assertion(validate_probability(0.5)),
    assertion(validate_probability(1.0)),
    !.

test(validate_probability_invalid, [fail]) :-
    % Test that value > 1.0 fails validation
    validate_probability(1.5).

% ============================================================================
% STATE SERIALIZATION TESTS
% ============================================================================

test(goal_serialization_roundtrip) :-
    % Test goal serialization and deserialization
    create_goal(
        goal10,
        algorithms,
        140,
        _{period: weekly},
        strict,
        _{description: "Test goal"},
        Goal
    ),
    goal_to_dict(Goal, Dict),
    dict_to_goal(Dict, RestoredGoal),
    assertion(Goal = RestoredGoal),
    !.

test(work_item_serialization_roundtrip) :-
    % Test work item serialization and deserialization
    create_work_item(
        work10,
        algorithm_clause,
        '/path.pl',
        complete,
        50,
        _{created: "2025-01-01T10:00:00"},
        [home],
        0.9,
        WorkItem
    ),
    work_item_to_dict(WorkItem, Dict),
    dict_to_work_item(Dict, RestoredWorkItem),
    assertion(WorkItem = RestoredWorkItem),
    !.

test(schedule_event_serialization_roundtrip) :-
    % Test schedule event serialization and deserialization
    create_schedule_event(
        event10,
        "2025-01-01T09:00:00",
        "2025-01-01T10:00:00",
        "Test event",
        "Home",
        [home],
        manual,
        0.9,
        Event
    ),
    schedule_event_to_dict(Event, Dict),
    dict_to_schedule_event(Dict, RestoredEvent),
    assertion(Event = RestoredEvent),
    !.

test(time_block_serialization_roundtrip) :-
    % Test time block serialization and deserialization
    create_time_block(
        "2025-01-01T14:00:00",
        "2025-01-01T16:00:00",
        category(work),
        0.7,
        0.3,
        0.9,
        Block
    ),
    time_block_to_dict(Block, Dict),
    dict_to_time_block(Dict, RestoredBlock),
    assertion(Block = RestoredBlock),
    !.

test(state_entity_storage) :-
    % Test storing entities in state
    config_path(ConfigPath),
    load_config(ConfigPath),
    init_state,
    
    % Create and add a goal
    create_goal(
        goal11,
        algorithms,
        100,
        _{period: weekly},
        strict,
        _{},
        Goal
    ),
    add_goal(Goal),
    
    % Create and add a work item
    create_work_item(
        work11,
        algorithm_clause,
        '/path.pl',
        complete,
        25,
        _{created: "2025-01-01T10:00:00"},
        [],
        0.9,
        WorkItem
    ),
    add_work_item(WorkItem),
    
    % Retrieve and verify
    get_goals(Goals),
    get_work_items(WorkItems),
    assertion(length(Goals, 1)),
    assertion(length(WorkItems, 1)),
    !.

test(state_serialization_with_entities) :-
    % Test full state serialization with entities
    config_path(ConfigPath),
    load_config(ConfigPath),
    init_state,
    
    % Add entities
    create_goal(goal12, algorithms, 140, _{period: weekly}, strict, _{}, Goal),
    add_goal(Goal),
    
    create_work_item(work12, algorithm_clause, '/path.pl', complete, 30, _{created: "2025-01-01T10:00:00"}, [], 0.9, WorkItem),
    add_work_item(WorkItem),
    
    create_schedule_event(event12, "2025-01-01T09:00:00", "2025-01-01T10:00:00", "Event", "Location", [], manual, 0.9, Event),
    add_schedule_event(Event),
    
    create_time_block("2025-01-01T14:00:00", "2025-01-01T16:00:00", category(work), 0.7, 0.3, 0.9, Block),
    add_time_block(Block),
    
    % Save state
    save_state,
    
    % Clear and reload
    clear_state,
    load_state,
    
    % Verify entities are restored
    get_goals(RestoredGoals),
    get_work_items(RestoredWorkItems),
    get_schedule_events(RestoredEvents),
    get_time_blocks(RestoredBlocks),
    
    assertion(length(RestoredGoals, 1)),
    assertion(length(RestoredWorkItems, 1)),
    assertion(length(RestoredEvents, 1)),
    assertion(length(RestoredBlocks, 1)),
    !.

:- end_tests(model).
