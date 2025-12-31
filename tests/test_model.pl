% test_model.pl - Tests for Spec 2: Unified data model
:- use_module(library(plunit)).
:- use_module('../src/model').
:- use_module('../src/validate').
:- use_module('../src/state').

% ============================================================================
% Goal Tests
% ============================================================================

:- begin_tests(model_goal).

test(create_goal_algorithms, [true(Goal = goal(test_goal1, algorithms, 100, week, strict, _))]) :-
    model:create_goal(test_goal1, algorithms, 100, week, strict, _{priority: high}, Goal).

test(create_goal_philosophies, [true(Goal = goal(test_goal2, philosophies, 7, week, adaptive, _))]) :-
    model:create_goal(test_goal2, philosophies, 7, week, adaptive, _{notes: 'Weekly essays'}, Goal).

test(validate_goal_success) :-
    model:create_goal(test_goal3, algorithms, 80, month, strict, _{}, Goal),
    model:validate_goal(Goal).

test(validate_goal_negative_count, [fail]) :-
    model:create_goal(test_goal4, algorithms, -10, week, strict, _{}, Goal),
    model:validate_goal(Goal).

test(validate_goal_invalid_type, [fail]) :-
    model:create_goal(test_goal5, invalid_type, 100, week, strict, _{}, _).

test(validate_goal_invalid_strictness, [fail]) :-
    model:create_goal(test_goal6, algorithms, 100, week, invalid_strictness, _{}, _).

:- end_tests(model_goal).

% ============================================================================
% Work Item Tests
% ============================================================================

:- begin_tests(model_work_item).

test(create_work_item_algorithm) :-
    get_time(Now),
    Timestamps = _{created: Now, modified: Now},
    model:create_work_item(work1, algorithm, '/test/alg.pl', draft, 50, Timestamps, [test], 0.95, WorkItem),
    WorkItem = work_item(work1, algorithm, '/test/alg.pl', draft, 50, _, [test], 0.95).

test(create_work_item_philosophy) :-
    get_time(Now),
    Timestamps = _{created: Now, modified: Now},
    model:create_work_item(work2, philosophy, '/test/essay.md', partial, 1500, Timestamps, [essay, draft], 0.80, WorkItem),
    WorkItem = work_item(work2, philosophy, '/test/essay.md', partial, 1500, _, [essay, draft], 0.80).

test(validate_work_item_success) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work3, algorithm, '/test/file.pl', complete, 100, Timestamps, [], 1.0, WorkItem),
    model:validate_work_item(WorkItem).

test(validate_work_item_negative_count, [fail]) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work4, algorithm, '/test/file.pl', draft, -10, Timestamps, [], 0.5, WorkItem),
    model:validate_work_item(WorkItem).

test(validate_work_item_invalid_confidence, [fail]) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work5, algorithm, '/test/file.pl', draft, 10, Timestamps, [], 1.5, WorkItem),
    model:validate_work_item(WorkItem).

test(update_work_item_status) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work6, algorithm, '/test/file.pl', draft, 10, Timestamps, [], 0.5, WorkItem1),
    model:update_work_item_status(WorkItem1, WorkItem2),
    WorkItem2 = work_item(work6, algorithm, '/test/file.pl', partial, 10, _, [], 0.5).

test(work_item_is_complete_true) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work7, algorithm, '/test/file.pl', complete, 100, Timestamps, [], 1.0, WorkItem),
    model:work_item_is_complete(WorkItem).

test(work_item_is_complete_false, [fail]) :-
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work8, algorithm, '/test/file.pl', draft, 10, Timestamps, [], 0.5, WorkItem),
    model:work_item_is_complete(WorkItem).

:- end_tests(model_work_item).

% ============================================================================
% Status Transition Tests
% ============================================================================

:- begin_tests(model_status_transitions).

test(valid_transition_draft_to_partial) :-
    model:valid_status_transition(draft, partial).

test(valid_transition_draft_to_complete) :-
    model:valid_status_transition(draft, complete).

test(valid_transition_partial_to_complete) :-
    model:valid_status_transition(partial, complete).

test(valid_transition_complete_to_submitted) :-
    model:valid_status_transition(complete, submitted).

test(invalid_transition_partial_to_draft, [fail]) :-
    model:valid_status_transition(partial, draft).

test(invalid_transition_complete_to_draft, [fail]) :-
    model:valid_status_transition(complete, draft).

test(invalid_transition_submitted_to_complete, [fail]) :-
    model:valid_status_transition(submitted, complete).

test(next_status_draft) :-
    model:next_status(draft, partial).

test(next_status_partial) :-
    model:next_status(partial, complete).

test(next_status_complete) :-
    model:next_status(complete, submitted).

:- end_tests(model_status_transitions).

% ============================================================================
% Schedule Event Tests
% ============================================================================

:- begin_tests(model_schedule_event).

test(create_schedule_event) :-
    get_time(Now),
    End is Now + 3600,
    model:create_schedule_event(event1, Now, End, 'Test Meeting', 'Office', [work], manual, 0.8, Event),
    Event = schedule_event(event1, _, _, 'Test Meeting', 'Office', [work], manual, 0.8).

test(validate_schedule_event_success) :-
    get_time(Now),
    End is Now + 3600,
    model:create_schedule_event(event2, Now, End, 'Sauna Session', 'Spa', [sauna, rest], ics, 0.9, Event),
    model:validate_schedule_event(Event).

test(validate_schedule_event_invalid_time, [fail]) :-
    get_time(Now),
    Start is Now + 3600,
    model:create_schedule_event(event3, Start, Now, 'Invalid Event', 'Place', [], manual, 0.5, Event),
    model:validate_schedule_event(Event).

test(validate_schedule_event_invalid_confidence, [fail]) :-
    get_time(Now),
    End is Now + 3600,
    model:create_schedule_event(event4, Now, End, 'Event', 'Place', [], manual, 2.0, Event),
    model:validate_schedule_event(Event).

test(schedule_event_with_multiple_tags) :-
    get_time(Now),
    End is Now + 7200,
    Tags = [travel, friend_house, seminar],
    model:create_schedule_event(event5, Now, End, 'Seminar at Friend\'s', 'Friend House', Tags, bridge, 0.6, Event),
    Event = schedule_event(event5, _, _, _, _, Tags, bridge, 0.6).

:- end_tests(model_schedule_event).

% ============================================================================
% Time Block Tests
% ============================================================================

:- begin_tests(model_time_block).

test(create_time_block_work) :-
    get_time(Now),
    End is Now + 7200,
    model:create_time_block(Now, End, work, 0.6, 0.3, 0.9, TimeBlock),
    TimeBlock = time_block(_, _, work, 0.6, 0.3, 0.9).

test(create_time_block_rest) :-
    get_time(Now),
    End is Now + 3600,
    model:create_time_block(Now, End, rest, 0.0, 0.8, 1.0, TimeBlock),
    TimeBlock = time_block(_, _, rest, 0.0, 0.8, 1.0).

test(create_time_block_play) :-
    get_time(Now),
    End is Now + 1800,
    model:create_time_block(Now, End, play, 0.2, 0.4, 0.7, TimeBlock),
    TimeBlock = time_block(_, _, play, 0.2, 0.4, 0.7).

test(create_time_block_travel) :-
    get_time(Now),
    End is Now + 900,
    model:create_time_block(Now, End, travel, 0.3, 0.2, 0.85, TimeBlock),
    TimeBlock = time_block(_, _, travel, 0.3, 0.2, 0.85).

test(validate_time_block_success) :-
    get_time(Now),
    End is Now + 3600,
    model:create_time_block(Now, End, work, 0.5, 0.5, 0.5, TimeBlock),
    model:validate_time_block(TimeBlock).

test(validate_time_block_invalid_category, [fail]) :-
    get_time(Now),
    End is Now + 3600,
    model:create_time_block(Now, End, invalid_category, 0.5, 0.5, 0.5, _).

test(validate_time_block_invalid_fatigue, [fail]) :-
    get_time(Now),
    End is Now + 3600,
    model:create_time_block(Now, End, work, 1.5, 0.5, 0.5, TimeBlock),
    model:validate_time_block(TimeBlock).

test(validate_time_block_invalid_time, [fail]) :-
    get_time(Now),
    Start is Now + 3600,
    model:create_time_block(Start, Now, work, 0.5, 0.5, 0.5, TimeBlock),
    model:validate_time_block(TimeBlock).

:- end_tests(model_time_block).

% ============================================================================
% Safety Margin Tests
% ============================================================================

:- begin_tests(model_safety_margin).

test(apply_safety_margin_100, [true(Diff < 0.0001)]) :-
    model:apply_safety_margin(100, Adjusted),
    Diff is abs(Adjusted - 110).

test(apply_safety_margin_50, [true(Diff < 0.0001)]) :-
    model:apply_safety_margin(50, Adjusted),
    Diff is abs(Adjusted - 55).

test(apply_safety_margin_zero, [true(Adjusted =:= 0)]) :-
    model:apply_safety_margin(0, Adjusted).

test(apply_safety_margin_decimal, [true(Diff < 0.0001)]) :-
    model:apply_safety_margin(33.5, Adjusted),
    Expected is 33.5 * 1.1,
    Diff is abs(Adjusted - Expected).

:- end_tests(model_safety_margin).

% ============================================================================
% Validation Tests
% ============================================================================

:- begin_tests(validate_predicates).

test(validate_timestamp_positive) :-
    validate:validate_timestamp(1234567890).

test(validate_timestamp_zero) :-
    validate:validate_timestamp(0).

test(validate_timestamp_negative, [fail]) :-
    validate:validate_timestamp(-1).

test(validate_timestamp_range_valid) :-
    validate:validate_timestamp_range(100, 200).

test(validate_timestamp_range_invalid_order, [fail]) :-
    validate:validate_timestamp_range(200, 100).

test(validate_nonnegative_positive) :-
    validate:validate_nonnegative(10).

test(validate_nonnegative_zero) :-
    validate:validate_nonnegative(0).

test(validate_nonnegative_negative, [fail]) :-
    validate:validate_nonnegative(-5).

test(validate_probability_zero) :-
    validate:validate_probability(0.0).

test(validate_probability_one) :-
    validate:validate_probability(1.0).

test(validate_probability_middle) :-
    validate:validate_probability(0.5).

test(validate_probability_too_high, [fail]) :-
    validate:validate_probability(1.5).

test(validate_probability_negative, [fail]) :-
    validate:validate_probability(-0.1).

test(validate_count_zero) :-
    validate:validate_count(0).

test(validate_count_positive) :-
    validate:validate_count(100).

test(validate_count_negative, [fail]) :-
    validate:validate_count(-10).

test(validate_count_float, [fail]) :-
    validate:validate_count(10.5).

:- end_tests(validate_predicates).

% ============================================================================
% Serialization Tests
% ============================================================================

:- begin_tests(model_serialization).

test(serialize_goal_to_state, [setup(state:clear_state), cleanup(state:clear_state)]) :-
    state:init_state,
    model:create_goal(goal_s1, algorithms, 100, week, strict, _{}, Goal),
    state:add_goal(Goal),
    state:get_goals(Goals),
    length(Goals, 1),
    Goals = [RetrievedGoal],
    RetrievedGoal = goal(goal_s1, algorithms, 100, week, strict, _).

test(serialize_work_item_to_state, [setup(state:clear_state), cleanup(state:clear_state)]) :-
    state:init_state,
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(work_s1, algorithm, '/test.pl', draft, 10, Timestamps, [test], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    state:get_work_items(WorkItems),
    length(WorkItems, 1),
    WorkItems = [RetrievedItem],
    RetrievedItem = work_item(work_s1, algorithm, '/test.pl', draft, 10, _, [test], 0.8).

test(serialize_schedule_event_to_state, [setup(state:clear_state), cleanup(state:clear_state)]) :-
    state:init_state,
    get_time(Now),
    End is Now + 3600,
    model:create_schedule_event(event_s1, Now, End, 'Test', 'Loc', [work], manual, 0.7, Event),
    state:add_schedule_event(Event),
    state:get_schedule_events(Events),
    length(Events, 1),
    Events = [RetrievedEvent],
    RetrievedEvent = schedule_event(event_s1, _, _, 'Test', 'Loc', [work], manual, 0.7).

test(serialize_multiple_entities, [setup(state:clear_state), cleanup(state:clear_state)]) :-
    state:init_state,
    % Add multiple goals
    model:create_goal(g1, algorithms, 80, week, strict, _{}, Goal1),
    model:create_goal(g2, philosophies, 5, week, adaptive, _{}, Goal2),
    state:add_goal(Goal1),
    state:add_goal(Goal2),
    % Add multiple work items
    get_time(Now),
    Timestamps = _{created: Now},
    model:create_work_item(w1, algorithm, '/a.pl', draft, 10, Timestamps, [], 0.8, WorkItem1),
    model:create_work_item(w2, philosophy, '/b.md', complete, 100, Timestamps, [], 0.9, WorkItem2),
    state:add_work_item(WorkItem1),
    state:add_work_item(WorkItem2),
    % Verify counts
    state:get_goals(Goals),
    state:get_work_items(WorkItems),
    length(Goals, 2),
    length(WorkItems, 2).

:- end_tests(model_serialization).
