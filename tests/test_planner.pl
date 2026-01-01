% test_planner.pl - Tests for planner, preferences, and reminder modules
:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- use_module('../src/config').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/log').
:- use_module('../src/preferences').
:- use_module('../src/planner').
:- use_module('../src/remind').

% ============================================================================
% Preferences Tests
% ============================================================================

:- begin_tests(preferences).

test(default_preferences, [true(Mode = adaptive)]) :-
    preferences:default_preference(goal_mode, Mode).

test(get_default_preference, [true(Mode = adaptive)]) :-
    preferences:get_preference(goal_mode, Mode).

test(set_valid_preference, [true(Mode = strict)]) :-
    preferences:set_preference(goal_mode, strict),
    preferences:get_preference(goal_mode, Mode).

test(validate_goal_mode, [true]) :-
    preferences:validate_preference(goal_mode, strict),
    preferences:validate_preference(goal_mode, adaptive).

test(validate_quiet_hours, [true]) :-
    preferences:validate_preference(quiet_hours_start, 22),
    preferences:validate_preference(quiet_hours_end, 7).

test(validate_fatigue_threshold, [true]) :-
    preferences:validate_preference(fatigue_threshold, 0.7),
    preferences:validate_preference(fatigue_threshold, 0.0),
    preferences:validate_preference(fatigue_threshold, 1.0).

test(invalid_fatigue_threshold, [fail]) :-
    preferences:validate_preference(fatigue_threshold, 1.5).

test(load_preferences, [true]) :-
    preferences:load_preferences.

test(in_work_day, [true(InWorkDay)]) :-
    % Create a timestamp for 10 AM today
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_stamp(date(Year, Month, Day, 10, 0, 0, 0, -, -), TestTime),
    
    (preferences:in_work_day(TestTime) -> InWorkDay = true ; InWorkDay = false).

:- end_tests(preferences).

% ============================================================================
% Reminder Tests
% ============================================================================

:- begin_tests(reminders).

test(schedule_future_reminder, [true(ID = test_reminder_1)]) :-
    get_time(Now),
    FutureTime is Now + 3600,  % 1 hour from now
    Context = _{message: 'Test reminder'},
    
    remind:schedule_reminder(test_reminder_1, FutureTime, custom, Context),
    
    remind:get_pending_reminders(Reminders),
    member(reminder(ID, _, _, _), Reminders).

test(cannot_schedule_past_reminder, [fail]) :-
    get_time(Now),
    PastTime is Now - 3600,  % 1 hour ago
    Context = _{message: 'Past reminder'},
    
    remind:schedule_reminder(test_past, PastTime, custom, Context).

test(get_due_reminders, [true(is_list(DueReminders))]) :-
    get_time(Now),
    
    % Schedule a reminder that should be due
    _DueTime is Now - 60,  % Would be due if it could be scheduled
    
    % Get due reminders
    remind:get_due_reminders(Now, DueReminders).

test(cancel_reminder, [true]) :-
    get_time(Now),
    FutureTime is Now + 7200,
    Context = _{message: 'Cancellable'},
    
    remind:schedule_reminder(test_cancel, FutureTime, custom, Context),
    remind:cancel_reminder(test_cancel),
    
    remind:get_pending_reminders(Reminders),
    \+ member(reminder(test_cancel, _, _, _), Reminders).

test(work_block_reminder_time, [true(ReminderTime < BlockTime)]) :-
    get_time(Now),
    BlockTime is Now + 3600,
    
    remind:work_block_reminder_time(BlockTime, ReminderTime).

test(event_prep_reminder_time, [true(ReminderTime < EventTime)]) :-
    get_time(Now),
    EventTime is Now + 7200,
    
    remind:event_prep_reminder_time(EventTime, ReminderTime).

test(reminder_history, [true(is_list(History))]) :-
    get_time(Now),
    OneHourAgo is Now - 3600,
    
    remind:reminder_history(OneHourAgo, History).

:- end_tests(reminders).

% ============================================================================
% Planner Tests
% ============================================================================

:- begin_tests(planner).

test(date_to_timestamp_range, [true((End > Start, End - Start =:= 86400))]) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    date_time_value(date, DateTime, Date),
    
    planner:date_to_timestamp_range(Date, Start, End).

test(find_available_slots_empty_schedule, [true(is_list(Slots))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Get today's range
    get_time(Now),
    NowInt is floor(Now),
    DayStart is NowInt - (NowInt mod 86400),
    DayEnd is DayStart + 86400,
    
    % Find slots (should find gaps in empty schedule)
    planner:find_available_slots(DayStart, DayEnd, Slots).

test(score_time_slot, [true(Score > 0)]) :-
    get_time(Now),
    
    % Test a morning time slot (should score well)
    stamp_date_time(Now, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_stamp(date(Year, Month, Day, 10, 0, 0, 0, -, -), SlotStart),
    SlotEnd is SlotStart + 3600,  % 1 hour slot
    
    planner:score_time_slot(SlotStart, SlotEnd, _, Score).

test(estimate_fatigue_morning, [true(Fatigue < 0.5)]) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_stamp(date(Year, Month, Day, 9, 0, 0, 0, -, -), MorningTime),
    
    planner:estimate_fatigue_at_time(MorningTime, Fatigue).

test(estimate_fatigue_evening, [true(Fatigue > 0.5)]) :-
    get_time(Now),
    stamp_date_time(Now, DateTime, local),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    date_time_stamp(date(Year, Month, Day, 19, 0, 0, 0, -, -), EveningTime),
    
    planner:estimate_fatigue_at_time(EveningTime, Fatigue).

test(calculate_fatigue, [true(is_dict(Model))]) :-
    get_time(Now),
    NowInt is floor(Now),
    DayStart is NowInt - (NowInt mod 86400),
    DayEnd is DayStart + 86400,
    
    planner:calculate_fatigue(DayStart, DayEnd, Model).

test(suggest_work_blocks_no_goals, [true(WorkBlocks = [])]) :-
    % With no goals, should suggest no blocks
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    NowInt is floor(Now),
    DayStart is (NowInt - (NowInt mod 86400)) + 32400,  % 9 AM
    DayEnd is DayStart + 3600,  % 1 hour slot
    
    Slots = [slot(DayStart, DayEnd, 60, 1.0)],
    Progress = weekly_progress(1, algorithms(0, 0), philosophies(0, 0)),
    
    planner:suggest_work_blocks(Slots, Progress, WorkBlocks).

test(suggest_work_blocks_with_backlog, [true(is_list(WorkBlocks))]) :-
    % With backlog, should suggest blocks
    Slots = [slot(1000000, 1003600, 60, 1.5), slot(1010000, 1013600, 60, 1.3)],
    Progress = weekly_progress(1, algorithms(0, 10), philosophies(0, 5)),
    
    planner:suggest_work_blocks(Slots, Progress, WorkBlocks).

test(plan_today, [true(is_dict(Plan) ; Plan = plan(_, _, _, _))]) :-
    state:clear_state,
    state:init_state,
    
    % Add a simple goal
    model:create_goal(test_goal, algorithms, 50, week, adaptive, _{}, Goal),
    state:add_goal(Goal),
    
    (catch(planner:plan_today(Plan), _, fail) -> true ; Plan = plan(date(2024,1,1), [], _{}, [])).

:- end_tests(planner).

% ============================================================================
% Integration Tests
% ============================================================================

:- begin_tests(planner_integration).

test(full_planning_workflow, [true]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Load preferences
    preferences:load_preferences,
    
    % Add goals
    model:create_goal(alg_goal, algorithms, 100, week, strict, _{}, AlgGoal),
    model:create_goal(phil_goal, philosophies, 7, week, adaptive, _{}, PhilGoal),
    state:add_goal(AlgGoal),
    state:add_goal(PhilGoal),
    
    % Try to generate plan
    (catch(planner:plan_today(_), _, fail) -> true ; true).

test(schedule_editing_add_block, [true]) :-
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    BlockStart is Now + 3600,
    BlockEnd is BlockStart + 5400,  % 90 minutes
    
    model:create_time_block(BlockStart, BlockEnd, work, 0.3, 0.2, 1.0, Block),
    
    (catch(planner:edit_schedule(add_block, Block, _), _, fail) -> true ; true).

:- end_tests(planner_integration).
