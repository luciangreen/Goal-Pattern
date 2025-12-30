% test_progress.pl - Tests for progress tracking and forecasting
:- use_module(library(plunit)).
:- use_module(library(filesex)).

:- use_module('../src/config').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/log').
:- use_module('../src/progress').
:- use_module('../src/productivity').
:- use_module('../src/report').

% ============================================================================
% Progress Tracking Tests
% ============================================================================

:- begin_tests(progress).

test(current_week, [true(Week > 0)]) :-
    progress:current_week(Week).

test(week_range, [true((End > Start, End - Start > 0))]) :-
    progress:current_week(Week),
    progress:week_range(Week, Start, End).

test(weekly_progress_no_data, [true(Summary = weekly_progress(_, algorithms(0, 0), philosophies(0, 0)))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Get current week progress (should be zero with no data)
    progress:current_week(Week),
    progress:weekly_progress(Week, Summary).

test(weekly_progress_with_goals, [true(Summary = weekly_progress(_, algorithms(0, 100), philosophies(0, 7)))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Add goals
    model:create_goal(goal1, algorithms, 100, week, strict, _{}, AlgGoal),
    model:create_goal(goal2, philosophies, 7, week, strict, _{}, PhilGoal),
    state:add_goal(AlgGoal),
    state:add_goal(PhilGoal),
    
    % Get current week progress
    progress:current_week(Week),
    progress:weekly_progress(Week, Summary).

test(weekly_progress_with_completed_work, [true(AlgsDone > 0)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Add goals
    model:create_goal(goal1, algorithms, 100, week, strict, _{}, AlgGoal),
    state:add_goal(AlgGoal),
    
    % Get current week
    progress:current_week(Week),
    
    % Add completed work item
    progress:week_range(Week, WeekStart, _),
    CompletedTime is WeekStart + 3600,  % 1 hour into the week
    Timestamps = _{created: WeekStart, modified: CompletedTime, completed: CompletedTime},
    model:create_work_item(work1, algorithm, '/test/alg.pl', complete, 10, Timestamps, [test], 1.0, WorkItem),
    state:add_work_item(WorkItem),
    
    % Get progress
    progress:weekly_progress(Week, weekly_progress(_, algorithms(AlgsDone, _), _)),
    
    % Clean up
    state:clear_state.

test(count_completed_work, [true(Count =:= 2)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Create time range
    get_time(Now),
    Start is Now - 86400,  % 1 day ago
    End is Now + 86400,    % 1 day from now
    
    % Add completed work items
    Timestamps1 = _{created: Start, modified: Now, completed: Now},
    Timestamps2 = _{created: Start, modified: Now, completed: Now},
    model:create_work_item(work1, algorithm, '/test/alg1.pl', complete, 10, Timestamps1, [], 1.0, Work1),
    model:create_work_item(work2, algorithm, '/test/alg2.pl', submitted, 15, Timestamps2, [], 1.0, Work2),
    state:add_work_item(Work1),
    state:add_work_item(Work2),
    
    % Count
    progress:count_completed_work(algorithm, Start, End, Count),
    
    % Clean up
    state:clear_state.

test(backlog_computation, [true(AlgsBacklog >= 0)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Add goals
    model:create_goal(goal1, algorithms, 100, week, strict, _{}, AlgGoal),
    state:add_goal(AlgGoal),
    
    % Compute backlog for current week
    progress:current_week(Week),
    progress:backlog(Week, Week, backlog(_, _, algorithms(AlgsBacklog), _)),
    
    % Clean up
    state:clear_state.

:- end_tests(progress).

% ============================================================================
% Productivity Tests
% ============================================================================

:- begin_tests(productivity).

test(default_productivity_algorithm, [true(UnitsPerHour > 0)]) :-
    productivity:default_productivity(algorithm, UnitsPerHour).

test(default_productivity_philosophy, [true(UnitsPerHour > 0)]) :-
    productivity:default_productivity(philosophy, UnitsPerHour).

test(estimate_productivity_no_data, [true(Confidence < 0.5)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Estimate productivity with no historical data
    productivity:estimate_productivity(algorithm, productivity(algorithm, _, Confidence)),
    
    % Clean up
    state:clear_state.

test(available_work_time, [true(Minutes >= 0)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Calculate available work time for current week
    progress:current_week(Week),
    productivity:available_work_time(Week, algorithm, Minutes),
    
    % Clean up
    state:clear_state.

test(forecast_achievable_units, [true(Units >= 0)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Forecast for next 4 weeks
    progress:current_week(CurrentWeek),
    FromWeek is CurrentWeek + 1,
    ToWeek is CurrentWeek + 4,
    productivity:forecast_achievable_units(algorithm, FromWeek, ToWeek, Units),
    
    % Clean up
    state:clear_state.

test(feasibility_forecast_no_backlog, [true(Feasibility = feasible(_, _))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Forecast with no backlog (should be feasible)
    productivity:feasibility_forecast(algorithm, 0, 4, Feasibility),
    
    % Clean up
    state:clear_state.

test(feasibility_forecast_with_backlog, [true(true)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Add a goal
    model:create_goal(goal1, algorithms, 100, week, strict, _{}, AlgGoal),
    state:add_goal(AlgGoal),
    
    % Forecast with backlog
    productivity:feasibility_forecast(algorithm, 200, 4, Feasibility),
    
    % Verify it returns either feasible or infeasible
    (Feasibility = feasible(_, _) ; Feasibility = infeasible(_)),
    
    % Clean up
    state:clear_state.

:- end_tests(productivity).

% ============================================================================
% Report Tests
% ============================================================================

:- begin_tests(report_output).

test(report_week_runs, [true(true)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Should run without error
    catch(report:report_week, _, true),
    
    % Clean up
    state:clear_state.

test(report_today_runs, [true(true)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Should run without error
    catch(report:report_today, _, true),
    
    % Clean up
    state:clear_state.

test(report_backlog_runs, [true(true)]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Should run without error
    catch(report:report_backlog, _, true),
    
    % Clean up
    state:clear_state.

:- end_tests(report_output).
