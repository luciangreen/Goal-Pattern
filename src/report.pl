% report.pl - Reporting and display functionality
:- module(report, [
    report_today/0,
    report_week/0,
    report_week/1,
    report_backlog/0,
    report_backlog/2,
    print_progress_summary/1,
    print_backlog_summary/1,
    print_feasibility/2
]).

:- use_module(progress).
:- use_module(productivity).
:- use_module(state).
:- use_module(log).

% ============================================================================
% Report Generation
% ============================================================================

% Report today's progress (current week)
report_today :-
    report_week.

% Report current week's progress
report_week :-
    current_week(Week),
    report_week(Week).

% Report specific week's progress
report_week(Week) :-
    format('~n=== Weekly Progress Report: Week ~w ===~n~n', [Week]),
    
    % Get weekly progress
    weekly_progress(Week, Summary),
    print_progress_summary(Summary),
    
    format('~n').

% Report backlog from a past week to current
report_backlog :-
    current_week(CurrentWeek),
    % Look back 4 weeks
    FromWeek is CurrentWeek - 4,
    report_backlog(FromWeek, CurrentWeek).

% Report backlog for a specific range
report_backlog(FromWeek, ToWeek) :-
    format('~n=== Backlog Report: Week ~w to Week ~w ===~n~n', [FromWeek, ToWeek]),
    
    % Get backlog
    backlog(FromWeek, ToWeek, BacklogSummary),
    print_backlog_summary(BacklogSummary),
    
    % Show feasibility for catching up
    BacklogSummary = backlog(_, _, algorithms(AlgsBacklog), philosophies(PhilsBacklog)),
    
    format('~n--- Catch-up Feasibility (next 4 weeks) ---~n', []),
    feasibility_forecast(algorithm, AlgsBacklog, 4, AlgsFeasibility),
    format('Algorithms: ', []),
    print_feasibility(algorithm, AlgsFeasibility),
    
    feasibility_forecast(philosophy, PhilsBacklog, 4, PhilsFeasibility),
    format('Philosophies: ', []),
    print_feasibility(philosophy, PhilsFeasibility),
    
    format('~n'),
    !.

% ============================================================================
% Printing Helpers
% ============================================================================

% Print weekly progress summary
print_progress_summary(weekly_progress(Week, AlgsProgress, PhilsProgress)) :-
    format('Week ~w Progress:~n', [Week]),
    
    % Algorithms
    AlgsProgress = algorithms(AlgsDone, AlgsTarget),
    (AlgsTarget > 0 -> AlgsPercent is (AlgsDone / AlgsTarget) * 100 ; AlgsPercent = 0),
    format('  Algorithms: ~w / ~w (~1f%)~n', [AlgsDone, AlgsTarget, AlgsPercent]),
    print_status_bar(AlgsDone, AlgsTarget),
    
    % Philosophies
    PhilsProgress = philosophies(PhilsDone, PhilsTarget),
    (PhilsTarget > 0 -> PhilsPercent is (PhilsDone / PhilsTarget) * 100 ; PhilsPercent = 0),
    format('  Philosophies: ~w / ~w (~1f%)~n', [PhilsDone, PhilsTarget, PhilsPercent]),
    print_status_bar(PhilsDone, PhilsTarget).

% Print backlog summary
print_backlog_summary(backlog(FromWeek, ToWeek, AlgsBacklog, PhilsBacklog)) :-
    format('Backlog from Week ~w to Week ~w:~n', [FromWeek, ToWeek]),
    
    % Algorithms
    AlgsBacklog = algorithms(AlgsTotal),
    format('  Algorithms backlog: ~w units~n', [AlgsTotal]),
    
    % Philosophies
    PhilsBacklog = philosophies(PhilsTotal),
    format('  Philosophies backlog: ~w units~n', [PhilsTotal]).

% Print feasibility forecast
print_feasibility(Type, feasible(CatchUpWeek, Confidence)) :-
    ConfidencePercent is Confidence * 100,
    format('Feasible - can catch up by week ~w (confidence: ~1f%)~n', 
           [CatchUpWeek, ConfidencePercent]),
    
    % Show additional details
    current_week(CurrentWeek),
    forecast_achievable_units(Type, CurrentWeek, CatchUpWeek, Achievable),
    estimate_productivity(Type, productivity(Type, UnitsPerHour, _)),
    format('  - Predicted achievable units: ~w~n', [Achievable]),
    format('  - Estimated productivity: ~2f units/hour~n', [UnitsPerHour]).

print_feasibility(Type, infeasible(shortfall(Shortfall, Confidence))) :-
    ConfidencePercent is Confidence * 100,
    format('Infeasible - shortfall of ~w units (confidence: ~1f%)~n', 
           [Shortfall, ConfidencePercent]),
    
    % Show additional details
    estimate_productivity(Type, productivity(Type, UnitsPerHour, _)),
    ExtraHoursNeeded is Shortfall / UnitsPerHour,
    format('  - Additional hours needed: ~1f~n', [ExtraHoursNeeded]),
    format('  - Estimated productivity: ~2f units/hour~n', [UnitsPerHour]).

% Print a simple status bar
print_status_bar(Done, Target) :-
    (Target > 0 ->
        Percent is (Done / Target) * 100,
        BarWidth = 40,
        FilledWidth is floor((Done / Target) * BarWidth),
        EmptyWidth is BarWidth - FilledWidth,
        
        format('  [', []),
        print_n_chars('=', FilledWidth),
        print_n_chars(' ', EmptyWidth),
        format('] ~1f%~n', [Percent])
    ;
        format('  [No target set]~n', [])
    ).

% Print N characters
print_n_chars(_, 0) :- !.
print_n_chars(Char, N) :-
    N > 0,
    format('~w', [Char]),
    N1 is N - 1,
    print_n_chars(Char, N1).
