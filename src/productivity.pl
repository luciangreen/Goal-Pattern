% productivity.pl - Productivity model and forecasting
:- module(productivity, [
    estimate_productivity/2,
    forecast_achievable_units/4,
    available_work_time/3,
    historical_productivity/3,
    feasibility_forecast/4
]).

:- use_module(state).
:- use_module(model).
:- use_module(progress).
:- use_module(log).

% ============================================================================
% Productivity Estimation
% ============================================================================

% Estimate units per hour for a given work type
% Returns: productivity(Type, UnitsPerHour, Confidence)
estimate_productivity(Type, productivity(Type, UnitsPerHour, Confidence)) :-
    historical_productivity(Type, Samples, TotalHours),
    
    % Calculate average productivity
    (TotalHours > 0 ->
        UnitsPerHour is Samples / TotalHours,
        % Confidence increases with more data
        SampleConfidence is min(1.0, TotalHours / 100.0),
        Confidence = SampleConfidence
    ;
        % Default estimates if no historical data
        default_productivity(Type, UnitsPerHour),
        Confidence = 0.3  % Low confidence without data
    ).

% Default productivity estimates (units per hour)
default_productivity(algorithm, 10.0).  % 10 clauses per hour
default_productivity(philosophy, 0.7).  % 0.7 essays per hour (~80 min per essay)

% Calculate historical productivity from work items
historical_productivity(Type, TotalUnits, TotalHours) :-
    state:get_work_items(Items),
    findall(Units-Hours, 
            work_item_productivity(Type, Items, Units, Hours),
            Samples),
    
    % Sum up all units and hours
    (Samples = [] ->
        TotalUnits = 0, TotalHours = 0
    ;
        findall(U, member(U-_, Samples), UnitsList),
        findall(H, member(_-H, Samples), HoursList),
        sum_list(UnitsList, TotalUnits),
        sum_list(HoursList, TotalHours)
    ).

% Extract productivity data from a work item
work_item_productivity(Type, Items, Units, Hours) :-
    member(work_item(_, Type, _, Status, Count, Timestamps, _, _), Items),
    member(Status, [complete, submitted]),
    
    % Get creation and completion times
    get_dict(created, Timestamps, CreatedStamp),
    (get_dict(completed, Timestamps, CompletedStamp) -> true ;
     get_dict(modified, Timestamps, CompletedStamp)),
    
    % Calculate hours worked (assuming continuous work)
    Hours is (CompletedStamp - CreatedStamp) / 3600,
    Hours > 0,
    
    % Units completed
    Units is Count.

% ============================================================================
% Available Work Time
% ============================================================================

% Calculate available work time in a week (in minutes)
% Considers schedule events and time blocks
available_work_time(Week, _, AvailableMinutes) :-
    progress:week_range(Week, StartStamp, EndStamp),
    
    % Get all schedule events in this week
    state:get_schedule_events(Events),
    include(event_in_range(StartStamp, EndStamp), Events, WeekEvents),
    
    % Calculate total scheduled time
    total_scheduled_minutes(WeekEvents, ScheduledMinutes),
    
    % Total minutes in a week
    WeekMinutes is 7 * 24 * 60,
    
    % Estimate work time as a portion of non-scheduled time
    % Assumptions: 
    % - 8 hours sleep per day = 56 hours/week = 3360 minutes
    % - Personal time, meals etc = 20 hours/week = 1200 minutes
    % - Remaining time is potentially workable
    NonWorkTime is 3360 + 1200 + ScheduledMinutes,
    PotentialWorkMinutes is max(0, WeekMinutes - NonWorkTime),
    
    % Apply context-based efficiency (different contexts have different productivity)
    % For now, assume 60% efficiency
    AvailableMinutes is PotentialWorkMinutes * 0.6.

% Check if an event is in the given range
event_in_range(StartStamp, EndStamp, schedule_event(_, EventStart, EventEnd, _, _, _, _, _)) :-
    EventStart >= StartStamp,
    EventEnd =< EndStamp.

% Calculate total minutes from schedule events
total_scheduled_minutes(Events, TotalMinutes) :-
    findall(Minutes, event_duration(Events, Minutes), MinutesList),
    sum_list(MinutesList, TotalMinutes).

% Get duration of an event in minutes
event_duration(Events, Minutes) :-
    member(schedule_event(_, Start, End, _, _, _, _, _), Events),
    Minutes is (End - Start) / 60.

% ============================================================================
% Forecasting
% ============================================================================

% Forecast achievable units in future weeks
% forecast_achievable_units(Type, FromWeek, ToWeek, AchievableUnits)
forecast_achievable_units(Type, FromWeek, ToWeek, AchievableUnits) :-
    % Get productivity estimate
    estimate_productivity(Type, productivity(Type, UnitsPerHour, _)),
    
    % Calculate total available minutes across weeks
    _ is ToWeek - FromWeek + 1,
    findall(Minutes,
            (between(FromWeek, ToWeek, Week),
             available_work_time(Week, Type, Minutes)),
            AvailableMinutesList),
    sum_list(AvailableMinutesList, TotalMinutes),
    TotalHours is TotalMinutes / 60,
    
    % Calculate achievable units
    AchievableUnits is floor(TotalHours * UnitsPerHour).

% ============================================================================
% Feasibility Forecast
% ============================================================================

% Determine if catch-up is feasible
% feasibility_forecast(Type, CurrentBacklog, Weeks, Feasibility)
% Feasibility: feasible(Week, Confidence) | infeasible(Reason)
feasibility_forecast(Type, CurrentBacklog, FutureWeeks, Feasibility) :-
    current_week(CurrentWeek),
    ToWeek is CurrentWeek + FutureWeeks,
    
    % Forecast achievable units
    forecast_achievable_units(Type, CurrentWeek, ToWeek, AchievableUnits),
    
    % Get ongoing weekly targets
    state:get_goals(Goals),
    (member(goal(_, Type, WeeklyTarget, week, _, _), Goals) -> true ; WeeklyTarget = 0),
    
    % Calculate total needed: backlog + ongoing targets
    OngoingTarget is WeeklyTarget * FutureWeeks,
    TotalNeeded is CurrentBacklog + OngoingTarget,
    
    % Calculate confidence based on achievable vs needed
    (TotalNeeded > 0 ->
        ConfidenceRatio is min(1.0, AchievableUnits / TotalNeeded),
        
        % Get productivity confidence
        estimate_productivity(Type, productivity(Type, _, ProdConfidence)),
        
        % Combined confidence (both achievable AND reliable estimate)
        Confidence is ConfidenceRatio * ProdConfidence,
        
        (AchievableUnits >= TotalNeeded ->
            CatchUpWeek is CurrentWeek + ceiling((CurrentBacklog / max(1, WeeklyTarget))),
            Feasibility = feasible(CatchUpWeek, Confidence)
        ;
            Shortfall is TotalNeeded - AchievableUnits,
            Feasibility = infeasible(shortfall(Shortfall, Confidence))
        )
    ;
        % No backlog, automatically feasible
        Feasibility = feasible(CurrentWeek, 1.0)
    ).
