% progress.pl - Work tallying vs goals + backlog tracking
:- module(progress, [
    weekly_progress/2,
    backlog/3,
    current_week/1,
    week_range/3,
    count_completed_work/4,
    compute_backlog/4
]).

:- use_module(state).
:- use_module(model).
:- use_module(log).

% ============================================================================
% Week Utilities
% ============================================================================

% Get current week number (ISO 8601 week numbering)
current_week(Week) :-
    get_time(Timestamp),
    stamp_date_time(Timestamp, DateTime, 'UTC'),
    date_time_value(date, DateTime, Date),
    day_of_the_week(Date, _DayNum),
    date_time_value(year, DateTime, Year),
    date_time_value(month, DateTime, Month),
    date_time_value(day, DateTime, Day),
    
    % Calculate ISO week number
    % ISO weeks start on Monday and week 1 contains the first Thursday
    calculate_iso_week(Year, Month, Day, Week).

% Calculate ISO 8601 week number
calculate_iso_week(Year, Month, Day, Week) :-
    % Create a date for Jan 4th of this year (always in week 1)
    date_time_stamp(date(Year, 1, 4, 0, 0, 0, 0, -, -), Jan4Stamp),
    stamp_date_time(Jan4Stamp, Jan4DateTime, 'UTC'),
    date_time_value(date, Jan4DateTime, Jan4Date),
    day_of_the_week(Jan4Date, Jan4DayNum),
    
    % Find Monday of week 1
    MondayOffset is 1 - Jan4DayNum,
    Week1MondayDay is 4 + MondayOffset,
    date_time_stamp(date(Year, 1, Week1MondayDay, 0, 0, 0, 0, -, -), Week1Stamp),
    
    % Create timestamp for the target date
    date_time_stamp(date(Year, Month, Day, 0, 0, 0, 0, -, -), TargetStamp),
    
    % Calculate week number
    DaysDiff is (TargetStamp - Week1Stamp) / 86400,
    Week is floor(DaysDiff / 7) + 1.

% Get timestamp range for a given week
week_range(Week, StartStamp, EndStamp) :-
    get_time(CurrentStamp),
    stamp_date_time(CurrentStamp, CurrentDateTime, 'UTC'),
    date_time_value(year, CurrentDateTime, Year),
    
    % Calculate the Monday of the given week
    date_time_stamp(date(Year, 1, 4, 0, 0, 0, 0, -, -), Jan4Stamp),
    stamp_date_time(Jan4Stamp, Jan4DateTime, 'UTC'),
    date_time_value(date, Jan4DateTime, Jan4Date),
    day_of_the_week(Jan4Date, Jan4DayNum),
    
    MondayOffset is 1 - Jan4DayNum,
    Week1MondayDay is 4 + MondayOffset,
    date_time_stamp(date(Year, 1, Week1MondayDay, 0, 0, 0, 0, -, -), Week1Stamp),
    
    % Calculate start of target week (Monday 00:00:00)
    WeekOffset is (Week - 1) * 7,
    StartStamp is Week1Stamp + (WeekOffset * 86400),
    
    % Calculate end of target week (Sunday 23:59:59)
    EndStamp is StartStamp + (7 * 86400) - 1.

% ============================================================================
% Work Counting
% ============================================================================

% Count completed work items in a time range for a specific type
count_completed_work(Type, StartStamp, EndStamp, Count) :-
    state:get_work_items(Items),
    include(is_completed_in_range(Type, StartStamp, EndStamp), Items, CompletedItems),
    length(CompletedItems, Count).

% Check if a work item is completed in the given range
is_completed_in_range(Type, StartStamp, EndStamp, work_item(_, ItemType, _, Status, _, Timestamps, _, _)) :-
    ItemType = Type,
    member(Status, [complete, submitted]),
    (get_dict(completed, Timestamps, CompletedStamp) -> true ; 
     get_dict(modified, Timestamps, CompletedStamp)),
    CompletedStamp >= StartStamp,
    CompletedStamp =< EndStamp.

% ============================================================================
% Weekly Progress
% ============================================================================

% Compute weekly progress for a given week
% Returns: weekly_progress(Week, algorithms(Done/Target), philosophies(Done/Target))
weekly_progress(Week, Summary) :-
    week_range(Week, StartStamp, EndStamp),
    
    % Count completed algorithms
    count_completed_work(algorithm, StartStamp, EndStamp, AlgsDone),
    
    % Count completed philosophies
    count_completed_work(philosophy, StartStamp, EndStamp, PhilsDone),
    
    % Get goals for this week
    state:get_goals(Goals),
    (member(goal(_, algorithms, AlgsTarget, week, _, _), Goals) -> true ; AlgsTarget = 0),
    (member(goal(_, philosophies, PhilsTarget, week, _, _), Goals) -> true ; PhilsTarget = 0),
    
    % Create summary
    Summary = weekly_progress(Week, 
                            algorithms(AlgsDone, AlgsTarget),
                            philosophies(PhilsDone, PhilsTarget)).

% ============================================================================
% Backlog Computation
% ============================================================================

% Compute backlog from week FromWeek to ToWeek
% Returns: backlog(FromWeek, ToWeek, algorithms(Backlog), philosophies(Backlog))
backlog(FromWeek, ToWeek, BacklogSummary) :-
    compute_backlog(FromWeek, ToWeek, AlgsBacklog, PhilsBacklog),
    BacklogSummary = backlog(FromWeek, ToWeek, 
                           algorithms(AlgsBacklog),
                           philosophies(PhilsBacklog)).

% Compute backlog for each type
compute_backlog(FromWeek, ToWeek, AlgsBacklog, PhilsBacklog) :-
    compute_backlog_for_type(algorithms, FromWeek, ToWeek, AlgsBacklog),
    compute_backlog_for_type(philosophies, FromWeek, ToWeek, PhilsBacklog).

% Compute backlog for a specific type
compute_backlog_for_type(Type, FromWeek, ToWeek, TotalBacklog) :-
    findall(Backlog, 
            week_backlog(Type, FromWeek, ToWeek, Backlog), 
            Backlogs),
    sum_list(Backlogs, TotalBacklog).

% Calculate backlog for a single week
week_backlog(Type, FromWeek, ToWeek, Backlog) :-
    between(FromWeek, ToWeek, Week),
    weekly_progress(Week, weekly_progress(Week, AlgsProgress, PhilsProgress)),
    
    % Extract appropriate progress
    (Type = algorithms -> 
        AlgsProgress = algorithms(Done, Target) ;
        PhilsProgress = philosophies(Done, Target)
    ),
    
    % Backlog is target minus done (capped at 0 for weeks with surplus)
    Backlog is max(0, Target - Done).
