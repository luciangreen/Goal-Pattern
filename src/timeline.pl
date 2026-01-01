% timeline.pl - Minute-level timeline generation from schedule events
:- module(timeline, [
    build_timeline/3,
    timeline_at_minute/3,
    get_minute_categories/4,
    get_context_tags_at_minute/3,
    work_outcome_windows/3,
    event_to_minutes/2
]).

:- use_module(state).
:- use_module(model).
:- use_module(log).

% ============================================================================
% Timeline Building
% ============================================================================

% Build a minute-level timeline from schedule events
% build_timeline(StartStamp, EndStamp, Timeline)
% Timeline is a list of timeline_minute(Timestamp, Categories, Tags, AttendanceConfidence)
build_timeline(StartStamp, EndStamp, Timeline) :-
    state:get_schedule_events(Events),
    (Events = [] -> 
        Timeline = []
    ;
        StartMinute is floor(StartStamp / 60),
        EndMinute is floor(EndStamp / 60),
        build_timeline_minutes(StartMinute, EndMinute, Events, Timeline)
    ).

% Build timeline for each minute in range
build_timeline_minutes(StartMinute, EndMinute, Events, Timeline) :-
    findall(MinuteData,
            (between(StartMinute, EndMinute, Minute),
             minute_data(Minute, Events, MinuteData)),
            Timeline).

% Get data for a specific minute
minute_data(Minute, _, timeline_minute(Timestamp, Categories, Tags, MaxConfidence)) :-
    Timestamp is Minute * 60,
    % Find all events that overlap this minute
    findall(Event, 
            event_overlaps_minute(Event, Minute), 
            OverlappingEvents),
    
    % Extract categories and tags from overlapping events
    extract_categories(OverlappingEvents, Categories),
    extract_tags(OverlappingEvents, Tags),
    
    % Get maximum attendance confidence
    (OverlappingEvents = [] -> 
        MaxConfidence = 1.0 
    ; 
        findall(Conf, 
                member(schedule_event(_, _, _, _, _, _, _, Conf), OverlappingEvents),
                Confidences),
        max_list(Confidences, MaxConfidence)
    ).

% Check if event overlaps a specific minute
event_overlaps_minute(Event, Minute) :-
    state:get_schedule_events(Events),
    member(Event, Events),
    Event = schedule_event(_, Start, End, _, _, _, _, _),
    StartMinute is floor(Start / 60),
    EndMinute is floor(End / 60),
    Minute >= StartMinute,
    Minute =< EndMinute.

% Extract unique categories from events
extract_categories([], []).
extract_categories(Events, Categories) :-
    Events \= [],
    findall(Category,
            (member(schedule_event(_, _, _, _, _, Tags, _, _), Events),
             member(Tag, Tags),
             category_from_tag(Tag, Category)),
            AllCategories),
    list_to_set(AllCategories, Categories).

% Map tags to categories
category_from_tag(travel, travel).
category_from_tag(sauna, rest).
category_from_tag(friend_house, play).
category_from_tag(home, work).
category_from_tag(seminar, work).
category_from_tag(work, work).
category_from_tag(rest, rest).
category_from_tag(play, play).
category_from_tag(_, other).

% Extract all tags from events
extract_tags(Events, UniqueTags) :-
    findall(Tag,
            (member(schedule_event(_, _, _, _, _, Tags, _, _), Events),
             member(Tag, Tags)),
            AllTags),
    list_to_set(AllTags, UniqueTags).

% ============================================================================
% Timeline Queries
% ============================================================================

% Get timeline data at a specific minute
timeline_at_minute(Minute, Timeline, MinuteData) :-
    Timestamp is Minute * 60,
    member(MinuteData, Timeline),
    MinuteData = timeline_minute(T, _, _, _),
    abs(T - Timestamp) < 60,
    !.

% Get categories active during a time range
get_minute_categories(StartStamp, EndStamp, Timeline, Categories) :-
    StartMinute is floor(StartStamp / 60),
    EndMinute is floor(EndStamp / 60),
    findall(Cat,
            (between(StartMinute, EndMinute, Minute),
             timeline_at_minute(Minute, Timeline, timeline_minute(_, Cats, _, _)),
             member(Cat, Cats)),
            AllCategories),
    list_to_set(AllCategories, Categories).

% Get context tags at a specific minute
get_context_tags_at_minute(Minute, Timeline, Tags) :-
    (timeline_at_minute(Minute, Timeline, timeline_minute(_, _, Tags, _)) -> true ; Tags = []).

% ============================================================================
% Work Outcome Analysis
% ============================================================================

% Find productive windows that precede work outcomes
% work_outcome_windows(Timeline, WorkItems, Windows)
% Windows = [window(StartMin, EndMin, WorkItemID, ContextTags)]
work_outcome_windows(Timeline, WorkItems, Windows) :-
    findall(Window,
            work_outcome_window(Timeline, WorkItems, Window),
            Windows).

% Identify a single work outcome window
work_outcome_window(Timeline, WorkItems, window(StartMin, EndMin, ItemID, ContextTags)) :-
    member(WorkItem, WorkItems),
    WorkItem = work_item(ItemID, _, _, Status, _, Timestamps, _, _),
    member(Status, [complete, submitted]),
    
    % Get completion/modification timestamp
    (get_dict(completed, Timestamps, CompletedStamp) -> true ;
     get_dict(modified, Timestamps, CompletedStamp)),
    
    % Look for productive window (e.g., 4 hours before completion)
    WindowHours = 4,
    StartStamp is CompletedStamp - (WindowHours * 3600),
    EndStamp = CompletedStamp,
    
    StartMin is floor(StartStamp / 60),
    EndMin is floor(EndStamp / 60),
    
    % Extract context tags from this window
    get_minute_categories(StartStamp, EndStamp, Timeline, _),
    findall(Tag,
            (between(StartMin, EndMin, Minute),
             get_context_tags_at_minute(Minute, Timeline, Tags),
             member(Tag, Tags)),
            AllTags),
    list_to_set(AllTags, ContextTags).

% ============================================================================
% Event Conversion Utilities
% ============================================================================

% Convert an event to a list of minute timestamps
event_to_minutes(schedule_event(_, Start, End, _, _, _, _, _), Minutes) :-
    StartMin is floor(Start / 60),
    EndMin is floor(End / 60),
    findall(M, between(StartMin, EndMin, M), Minutes).
