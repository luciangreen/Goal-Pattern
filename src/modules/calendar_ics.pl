% calendar_ics.pl - ICS calendar file parsing and event ingestion
:- module(calendar_ics, [
    import_ics_file/1,
    parse_ics_file/2,
    apply_event_tags/3,
    determine_attendance_confidence/2,
    import_events/0,
    parse_ics_datetime/2
]).

:- use_module(library(readutil)).
:- use_module(library(date)).
:- use_module(library(http/json)).
:- use_module('../config').
:- use_module('../log').
:- use_module('../state').
:- use_module('../model').

% ============================================================================
% Main Import Interface
% ============================================================================

% Import all configured ICS files
import_events :-
    log_info('Starting calendar import...'),
    (config:get_config([calendar, ics_files], ICSFiles) ->
        (maplist(import_ics_file, ICSFiles),
         log_info('Calendar import completed'))
    ;
        log_warn('No ICS files configured for import')
    ),
    !.

% Import a single ICS file
import_ics_file(FilePath) :-
    (exists_file(FilePath) ->
        (format(atom(Msg), 'Importing ICS file: ~w', [FilePath]),
         log_info(Msg),
         parse_ics_file(FilePath, Events),
         process_events(Events, FilePath))
    ;
        (format(atom(Msg), 'ICS file not found: ~w', [FilePath]),
         log_warn(Msg))
    ),
    !.

% Process parsed events
process_events([], _).
process_events([Event|Rest], Source) :-
    (process_single_event(Event, Source) ->
        true
    ;
        format(atom(Msg), 'Failed to process event: ~w', [Event]),
        log_warn(Msg)
    ),
    process_events(Rest, Source).

% Process a single event
process_single_event(Event, _Source) :-
    get_dict(uid, Event, UID),
    get_dict(summary, Event, Summary),
    get_dict(start, Event, Start),
    get_dict(end, Event, End),
    get_dict(location, Event, Location),
    
    % Apply tags based on rules
    apply_event_tags(Summary, Location, Tags),
    
    % Determine attendance confidence
    determine_attendance_confidence(Event, AttendanceConfidence),
    
    % Check if event already exists
    atom_concat('ics_', UID, EventID),
    (find_existing_event(EventID, ExistingEvent) ->
        update_existing_event(ExistingEvent, EventID, Start, End, Summary, Location, Tags, AttendanceConfidence)
    ;
        create_new_event(EventID, Start, End, Summary, Location, Tags, AttendanceConfidence)
    ),
    !.

process_single_event(Event, _Source) :-
    format(atom(Msg), 'Skipping malformed event: ~w', [Event]),
    log_debug(Msg).

% ============================================================================
% ICS File Parsing
% ============================================================================

% Parse ICS file and extract events
parse_ics_file(FilePath, Events) :-
    catch(
        parse_ics_file_safe(FilePath, Events),
        Error,
        (format(atom(Msg), 'Error parsing ICS file ~w: ~w', [FilePath, Error]),
         log_error(Msg),
         Events = [])
    ).

parse_ics_file_safe(FilePath, Events) :-
    open(FilePath, read, Stream, [encoding(utf8)]),
    read_string(Stream, _, Content),
    close(Stream),
    split_string(Content, "\n", "\r", Lines),
    extract_events(Lines, Events),
    !.

% Extract VEVENT entries from ICS lines
extract_events(Lines, Events) :-
    extract_all_events(Lines, [], Events).

% Extract all events recursively
extract_all_events([], Acc, Events) :- 
    reverse(Acc, Events),
    !.

extract_all_events(Lines, Acc, Events) :-
    % Find next VEVENT
    (extract_next_event(Lines, Event, RemainingLines) ->
        extract_all_events(RemainingLines, [Event|Acc], Events)
    ;
        reverse(Acc, Events)
    ).

% Extract the next event from lines
extract_next_event(Lines, Event, RemainingLines) :-
    append(_Before, ["BEGIN:VEVENT"|EventLines], Lines),
    append(EventContent, ["END:VEVENT"|After], EventLines),
    parse_vevent(EventContent, Event),
    !,
    RemainingLines = After.

% Parse VEVENT content into event dictionary
parse_vevent(Lines, Event) :-
    parse_vevent_lines(Lines, _{}, Event),
    % Ensure required fields exist
    get_dict(uid, Event, _),
    get_dict(summary, Event, _),
    get_dict(dtstart, Event, _),
    !.

% Parse individual VEVENT lines
parse_vevent_lines([], Event, Event) :- !.
parse_vevent_lines([Line|Rest], Acc, Event) :-
    parse_vevent_line(Line, Acc, NewAcc),
    parse_vevent_lines(Rest, NewAcc, Event).

% Parse individual line types
parse_vevent_line(Line, Acc, NewAcc) :-
    split_string(Line, ":", "", [KeyPart, ValuePart]),
    split_string(KeyPart, ";", "", [Key|_Params]),
    string_lower(Key, LowerKey),
    parse_field(LowerKey, ValuePart, Acc, NewAcc),
    !.

parse_vevent_line(_Line, Acc, Acc).

% Parse specific fields
parse_field("uid", Value, Acc, NewAcc) :-
    string(Value),
    put_dict(uid, Acc, Value, NewAcc).

parse_field("summary", Value, Acc, NewAcc) :-
    string(Value),
    put_dict(summary, Acc, Value, NewAcc).

parse_field("location", Value, Acc, NewAcc) :-
    string(Value),
    put_dict(location, Acc, Value, NewAcc).

parse_field("description", Value, Acc, NewAcc) :-
    string(Value),
    put_dict(description, Acc, Value, NewAcc).

parse_field("status", Value, Acc, NewAcc) :-
    string(Value),
    put_dict(status, Acc, Value, NewAcc).

parse_field("dtstart", Value, Acc, NewAcc) :-
    parse_ics_datetime(Value, Timestamp),
    put_dict(dtstart, Acc, Value, Temp),
    put_dict(start, Temp, Timestamp, NewAcc).

parse_field("dtend", Value, Acc, NewAcc) :-
    parse_ics_datetime(Value, Timestamp),
    put_dict(dtend, Acc, Value, Temp),
    put_dict(end, Temp, Timestamp, NewAcc).

parse_field(_Key, _Value, Acc, Acc).

% Parse ICS datetime to Unix timestamp
% Format: 20231215T140000Z or 20231215T140000
parse_ics_datetime(DateTimeStr, Timestamp) :-
    string(DateTimeStr),
    string_chars(DateTimeStr, Chars),
    (append(DateChars, ['T'|TimeChars], Chars) ->
        parse_ics_date(DateChars, Year, Month, Day),
        parse_ics_time(TimeChars, Hour, Minute, Second),
        date_time_stamp(date(Year, Month, Day, Hour, Minute, Second, 0, -, -), Timestamp)
    ;
        % Date only, use start of day
        parse_ics_date(Chars, Year, Month, Day),
        date_time_stamp(date(Year, Month, Day, 0, 0, 0, 0, -, -), Timestamp)
    ),
    !.

parse_ics_datetime(_DateTimeStr, 0).

% Parse date part: YYYYMMDD
parse_ics_date(Chars, Year, Month, Day) :-
    length(YearChars, 4),
    append(YearChars, Rest1, Chars),
    atom_chars(YearAtom, YearChars),
    atom_number(YearAtom, Year),
    
    length(MonthChars, 2),
    append(MonthChars, Rest2, Rest1),
    atom_chars(MonthAtom, MonthChars),
    atom_number(MonthAtom, Month),
    
    length(DayChars, 2),
    append(DayChars, _, Rest2),
    atom_chars(DayAtom, DayChars),
    atom_number(DayAtom, Day),
    !.

% Parse time part: HHMMSS or HHMMSSZZ
parse_ics_time(Chars, Hour, Minute, Second) :-
    % Remove trailing 'Z' if present
    (append(TimeChars, ['Z'], Chars) -> true ; TimeChars = Chars),
    
    length(HourChars, 2),
    append(HourChars, Rest1, TimeChars),
    atom_chars(HourAtom, HourChars),
    atom_number(HourAtom, Hour),
    
    length(MinuteChars, 2),
    append(MinuteChars, Rest2, Rest1),
    atom_chars(MinuteAtom, MinuteChars),
    atom_number(MinuteAtom, Minute),
    
    length(SecondChars, 2),
    append(SecondChars, _, Rest2),
    atom_chars(SecondAtom, SecondChars),
    atom_number(SecondAtom, Second),
    !.

parse_ics_time(_, 0, 0, 0).

% ============================================================================
% Event Tagging
% ============================================================================

% Apply tags based on keyword rules in config
apply_event_tags(Summary, Location, Tags) :-
    load_tag_rules(TagRules),
    findall(Tag, matches_tag_rule(Summary, Location, TagRules, Tag), TagList),
    list_to_set(TagList, Tags),
    !.

apply_event_tags(_Summary, _Location, []).

% Load tag rules from config
load_tag_rules(TagRules) :-
    % Try to load from tag_rules.json
    (exists_file('config/tag_rules.json') ->
        open('config/tag_rules.json', read, Stream, [encoding(utf8)]),
        json_read_dict(Stream, TagRules),
        close(Stream)
    ;
        TagRules = _{event_tags: _{}, location_tags: _{}}
    ),
    !.

% Check if event matches a tag rule
matches_tag_rule(Summary, Location, TagRules, Tag) :-
    get_dict(event_tags, TagRules, EventTags),
    dict_pairs(EventTags, _, Pairs),
    member(Tag-Keywords, Pairs),
    member(Keyword, Keywords),
    (contains_keyword_ci(Summary, Keyword) ; contains_keyword_ci(Location, Keyword)).

matches_tag_rule(_Summary, Location, TagRules, Tag) :-
    get_dict(location_tags, TagRules, LocationTags),
    dict_pairs(LocationTags, _, Pairs),
    member(Tag-Keywords, Pairs),
    member(Keyword, Keywords),
    contains_keyword_ci(Location, Keyword).

% Case-insensitive keyword matching
contains_keyword_ci(Text, Keyword) :-
    string(Text),
    string(Keyword),
    string_lower(Text, LowerText),
    string_lower(Keyword, LowerKeyword),
    sub_string(LowerText, _, _, _, LowerKeyword).

% ============================================================================
% Attendance Confidence
% ============================================================================

% Determine attendance confidence based on event status and keywords
determine_attendance_confidence(Event, Confidence) :-
    (get_dict(status, Event, Status) ->
        determine_confidence_by_status(Status, Confidence)
    ;
        % Default confidence when no status is available
        Confidence = 0.5
    ),
    !.

determine_confidence_by_status(Status, Confidence) :-
    string(Status),
    string_upper(Status, UpperStatus),
    (UpperStatus = "CONFIRMED" -> Confidence = 0.9
    ; UpperStatus = "TENTATIVE" -> Confidence = 0.3
    ; UpperStatus = "CANCELLED" -> Confidence = 0.0
    ; Confidence = 0.5  % Default
    ).

% ============================================================================
% Event Management
% ============================================================================

% Find existing event by ID
find_existing_event(EventID, Event) :-
    state:get_schedule_events(Events),
    member(Event, Events),
    Event = schedule_event(EventID, _, _, _, _, _, _, _),
    !.

% Create new event
create_new_event(EventID, Start, End, Summary, Location, Tags, AttendanceConfidence) :-
    model:create_schedule_event(EventID, Start, End, Summary, Location, 
                                Tags, ics, AttendanceConfidence, Event),
    state:add_schedule_event(Event),
    
    format(atom(Msg), 'New event imported: ~w (~w)', [Summary, EventID]),
    log_info(Msg),
    !.

% Update existing event
update_existing_event(_OldEvent, EventID, Start, End, Summary, Location, Tags, AttendanceConfidence) :-
    model:create_schedule_event(EventID, Start, End, Summary, Location,
                                Tags, ics, AttendanceConfidence, NewEvent),
    state:update_schedule_event(EventID, NewEvent),
    
    format(atom(Msg), 'Event updated: ~w (~w)', [Summary, EventID]),
    log_debug(Msg),
    !.
