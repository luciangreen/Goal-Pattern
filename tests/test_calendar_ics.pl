% test_calendar_ics.pl - Tests for ICS calendar parsing
:- use_module(library(plunit)).
:- use_module('../src/modules/calendar_ics').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/config').
:- use_module('../src/log').

% ============================================================================
% Test Suite: ICS Parsing
% ============================================================================

:- begin_tests(calendar_ics).

% Test parsing a simple ICS file
test(parse_simple_ics, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Create a test ICS file
    ICSContent = "BEGIN:VCALENDAR\nVERSION:2.0\nPRODID:-//Test//Test//EN\nBEGIN:VEVENT\nUID:test-event-001\nDTSTART:20231215T140000Z\nDTEND:20231215T150000Z\nSUMMARY:Test Meeting\nLOCATION:Office\nEND:VEVENT\nEND:VCALENDAR",
    write_test_ics_file('test_calendar.ics', ICSContent),
    
    % Parse the file
    parse_ics_file('test_calendar.ics', Events),
    
    % Verify results
    assertion(Events \= []),
    assertion(length(Events, 1)),
    
    % Check event content
    Events = [Event|_],
    assertion(get_dict(uid, Event, _)),
    assertion(get_dict(summary, Event, Summary)),
    assertion(Summary = "Test Meeting"),
    
    % Cleanup
    delete_file('test_calendar.ics').

% Test parsing multiple events
test(parse_multiple_events, [setup(setup_test), cleanup(cleanup_test)]) :-
    ICSContent = "BEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nUID:event-001\nDTSTART:20231215T140000Z\nDTEND:20231215T150000Z\nSUMMARY:Meeting 1\nEND:VEVENT\nBEGIN:VEVENT\nUID:event-002\nDTSTART:20231216T100000Z\nDTEND:20231216T110000Z\nSUMMARY:Meeting 2\nEND:VEVENT\nEND:VCALENDAR",
    write_test_ics_file('test_multi.ics', ICSContent),
    
    parse_ics_file('test_multi.ics', Events),
    assertion(length(Events, 2)),
    
    delete_file('test_multi.ics').

% Test event tagging with keywords
test(event_tagging, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Test sauna tag
    apply_event_tags("Weekly Sauna Session", "", Tags1),
    assertion(member(sauna, Tags1)),
    
    % Test seminar tag
    apply_event_tags("AI Seminar at University", "Room 101", Tags2),
    assertion(member(seminar, Tags2)),
    
    % Test work tag
    apply_event_tags("Work meeting with team", "Office", Tags3),
    assertion(member(work, Tags3)),
    
    % Test multiple tags
    apply_event_tags("Travel to conference", "Airport", Tags4),
    assertion(member(travel, Tags4)).

% Test attendance confidence determination
test(attendance_confidence, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Confirmed event
    Event1 = _{status: "CONFIRMED"},
    determine_attendance_confidence(Event1, Conf1),
    assertion(Conf1 = 0.9),
    
    % Tentative event
    Event2 = _{status: "TENTATIVE"},
    determine_attendance_confidence(Event2, Conf2),
    assertion(Conf2 = 0.3),
    
    % Cancelled event
    Event3 = _{status: "CANCELLED"},
    determine_attendance_confidence(Event3, Conf3),
    assertion(Conf3 = 0.0),
    
    % No status (default)
    Event4 = _{},
    determine_attendance_confidence(Event4, Conf4),
    assertion(Conf4 = 0.5).

% Test ICS datetime parsing
test(ics_datetime_parsing, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Parse a standard ICS datetime
    parse_ics_datetime("20231215T140000Z", Timestamp1),
    assertion(number(Timestamp1)),
    assertion(Timestamp1 > 0),
    
    % Parse date-only
    parse_ics_datetime("20231215", Timestamp2),
    assertion(number(Timestamp2)),
    assertion(Timestamp2 > 0).

% Test event import and storage
test(import_event_to_state, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Clear state
    state:clear_state,
    state:init_state,
    
    % Create and import event
    create_schedule_event(
        test_event_001,
        1702648800,  % Some timestamp
        1702652400,  % Some later timestamp
        "Test Event",
        "Test Location",
        [work, home],
        ics,
        0.8,
        Event
    ),
    
    state:add_schedule_event(Event),
    
    % Verify it's in state
    state:get_schedule_events(Events),
    assertion(length(Events, 1)),
    assertion(member(schedule_event(test_event_001, _, _, "Test Event", _, _, _, _), Events)),
    !.

% Test duplicate event handling
test(duplicate_event_update, [setup(setup_test), cleanup(cleanup_test)]) :-
    state:clear_state,
    state:init_state,
    
    % Add first event
    create_schedule_event(
        dup_test_001,
        1702648800,
        1702652400,
        "Original Title",
        "Location 1",
        [work],
        ics,
        0.5,
        Event1
    ),
    state:add_schedule_event(Event1),
    
    % Update the same event
    create_schedule_event(
        dup_test_001,
        1702648800,
        1702652400,
        "Updated Title",
        "Location 2",
        [home],
        ics,
        0.8,
        Event2
    ),
    state:update_schedule_event(dup_test_001, Event2),
    
    % Verify only one event exists and it's updated
    state:get_schedule_events(Events),
    assertion(length(Events, 1)),
    assertion(member(schedule_event(dup_test_001, _, _, "Updated Title", "Location 2", _, _, _), Events)),
    !.

% Test malformed ICS file handling
test(malformed_ics_handling, [setup(setup_test), cleanup(cleanup_test)]) :-
    % Create malformed ICS
    ICSContent = "NOT A VALID ICS FILE",
    write_test_ics_file('test_malformed.ics', ICSContent),
    
    % Parse should return empty list without crashing
    parse_ics_file('test_malformed.ics', Events),
    assertion(Events = []),
    
    delete_file('test_malformed.ics').

% Test event without location field
test(event_without_location, [setup(setup_test), cleanup(cleanup_test)]) :-
    ICSContent = "BEGIN:VCALENDAR\nVERSION:2.0\nBEGIN:VEVENT\nUID:no-loc-001\nDTSTART:20231215T140000Z\nDTEND:20231215T150000Z\nSUMMARY:No Location Event\nEND:VEVENT\nEND:VCALENDAR",
    write_test_ics_file('test_no_location.ics', ICSContent),
    
    % Parse should work even without location
    parse_ics_file('test_no_location.ics', Events),
    assertion(Events \= []),
    assertion(length(Events, 1)),
    
    % Check that event was parsed
    Events = [Event|_],
    assertion(get_dict(uid, Event, _)),
    assertion(get_dict(summary, Event, "No Location Event")),
    
    delete_file('test_no_location.ics').

% ============================================================================
% Test Helpers
% ============================================================================

setup_test :-
    % Initialize config and state if needed
    catch(config:load_config('config/config.json'), _, true),
    catch(state:init_state, _, true).

cleanup_test :-
    % Clean up any test files
    (exists_file('test_calendar.ics') -> delete_file('test_calendar.ics') ; true),
    (exists_file('test_multi.ics') -> delete_file('test_multi.ics') ; true),
    (exists_file('test_malformed.ics') -> delete_file('test_malformed.ics') ; true),
    (exists_file('test_no_location.ics') -> delete_file('test_no_location.ics') ; true).

% Helper to write test ICS file
write_test_ics_file(Filename, Content) :-
    open(Filename, write, Stream),
    write(Stream, Content),
    close(Stream).

:- end_tests(calendar_ics).
