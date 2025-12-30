#!/usr/bin/env swipl

% demo_calendar_import.pl - Interactive demo for calendar import
% Usage: swipl demo_calendar_import.pl

:- use_module('src/modules/calendar_ics').
:- use_module('src/state').
:- use_module('src/config').
:- use_module('src/log').

demo :-
    writeln('=== Calendar Import Demo ==='),
    writeln(''),
    
    % Initialize
    writeln('1. Initializing system...'),
    config:load_config('config/config.json'),
    state:init_state,
    writeln('   ✓ System initialized'),
    writeln(''),
    
    % Import sample calendar
    writeln('2. Importing sample calendar (examples/sample_calendar.ics)...'),
    calendar_ics:import_ics_file('examples/sample_calendar.ics'),
    writeln('   ✓ Calendar imported'),
    writeln(''),
    
    % Display results
    writeln('3. Imported Events:'),
    writeln(''),
    state:get_schedule_events(Events),
    length(Events, Count),
    format('   Found ~w events:~n~n', [Count]),
    
    % Display each event with formatting
    forall(
        member(schedule_event(_ID, Start, End, Title, Location, Tags, _Source, Conf), Events),
        (
            format('   📅 ~w~n', [Title]),
            format('      Location: ~w~n', [Location]),
            format('      Tags: ~w~n', [Tags]),
            format('      Confidence: ~w~n', [Conf]),
            format_time(atom(StartTime), '%Y-%m-%d %H:%M', Start),
            format_time(atom(EndTime), '%Y-%m-%d %H:%M', End),
            format('      Time: ~w to ~w~n~n', [StartTime, EndTime])
        )
    ),
    
    writeln('=== Demo Complete ==='),
    writeln(''),
    writeln('Try it yourself:'),
    writeln('1. Edit config/config.json to add your ICS file paths'),
    writeln('2. Run: ?- calendar_ics:import_events.'),
    writeln('3. View: ?- state:get_schedule_events(Events).'),
    writeln(''),
    
    halt(0).

:- initialization(demo).
