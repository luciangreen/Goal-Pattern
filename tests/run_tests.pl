% run_tests.pl - Test runner
:- use_module(library(plunit)).

% Load test files
:- consult('test_model.pl').
:- consult('test_daemon.pl').
:- consult('test_disk_scan.pl').
:- consult('test_calendar_ics.pl').
:- consult('test_progress.pl').
:- consult('test_planner.pl').

% Run all tests
run_tests :-
    run_tests([model_goal, model_work_item, model_status_transitions, model_schedule_event, 
               model_time_block, model_safety_margin, validate_predicates, model_serialization,
               daemon, disk_scan, calendar_ics, progress, productivity, report_output,
               preferences, reminders, planner, planner_integration]),
    halt(0).

% Run tests on load if called with -g run_tests
:- initialization(true).
