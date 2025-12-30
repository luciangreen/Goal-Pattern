% run_tests.pl - Test runner
:- use_module(library(plunit)).

% Load test files
:- consult('test_daemon.pl').
:- consult('test_disk_scan.pl').
:- consult('test_calendar_ics.pl').
:- consult('test_progress.pl').

% Run all tests
run_tests :-
    run_tests([daemon, disk_scan, calendar_ics, progress, productivity, report_output]),
    halt(0).

% Run tests on load if called with -g run_tests
:- initialization(true).
