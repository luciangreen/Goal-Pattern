% run_tests.pl - Test runner
:- use_module(library(plunit)).

% Load test files
:- consult('test_daemon.pl').
:- consult('test_model.pl').

% Run all tests
run_tests :-
    run_tests([daemon, model]),
    halt(0).

% Run tests on load if called with -g run_tests
:- initialization(true).
