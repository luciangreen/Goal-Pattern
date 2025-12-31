% run_tests.pl - Test runner
:- use_module(library(plunit)).

% Load test files
:- consult('test_model.pl').
:- consult('test_daemon.pl').
:- consult('test_disk_scan.pl').
:- consult('test_calendar_ics.pl').
:- consult('test_progress.pl').
:- consult('test_planner.pl').
:- consult('test_gmail.pl').
:- consult('test_timeline.pl').
:- consult('test_stats.pl').
:- consult('test_insights.pl').
:- consult('test_llm.pl').
:- consult('test_review.pl').
:- consult('test_automate.pl').

% Run all tests
run_tests :-
    run_tests([model_goal, model_work_item, model_status_transitions, model_schedule_event, 
               model_time_block, model_safety_margin, validate_predicates, model_serialization,
               daemon, disk_scan, calendar_ics, progress, productivity, report_output,
               preferences, reminders, planner, planner_integration, gmail,
               timeline, stats, insights,
               llm_input_pack, llm_response_parsing, llm_workflow_rules, llm_audit, llm_suggestion_management,
               review_task_creation, safety_margin_calculation, verification_workflow, 
               review_completion, review_status_queries, automate]),
    halt(0).

% Run tests on load if called with -g run_tests
:- initialization(true).
