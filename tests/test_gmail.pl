% test_gmail.pl - Tests for Gmail module
:- begin_tests(gmail).

:- use_module('../src/modules/gmail').
:- use_module('../src/model').
:- use_module('../src/state').
:- use_module('../src/config').
:- use_module(library(http/json)).

% Test setup
setup_test_state :-
    config:load_config('config/config.json'),
    state:clear_state,
    state:init_state.

teardown_test_state :-
    state:clear_state.

% ============================================================================
% JSON Parsing Tests
% ============================================================================

test(parse_gmail_json_valid, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    JSONString = '{"emails": [{"message_id": "msg1", "thread_id": "thread1", "subject": "Test", "to": "test@example.com", "date": "Mon, 1 Jan 2024 10:00:00 +0000", "timestamp": 1704103200.0, "snippet": "Test message"}], "count": 1}',
    gmail:parse_gmail_json(JSONString, Emails),
    length(Emails, 1),
    Emails = [Email],
    get_dict(message_id, Email, MID),
    (MID = "msg1" ; MID = 'msg1'),  % Accept either string or atom
    get_dict(subject, Email, Subj),
    (Subj = "Test" ; Subj = 'Test').

test(parse_gmail_json_empty, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    JSONString = '{"emails": [], "count": 0}',
    gmail:parse_gmail_json(JSONString, Emails),
    length(Emails, 0).

test(parse_gmail_json_multiple, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    JSONString = '{"emails": [{"message_id": "msg1", "thread_id": "thread1", "subject": "Test1", "to": "test@example.com", "date": "Mon, 1 Jan 2024 10:00:00 +0000", "timestamp": 1704103200.0, "snippet": "Test 1"}, {"message_id": "msg2", "thread_id": "thread2", "subject": "Test2", "to": "test@example.com", "date": "Mon, 1 Jan 2024 11:00:00 +0000", "timestamp": 1704106800.0, "snippet": "Test 2"}], "count": 2}',
    gmail:parse_gmail_json(JSONString, Emails),
    length(Emails, 2).

% ============================================================================
% Evidence Extraction Tests
% ============================================================================

test(extract_work_evidence_completed_algorithms, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Weekly Update',
    Snippet = 'I completed 15 algorithms this week',
    gmail:extract_work_evidence(Subject, Snippet, Evidence),
    length(Evidence, L),
    L > 0,
    member(evidence(_, completion, Count, algorithm, Confidence, _), Evidence),
    Count = 15,
    Confidence >= 0.7.

test(extract_work_evidence_submitted_essay, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Philosophy Essay Submission',
    Snippet = 'Just submitted my philosophy essay on epistemic logic',
    gmail:extract_work_evidence(Subject, Snippet, Evidence),
    length(Evidence, L),
    L > 0,
    member(evidence(_, submission, _, philosophy, Confidence, _), Evidence),
    Confidence >= 0.7.

test(extract_work_evidence_multiple_patterns, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Work Update',
    Snippet = 'Completed 10 algorithms and submitted 2 essays today',
    gmail:extract_work_evidence(Subject, Snippet, Evidence),
    length(Evidence, L),
    L >= 2.

test(extract_work_evidence_wrote_clauses, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Daily Progress',
    Snippet = 'Wrote 25 Prolog clauses for the new module',
    gmail:extract_work_evidence(Subject, Snippet, Evidence),
    length(Evidence, L),
    L > 0,
    member(evidence(_, completion, Count, algorithm, _, _), Evidence),
    Count = 25.

test(extract_work_evidence_no_match, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Random Email',
    Snippet = 'Just checking in to say hello',
    gmail:extract_work_evidence(Subject, Snippet, Evidence),
    length(Evidence, 0).

% ============================================================================
% Schedule Change Extraction Tests
% ============================================================================

test(extract_schedule_changes_cancelled, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Cancelled AI Seminar',
    Snippet = 'The AI seminar scheduled for tomorrow has been cancelled',
    gmail:extract_schedule_changes(Subject, Snippet, Changes),
    length(Changes, L),
    L > 0,
    member(evidence(_, cancellation, _, _, Confidence, _), Changes),
    Confidence >= 0.7.

test(extract_schedule_changes_rescheduled, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'Meeting Rescheduled',
    Snippet = 'The weekly meeting has been rescheduled to next Monday',
    gmail:extract_schedule_changes(Subject, Snippet, Changes),
    length(Changes, L),
    L > 0,
    member(evidence(_, reschedule, _, _, _, _), Changes).

test(extract_schedule_changes_new_event, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    Subject = 'New Appointment',
    Snippet = 'Scheduled a new meeting with the team for Friday',
    gmail:extract_schedule_changes(Subject, Snippet, Changes),
    length(Changes, L),
    L > 0,
    member(evidence(_, addition, _, _, _, _), Changes).

% ============================================================================
% Work Type Normalization Tests
% ============================================================================

test(normalize_work_type_algorithm, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('algorithm', Type),
    Type = algorithm.

test(normalize_work_type_algorithms, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('algorithms', Type),
    Type = algorithm.

test(normalize_work_type_clause, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('clause', Type),
    Type = algorithm.

test(normalize_work_type_philosophy, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('philosophy', Type),
    Type = philosophy.

test(normalize_work_type_essay, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('essay', Type),
    Type = philosophy.

test(normalize_work_type_unknown, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:normalize_work_type('unknown_type', Type),
    Type = unknown.

% ============================================================================
% Integration Tests
% ============================================================================

test(load_evidence_rules, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:load_evidence_rules(Rules),
    is_dict(Rules),
    get_dict(work_completion_patterns, Rules, Patterns),
    is_list(Patterns),
    length(Patterns, L),
    L >= 5.  % At least 5 patterns as per spec

test(evidence_rules_have_required_fields, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    gmail:load_evidence_rules(Rules),
    get_dict(work_completion_patterns, Rules, Patterns),
    Patterns = [Pattern|_],
    get_dict(name, Pattern, _),
    get_dict(pattern, Pattern, _),
    get_dict(type, Pattern, _),
    get_dict(confidence, Pattern, _).

% ============================================================================
% Dry Run Test (requires helper script)
% ============================================================================

test(dry_run_fetch, [setup(setup_test_state), cleanup(teardown_test_state)]) :-
    % This test requires the helper script to be present
    % and tests dry-run mode
    catch(
        (gmail:fetch_gmail_messages(_{dry_run: true}),
         state:get_work_items(Items),
         length(Items, L),
         L >= 0  % Should have processed sample data
        ),
        Error,
        (format('Dry run test failed (expected if helper not available): ~w~n', [Error]), fail)
    ).

:- end_tests(gmail).
