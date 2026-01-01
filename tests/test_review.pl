% test_review.pl - Tests for review workflow with 10% safety margin (Spec 9)
:- use_module(library(plunit)).
:- use_module('../src/review').
:- use_module('../src/state').
:- use_module('../src/model').

% ============================================================================
% Review Module Tests
% ============================================================================

:- begin_tests(review_task_creation).

test(create_review_task_basic, []) :-
    % Initialize state
    state:init_state,
    
    % Create work item
    model:create_work_item(work1, algorithm, "test.pl", draft, 10,
                          _{created: 1000}, [], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    
    % Create review task from suggestion
    Suggestion = _{
        text: "Completion text",
        word_count: 0,
        clause_count: 100,
        confidence: 0.75,
        workflow_step: 'user_grammar_check'
    },
    
    review:create_review_task(work1, Suggestion),
    
    % Verify task created
    state:get_review_tasks(Tasks),
    assertion(length(Tasks, 1)),
    Tasks = [Task],
    assertion(get_dict(work_item_id, Task, work1)),
    assertion(get_dict(llm_generated_count, Task, 100)),
    assertion(get_dict(required_extra_work, Task, 10)),  % 10% of 100
    assertion(get_dict(verified_extra_work, Task, 0)),
    assertion(get_dict(safety_margin_met, Task, false)),
    
    % Cleanup
    state:clear_state.

test(create_review_task_word_count, []) :-
    state:init_state,
    
    model:create_work_item(work2, philosophy, "essay.txt", draft, 50,
                          _{created: 1000}, [], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    
    Suggestion = _{
        text: "Essay completion",
        word_count: 500,
        clause_count: 0,
        confidence: 0.8
    },
    
    review:create_review_task(work2, Suggestion),
    
    state:get_review_tasks([Task]),
    assertion(get_dict(llm_generated_count, Task, 500)),
    assertion(get_dict(required_extra_work, Task, 50)),  % 10% of 500
    
    state:clear_state.

:- end_tests(review_task_creation).

:- begin_tests(safety_margin_calculation).

test(calculate_required_extra_work, []) :-
    review:calculate_required_extra_work(100, Required),
    assertion(Required = 10).

test(calculate_required_extra_work_rounding, []) :-
    % Should round up: 10% of 95 = 9.5, rounds to 10
    review:calculate_required_extra_work(95, Required),
    assertion(Required = 10).

test(calculate_required_extra_work_small, []) :-
    % 10% of 5 = 0.5, rounds to 1
    review:calculate_required_extra_work(5, Required),
    assertion(Required = 1).

test(apply_safety_margin, []) :-
    review:apply_safety_margin(100, Adjusted),
    assertion(Adjusted =:= 110.0).

test(check_safety_margin_not_met, []) :-
    state:init_state,
    
    Task = _{
        task_id: task1,
        work_item_id: work1,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 5,
        safety_margin_met: false
    },
    state:add_review_task(Task),
    
    % Should fail - not enough extra work
    assertion(\+ review:check_safety_margin_met(task1)),
    
    state:clear_state.

test(check_safety_margin_met, []) :-
    state:init_state,
    
    Task = _{
        task_id: task2,
        work_item_id: work1,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 10,
        safety_margin_met: true
    },
    state:add_review_task(Task),
    
    % Should succeed - exactly enough extra work
    assertion(review:check_safety_margin_met(task2)),
    
    state:clear_state.

test(check_safety_margin_exceeded, []) :-
    state:init_state,
    
    Task = _{
        task_id: task3,
        work_item_id: work1,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 15,
        safety_margin_met: true
    },
    state:add_review_task(Task),
    
    % Should succeed - more than enough extra work
    assertion(review:check_safety_margin_met(task3)),
    
    state:clear_state.

:- end_tests(safety_margin_calculation).

:- begin_tests(verification_workflow).

test(verify_work_partial, []) :-
    state:init_state,
    
    Task = _{
        task_id: task1,
        work_item_id: work1,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 0,
        verification_notes: [],
        safety_margin_met: false
    },
    state:add_review_task(Task),
    
    % Verify 5 units of work (not enough yet)
    VerificationData = _{
        extra_work_count: 5,
        verification_notes: "Added 5 clauses",
        verifier: 'user'
    },
    review:verify_work(task1, VerificationData),
    
    % Check updated task
    state:get_review_task(task1, UpdatedTask),
    assertion(get_dict(verified_extra_work, UpdatedTask, 5)),
    assertion(get_dict(safety_margin_met, UpdatedTask, false)),
    assertion(get_dict(status, UpdatedTask, 'needs_review')),
    
    state:clear_state.

test(verify_work_complete, []) :-
    state:init_state,
    
    Task = _{
        task_id: task2,
        work_item_id: work2,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 0,
        verification_notes: [],
        safety_margin_met: false
    },
    state:add_review_task(Task),
    
    % Verify 10 units of work (exactly enough)
    VerificationData = _{
        extra_work_count: 10,
        verification_notes: "Added 10 clauses",
        verifier: 'user'
    },
    review:verify_work(task2, VerificationData),
    
    % Check updated task
    state:get_review_task(task2, UpdatedTask),
    assertion(get_dict(verified_extra_work, UpdatedTask, 10)),
    assertion(get_dict(safety_margin_met, UpdatedTask, true)),
    assertion(get_dict(status, UpdatedTask, 'verified')),
    
    state:clear_state.

test(verify_work_incremental, []) :-
    state:init_state,
    
    Task = _{
        task_id: task3,
        work_item_id: work3,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 3,
        verification_notes: [],
        safety_margin_met: false
    },
    state:add_review_task(Task),
    
    % Add 7 more units (total 10)
    VerificationData = _{
        extra_work_count: 7,
        verification_notes: "Added 7 more clauses"
    },
    review:verify_work(task3, VerificationData),
    
    state:get_review_task(task3, UpdatedTask),
    assertion(get_dict(verified_extra_work, UpdatedTask, 10)),
    assertion(get_dict(safety_margin_met, UpdatedTask, true)),
    
    state:clear_state.

:- end_tests(verification_workflow).

:- begin_tests(review_completion).

test(complete_review_success, []) :-
    state:init_state,
    
    % Create work item
    model:create_work_item(work1, algorithm, "test.pl", partial, 50,
                          _{created: 1000}, [], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    
    % Create verified task
    Task = _{
        task_id: task1,
        work_item_id: work1,
        source: 'llm',
        status: 'verified',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 10,
        verification_notes: [],
        safety_margin_met: true
    },
    state:add_review_task(Task),
    
    % Complete review
    CompletionData = _{
        completion_notes: "All verification steps completed"
    },
    review:complete_review(task1, CompletionData),
    
    % Verify work item updated
    state:get_work_item(work1, UpdatedItem),
    UpdatedItem = work_item(work1, algorithm, "test.pl", complete, FinalCount, _, _, _),
    % Original 50 + LLM 100 + Extra 10 = 160
    assertion(FinalCount = 160),
    
    % Verify task marked completed
    state:get_review_task(task1, CompletedTask),
    assertion(get_dict(status, CompletedTask, 'completed')),
    
    state:clear_state.

test(complete_review_fails_without_safety_margin, [fail]) :-
    state:init_state,
    
    model:create_work_item(work2, algorithm, "test.pl", partial, 50,
                          _{created: 1000}, [], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    
    Task = _{
        task_id: task2,
        work_item_id: work2,
        source: 'llm',
        status: 'needs_review',
        llm_generated_count: 100,
        required_extra_work: 10,
        verified_extra_work: 5,  % Not enough!
        verification_notes: [],
        safety_margin_met: false
    },
    state:add_review_task(Task),
    
    CompletionData = _{completion_notes: "Try to complete"},
    % This should fail
    review:complete_review(task2, CompletionData),
    
    state:clear_state.

:- end_tests(review_completion).

:- begin_tests(review_status_queries).

test(get_review_status_no_reviews, []) :-
    state:init_state,
    
    review:get_review_status(work1, Status),
    assertion(get_dict(has_reviews, Status, false)),
    
    state:clear_state.

test(get_review_status_with_tasks, []) :-
    state:init_state,
    
    Task1 = _{
        task_id: task1, work_item_id: work1, source: 'llm',
        status: 'needs_review', required_extra_work: 10, verified_extra_work: 5
    },
    Task2 = _{
        task_id: task2, work_item_id: work1, source: 'llm',
        status: 'verified', required_extra_work: 20, verified_extra_work: 25
    },
    
    state:add_review_task(Task1),
    state:add_review_task(Task2),
    
    review:get_review_status(work1, Status),
    assertion(get_dict(has_reviews, Status, true)),
    assertion(get_dict(total_tasks, Status, 2)),
    assertion(get_dict(needs_review, Status, 1)),
    assertion(get_dict(verified, Status, 1)),
    assertion(get_dict(total_required_extra, Status, 30)),
    assertion(get_dict(total_verified_extra, Status, 30)),
    
    state:clear_state.

test(get_tasks_needing_review, []) :-
    state:init_state,
    
    Task1 = _{task_id: task1, status: 'needs_review'},
    Task2 = _{task_id: task2, status: 'verified'},
    Task3 = _{task_id: task3, status: 'needs_review'},
    Task4 = _{task_id: task4, status: 'completed'},
    
    state:add_review_task(Task1),
    state:add_review_task(Task2),
    state:add_review_task(Task3),
    state:add_review_task(Task4),
    
    review:get_tasks_needing_review(NeedingReview),
    assertion(length(NeedingReview, 2)),
    
    state:clear_state.

:- end_tests(review_status_queries).
