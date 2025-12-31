% test_llm.pl - Tests for LLM module (Spec 9)
:- use_module(library(plunit)).
:- use_module('../src/modules/llm').
:- use_module('../src/review').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/config').

% ============================================================================
% LLM Module Tests
% ============================================================================

:- begin_tests(llm_input_pack).

test(build_input_pack_draft_workflow, []) :-
    Text = "Sample algorithm text",
    Context = _{type: algorithm, current_count: 10},
    Workflow = 'draft_then_user_check',
    llm:build_input_pack(Text, Context, Workflow, Pack),
    
    % Verify pack structure
    assertion(is_dict(Pack)),
    assertion(get_dict(text, Pack, Text)),
    assertion(get_dict(workflow, Pack, Workflow)),
    assertion(get_dict(system_prompt, Pack, _)),
    assertion(get_dict(user_prompt, Pack, _)).

test(build_input_pack_outline_workflow, []) :-
    Text = "Philosophy draft",
    Context = _{type: philosophy, current_count: 50},
    Workflow = 'outline_only',
    llm:build_input_pack(Text, Context, Workflow, Pack),
    
    % Verify pack has outline-specific prompts
    assertion(is_dict(Pack)),
    get_dict(system_prompt, Pack, SysPrompt),
    assertion(sub_string(SysPrompt, _, _, _, "philosophy")).

test(build_input_pack_checklist_workflow, []) :-
    Text = "Complete text",
    Context = _{type: algorithm, current_count: 100},
    Workflow = 'complete_with_checklist',
    llm:build_input_pack(Text, Context, Workflow, Pack),
    
    % Verify pack structure
    assertion(is_dict(Pack)),
    get_dict(user_prompt, Pack, UserPrompt),
    assertion(sub_string(UserPrompt, _, _, _, "checklist")).

:- end_tests(llm_input_pack).

:- begin_tests(llm_response_parsing).

test(parse_llm_response_basic, []) :-
    Response = _{
        suggestions: [
            _{text: "Suggested completion", type: completion, confidence: 0.8,
              citations: [], word_count: 50, clause_count: 5}
        ],
        confidence: 0.75
    },
    llm:parse_llm_response(Response, Suggestions),
    
    % Verify parsed suggestions
    assertion(length(Suggestions, 1)),
    Suggestions = [S1],
    assertion(get_dict(text, S1, "Suggested completion")),
    assertion(get_dict(confidence, S1, 0.8)),
    assertion(get_dict(word_count, S1, 50)),
    assertion(get_dict(needs_review, S1, true)).

test(parse_llm_response_multiple, []) :-
    Response = _{
        suggestions: [
            _{text: "First suggestion", type: completion, confidence: 0.8,
              citations: [], word_count: 30, clause_count: 3},
            _{text: "Second suggestion", type: alternative, confidence: 0.7,
              citations: [], word_count: 40, clause_count: 4}
        ],
        confidence: 0.75
    },
    llm:parse_llm_response(Response, Suggestions),
    
    % Verify multiple suggestions
    assertion(length(Suggestions, 2)).

test(parse_llm_response_default_values, []) :-
    % Response with minimal fields, should use defaults
    Response = _{
        suggestions: [
            _{text: "Minimal suggestion"}
        ]
    },
    llm:parse_llm_response(Response, Suggestions),
    
    % Verify defaults are applied
    Suggestions = [S1],
    assertion(get_dict(confidence, S1, 0.7)),
    assertion(get_dict(citations, S1, [])),
    assertion(get_dict(word_count, S1, 0)).

:- end_tests(llm_response_parsing).

:- begin_tests(llm_workflow_rules).

test(workflow_draft_then_user_check, []) :-
    Suggestions = [_{text: "Draft", needs_review: true}],
    WorkItem = work_item(test1, algorithm, "test.pl", draft, 10, _{}, [], 0.8),
    llm:apply_workflow_rules('draft_then_user_check', Suggestions, WorkItem, Processed),
    
    % Verify workflow step added
    Processed = [P1],
    assertion(get_dict(workflow_step, P1, 'user_grammar_check')).

test(workflow_outline_only, []) :-
    Suggestions = [_{text: "Outline", needs_review: true}],
    WorkItem = work_item(test2, philosophy, "test.txt", draft, 0, _{}, [], 0.8),
    llm:apply_workflow_rules('outline_only', Suggestions, WorkItem, Processed),
    
    % Verify workflow step added
    Processed = [P1],
    assertion(get_dict(workflow_step, P1, 'user_expansion')).

test(workflow_complete_with_checklist, []) :-
    Suggestions = [_{text: "Complete", needs_review: true}],
    WorkItem = work_item(test3, algorithm, "test.pl", partial, 50, _{}, [], 0.8),
    llm:apply_workflow_rules('complete_with_checklist', Suggestions, WorkItem, Processed),
    
    % Verify workflow step added
    Processed = [P1],
    assertion(get_dict(workflow_step, P1, 'checklist_verification')).

:- end_tests(llm_workflow_rules).

:- begin_tests(llm_audit).

test(hash_input_pack, []) :-
    Pack = _{text: "Test", workflow: 'draft'},
    llm:hash_input_pack(Pack, Hash),
    
    % Verify hash is generated
    assertion(atom(Hash)),
    assertion(atom_length(Hash, L)),
    assertion(L > 0).

test(audit_llm_interaction, []) :-
    % Initialize state
    state:init_state,
    
    InputPack = _{text: "Test", workflow: 'draft', system_prompt: "Sys", user_prompt: "User"},
    Response = _{response_id: "resp123", suggestions: []},
    Suggestions = [],
    
    % Audit the interaction
    llm:audit_llm_interaction(analyze, InputPack, Response, Suggestions),
    
    % Verify audit record was stored
    state:get_llm_audit_records(Records),
    assertion(length(Records, 1)),
    Records = [R1],
    assertion(get_dict(operation, R1, analyze)),
    assertion(get_dict(response_id, R1, "resp123")),
    
    % Cleanup
    state:clear_state.

:- end_tests(llm_audit).

:- begin_tests(llm_suggestion_management).

test(mark_suggestions_for_review, []) :-
    ParsedSuggestions = [
        _{text: "Suggestion 1", confidence: 0.8},
        _{text: "Suggestion 2", confidence: 0.7}
    ],
    llm:mark_suggestions_for_review(ParsedSuggestions, Marked),
    
    % Verify review metadata added
    assertion(length(Marked, 2)),
    Marked = [M1, M2],
    assertion(get_dict(review_status, M1, 'needs_review')),
    assertion(get_dict(review_status, M2, 'needs_review')),
    assertion(get_dict(created_at, M1, _)),
    assertion(get_dict(safety_margin_applied, M1, false)).

test(apply_llm_suggestion, []) :-
    % Initialize state
    state:init_state,
    
    % Create a work item
    model:create_work_item(work1, algorithm, "test.pl", draft, 10, 
                          _{created: 1000}, [], 0.8, WorkItem),
    state:add_work_item(WorkItem),
    
    % Apply suggestion
    Suggestion = _{
        text: "Completion text",
        word_count: 100,
        clause_count: 10,
        confidence: 0.75
    },
    llm:apply_llm_suggestion(work1, Suggestion),
    
    % Verify review task created
    state:get_review_tasks(Tasks),
    assertion(length(Tasks, L)),
    assertion(L > 0),
    
    % Cleanup
    state:clear_state.

test(get_llm_suggestions, []) :-
    % Initialize state
    state:init_state,
    
    % Add some review tasks
    Task1 = _{task_id: task1, source: 'llm', status: 'needs_review'},
    Task2 = _{task_id: task2, source: 'manual', status: 'needs_review'},
    Task3 = _{task_id: task3, source: 'llm', status: 'verified'},
    
    state:add_review_task(Task1),
    state:add_review_task(Task2),
    state:add_review_task(Task3),
    
    % Get LLM suggestions
    llm:get_llm_suggestions(LLMSuggestions),
    
    % Should only get LLM-sourced tasks
    assertion(length(LLMSuggestions, 2)),
    
    % Cleanup
    state:clear_state.

:- end_tests(llm_suggestion_management).
