% llm.pl - LLM integration (ChatGPT/Gemini) with safety margin and verification
:- module(llm, [
    llm_analyze_text/3,
    llm_suggest_completions/3,
    build_input_pack/4,
    parse_llm_response/2,
    apply_llm_suggestion/2,
    get_llm_suggestions/1,
    audit_llm_call/5
]).

:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(process)).
:- use_module(library(sha)).
:- use_module('../config').
:- use_module('../log').
:- use_module('../state').
:- use_module('../model').
:- use_module('../review').

% ============================================================================
% Main LLM Interface
% ============================================================================

% Analyze text using LLM and return suggestions
% llm_analyze_text(+Text, +Context, -Suggestions)
llm_analyze_text(Text, Context, Suggestions) :-
    log_info('Starting LLM text analysis...'),
    
    % Build input pack
    build_input_pack(Text, Context, analyze, InputPack),
    
    % Call LLM helper
    call_llm_helper(InputPack, Response),
    
    % Parse response
    parse_llm_response(Response, ParsedSuggestions),
    
    % Audit the call
    audit_llm_interaction(analyze, InputPack, Response, ParsedSuggestions),
    
    % Mark suggestions as needing review
    mark_suggestions_for_review(ParsedSuggestions, Suggestions),
    
    log_info('LLM analysis completed'),
    !.

% Suggest completions for incomplete work
% llm_suggest_completions(+WorkItemID, +Options, -Suggestions)
llm_suggest_completions(WorkItemID, Options, Suggestions) :-
    log_info('Generating LLM completions...'),
    
    % Get work item
    state:get_work_item(WorkItemID, WorkItem),
    WorkItem = work_item(_, Type, Origin, Status, Count, _, Tags, _),
    
    % Build context
    Context = _{
        work_item_id: WorkItemID,
        type: Type,
        status: Status,
        current_count: Count,
        tags: Tags,
        options: Options
    },
    
    % Load file content if applicable
    (atom(Origin), exists_file(Origin) ->
        read_file_to_string(Origin, Text, [])
    ; Text = ""),
    
    % Build input pack
    (get_dict(workflow, Options, Workflow) -> true ; Workflow = 'draft_then_user_check'),
    build_input_pack(Text, Context, Workflow, InputPack),
    
    % Call LLM helper
    call_llm_helper(InputPack, Response),
    
    % Parse response
    parse_llm_response(Response, ParsedSuggestions),
    
    % Audit the call
    audit_llm_interaction(suggest_completions, InputPack, Response, ParsedSuggestions),
    
    % Apply workflow-specific processing
    apply_workflow_rules(Workflow, ParsedSuggestions, WorkItem, Suggestions),
    
    log_info('LLM completions generated'),
    !.

% ============================================================================
% Input Pack Builder
% ============================================================================

% Build input pack for LLM from files/snippets/goals/settings
% build_input_pack(+Text, +Context, +Workflow, -InputPack)
build_input_pack(Text, Context, Workflow, InputPack) :-
    % Get workflow configuration
    config:get_config([llm, workflows, Workflow], WorkflowConfig),
    
    % Build the pack
    get_time(Timestamp),
    InputPack = _{
        text: Text,
        context: Context,
        workflow: Workflow,
        workflow_config: WorkflowConfig,
        timestamp: Timestamp,
        system_prompt: SystemPrompt,
        user_prompt: UserPrompt
    },
    
    % Generate prompts based on workflow
    generate_prompts(Workflow, Text, Context, WorkflowConfig, SystemPrompt, UserPrompt),
    !.

% Generate system and user prompts for workflow
generate_prompts('draft_then_user_check', Text, Context, Config, SystemPrompt, UserPrompt) :-
    get_dict(instructions, Config, Instructions, 
             "Generate a draft completion. User will grammar-check and paraphrase."),
    format(string(SystemPrompt), 
           "You are an assistant helping with ~w. ~w", 
           [Context.type, Instructions]),
    format(string(UserPrompt),
           "Given this text:\n~w\n\nProvide suggestions for completion. Current count: ~w",
           [Text, Context.current_count]).

generate_prompts('outline_only', Text, Context, Config, SystemPrompt, UserPrompt) :-
    get_dict(instructions, Config, Instructions, 
             "Generate only an outline, not full text."),
    format(string(SystemPrompt), 
           "You are an assistant helping with ~w. ~w", 
           [Context.type, Instructions]),
    format(string(UserPrompt),
           "Given this text:\n~w\n\nProvide an outline for completion.",
           [Text]).

generate_prompts('complete_with_checklist', Text, Context, Config, SystemPrompt, UserPrompt) :-
    get_dict(instructions, Config, Instructions, 
             "Generate complete text but include a verification checklist."),
    format(string(SystemPrompt), 
           "You are an assistant helping with ~w. ~w", 
           [Context.type, Instructions]),
    format(string(UserPrompt),
           "Given this text:\n~w\n\nProvide completion with verification checklist.",
           [Text]).

% Default workflow
generate_prompts(Workflow, Text, Context, _Config, SystemPrompt, UserPrompt) :-
    format(string(SystemPrompt), 
           "You are an assistant helping with ~w work.", 
           [Context.type]),
    format(string(UserPrompt),
           "Given this text:\n~w\n\nProvide suggestions using workflow: ~w",
           [Text, Workflow]).

% ============================================================================
% LLM Helper Calling
% ============================================================================

% Call external LLM helper script
call_llm_helper(InputPack, Response) :-
    config:get_config([llm, helper_script], HelperScript),
    config:get_config([llm, provider], Provider, 'gemini'),
    
    % Write input pack to temporary JSON file
    tmp_file_stream(text, TmpFile, Stream),
    json_write_dict(Stream, InputPack, []),
    close(Stream),
    
    % Build command
    format(atom(Args), '--provider=~w --input=~w', [Provider, TmpFile]),
    split_string(Args, " ", "", ArgsList),
    
    % Execute helper
    format(atom(Msg), 'Executing: python3 ~w ~w', [HelperScript, Args]),
    log_debug(Msg),
    
    catch(
        (setup_call_cleanup(
            process_create(path(python3), [HelperScript | ArgsList],
                          [stdout(pipe(Out)), stderr(null), process(PID)]),
            read_string(Out, _, JSONString),
            (close(Out), process_wait(PID, ExitCode))
        ),
        (ExitCode = exit(0) ->
            atom_string(JSONAtom, JSONString),
            atom_json_dict(JSONAtom, Response, [])
        ;
            format(atom(ErrorMsg), 'LLM helper exited with code: ~w', [ExitCode]),
            log_error(ErrorMsg),
            fail
        )),
        Error,
        (format(atom(ErrorMsg), 'Error calling LLM helper: ~w', [Error]),
         log_error(ErrorMsg),
         fail)
    ),
    
    % Clean up temp file
    delete_file(TmpFile),
    !.

% ============================================================================
% Response Parsing
% ============================================================================

% Parse LLM JSON response into structured suggestions
% parse_llm_response(+Response, -Suggestions)
parse_llm_response(Response, Suggestions) :-
    get_dict(suggestions, Response, SuggestionsList),
    (get_dict(confidence, Response, OverallConfidence) -> true ; OverallConfidence = 0.7),
    
    % Process each suggestion
    maplist(parse_single_suggestion(OverallConfidence), SuggestionsList, Suggestions),
    !.

% Parse a single suggestion
parse_single_suggestion(DefaultConfidence, SuggDict, Suggestion) :-
    get_dict(text, SuggDict, Text),
    (get_dict(type, SuggDict, Type) -> true ; Type = 'completion'),
    (get_dict(confidence, SuggDict, Confidence) -> true ; Confidence = DefaultConfidence),
    (get_dict(citations, SuggDict, Citations) -> true ; Citations = []),
    (get_dict(word_count, SuggDict, WordCount) -> true ; WordCount = 0),
    (get_dict(clause_count, SuggDict, ClauseCount) -> true ; ClauseCount = 0),
    
    Suggestion = _{
        text: Text,
        type: Type,
        confidence: Confidence,
        citations: Citations,
        word_count: WordCount,
        clause_count: ClauseCount,
        needs_review: true,
        safety_margin_applied: false
    }.

% ============================================================================
% Workflow Processing
% ============================================================================

% Apply workflow-specific rules to suggestions
apply_workflow_rules('draft_then_user_check', Suggestions, _WorkItem, ProcessedSuggestions) :-
    % Mark all as needing user grammar check and paraphrase
    maplist(mark_needs_user_grammar_check, Suggestions, ProcessedSuggestions).

apply_workflow_rules('outline_only', Suggestions, _WorkItem, ProcessedSuggestions) :-
    % Outlines need expansion by user
    maplist(mark_needs_expansion, Suggestions, ProcessedSuggestions).

apply_workflow_rules('complete_with_checklist', Suggestions, _WorkItem, ProcessedSuggestions) :-
    % Add checklist requirement
    maplist(mark_needs_checklist_completion, Suggestions, ProcessedSuggestions).

% Default: mark as needing review
apply_workflow_rules(_Workflow, Suggestions, _WorkItem, ProcessedSuggestions) :-
    maplist(mark_needs_review, Suggestions, ProcessedSuggestions).

% Mark suggestion as needing user grammar check
mark_needs_user_grammar_check(Suggestion, Marked) :-
    put_dict(workflow_step, Suggestion, 'user_grammar_check', Marked).

% Mark suggestion as needing expansion
mark_needs_expansion(Suggestion, Marked) :-
    put_dict(workflow_step, Suggestion, 'user_expansion', Marked).

% Mark suggestion as needing checklist completion
mark_needs_checklist_completion(Suggestion, Marked) :-
    put_dict(workflow_step, Suggestion, 'checklist_verification', Marked).

% Mark suggestion as needing review
mark_needs_review(Suggestion, Marked) :-
    put_dict(needs_review, Suggestion, true, Marked).

% ============================================================================
% Suggestion Management
% ============================================================================

% Mark parsed suggestions for review in the review module
mark_suggestions_for_review(ParsedSuggestions, MarkedSuggestions) :-
    get_time(Timestamp),
    maplist(add_review_metadata(Timestamp), ParsedSuggestions, MarkedSuggestions).

add_review_metadata(Timestamp, Suggestion, MarkedSuggestion) :-
    put_dict(_{
        created_at: Timestamp,
        review_status: 'needs_review',
        safety_margin_applied: false
    }, Suggestion, MarkedSuggestion).

% Apply an LLM suggestion to a work item
% This creates a review task, does not directly mark as complete
apply_llm_suggestion(WorkItemID, Suggestion) :-
    % Create review task with 10% safety margin requirement
    review:create_review_task(WorkItemID, Suggestion),
    log_info('LLM suggestion applied, review task created'),
    !.

% Get all pending LLM suggestions that need review
get_llm_suggestions(Suggestions) :-
    review:get_tasks_needing_review(AllTasks),
    include(is_llm_generated, AllTasks, Suggestions).

is_llm_generated(Task) :-
    get_dict(source, Task, 'llm').

% ============================================================================
% Audit Logging
% ============================================================================

% Audit LLM interaction
audit_llm_interaction(Operation, InputPack, Response, Suggestions) :-
    get_time(Timestamp),
    
    % Generate prompt hash (not storing full prompt unless opted-in)
    config:get_config([llm, store_full_prompts], StoreFullPrompts, false),
    (StoreFullPrompts = true ->
        PromptData = InputPack
    ;
        hash_input_pack(InputPack, PromptHash),
        PromptData = _{hash: PromptHash}
    ),
    
    % Get model info
    config:get_config([llm, provider], Provider),
    config:get_config([llm, model], Model, 'default'),
    
    % Count suggestions
    length(Suggestions, SuggestionCount),
    
    % Create audit record
    AuditRecord = _{
        timestamp: Timestamp,
        operation: Operation,
        provider: Provider,
        model: Model,
        prompt_data: PromptData,
        suggestion_count: SuggestionCount,
        response_id: ResponseID
    },
    
    % Extract response ID if present
    (get_dict(response_id, Response, ResponseID) ; ResponseID = 'unknown'),
    
    % Store audit record
    audit_llm_call(Operation, Provider, Model, PromptData, AuditRecord),
    !.

% Store audit log
audit_llm_call(Operation, Provider, Model, _PromptData, AuditRecord) :-
    % Add to state audit log
    state:add_llm_audit_record(AuditRecord),
    
    % Log for transparency
    format(atom(Msg), 'LLM audit: ~w via ~w/~w', [Operation, Provider, Model]),
    log_info(Msg),
    !.

% Hash input pack for audit trail
hash_input_pack(InputPack, Hash) :-
    format(string(PackString), '~w', [InputPack]),
    sha_hash(PackString, HashBytes, [algorithm(sha256)]),
    hash_atom(HashBytes, Hash).
