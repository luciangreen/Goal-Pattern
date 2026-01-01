% review.pl - Review workflow with 10% safety margin enforcement
:- module(review, [
    create_review_task/2,
    get_tasks_needing_review/1,
    verify_work/2,
    check_safety_margin_met/1,
    complete_review/2,
    get_review_status/2,
    calculate_required_extra_work/2
]).

:- use_module(library(apply)).
:- use_module('log').
:- use_module('state').
:- use_module('model').

% ============================================================================
% Review Task Management
% ============================================================================

% Create a review task from an LLM suggestion
% create_review_task(+WorkItemID, +Suggestion)
create_review_task(WorkItemID, Suggestion) :-
    % Generate unique task ID
    get_time(Timestamp),
    format(atom(TaskID), 'review_~w_~w', [WorkItemID, Timestamp]),
    
    % Extract counts from suggestion
    (get_dict(word_count, Suggestion, WordCount) -> true ; WordCount = 0),
    (get_dict(clause_count, Suggestion, ClauseCount) -> true ; ClauseCount = 0),
    BaseCount is max(WordCount, ClauseCount),
    
    % Calculate required extra work (10% safety margin)
    calculate_required_extra_work(BaseCount, RequiredExtraWork),
    
    % Create task structure
    get_dict(text, Suggestion, Text),
    (get_dict(confidence, Suggestion, Confidence) -> true ; Confidence = 0.7),
    (get_dict(workflow_step, Suggestion, WorkflowStep) -> true ; WorkflowStep = 'general_review'),
    
    Task = _{
        task_id: TaskID,
        work_item_id: WorkItemID,
        source: 'llm',
        status: 'needs_review',
        created_at: Timestamp,
        llm_generated_count: BaseCount,
        required_extra_work: RequiredExtraWork,
        verified_extra_work: 0,
        workflow_step: WorkflowStep,
        confidence: Confidence,
        suggestion_text: Text,
        verification_notes: [],
        safety_margin_met: false
    },
    
    % Store in state
    state:add_review_task(Task),
    
    format(atom(Msg), 'Created review task ~w for work item ~w (requires +10% = ~w extra units)',
           [TaskID, WorkItemID, RequiredExtraWork]),
    log_info(Msg),
    !.

% Get all tasks that need review
get_tasks_needing_review(Tasks) :-
    state:get_review_tasks(AllTasks),
    include(needs_review, AllTasks, Tasks).

needs_review(Task) :-
    get_dict(status, Task, 'needs_review').

% ============================================================================
% Safety Margin Enforcement (10% Rule)
% ============================================================================

% Calculate required extra work (10% of base count)
% calculate_required_extra_work(+BaseCount, -RequiredExtra)
calculate_required_extra_work(BaseCount, RequiredExtra) :-
    number(BaseCount),
    RequiredExtra is ceiling(BaseCount * 0.1),
    !.

% Check if safety margin requirement has been met for a task
% check_safety_margin_met(+TaskID)
check_safety_margin_met(TaskID) :-
    state:get_review_task(TaskID, Task),
    get_dict(required_extra_work, Task, Required),
    get_dict(verified_extra_work, Task, Verified),
    Verified >= Required,
    !.

% ============================================================================
% Verification Workflow
% ============================================================================

% Verify work done beyond LLM output
% verify_work(+TaskID, +VerificationData)
verify_work(TaskID, VerificationData) :-
    state:get_review_task(TaskID, Task),
    
    % Extract verification details
    get_dict(extra_work_count, VerificationData, ExtraWorkCount),
    (get_dict(verification_notes, VerificationData, Notes) -> true ; Notes = "User verified"),
    (get_dict(verifier, VerificationData, Verifier) -> true ; Verifier = 'user'),
    
    % Update verified extra work
    get_dict(verified_extra_work, Task, CurrentVerified),
    NewVerified is CurrentVerified + ExtraWorkCount,
    
    % Check if safety margin now met
    get_dict(required_extra_work, Task, Required),
    SafetyMarginMet = (NewVerified >= Required),
    
    % Determine new status
    (SafetyMarginMet = true ->
        NewStatus = 'verified'
    ;
        NewStatus = 'needs_review'
    ),
    
    % Update task
    get_time(Timestamp),
    get_dict(verification_notes, Task, OldNotes),
    NewNote = _{
        timestamp: Timestamp,
        verifier: Verifier,
        extra_work: ExtraWorkCount,
        notes: Notes
    },
    append(OldNotes, [NewNote], AllNotes),
    
    put_dict(_{
        verified_extra_work: NewVerified,
        safety_margin_met: SafetyMarginMet,
        status: NewStatus,
        verification_notes: AllNotes,
        last_verified_at: Timestamp
    }, Task, UpdatedTask),
    
    % Save updated task
    state:update_review_task(TaskID, UpdatedTask),
    
    format(atom(Msg), 'Verified ~w extra units for task ~w (total: ~w/~w, margin met: ~w)',
           [ExtraWorkCount, TaskID, NewVerified, Required, SafetyMarginMet]),
    log_info(Msg),
    !.

% Complete review and promote work item to complete status
% complete_review(+TaskID, +CompletionData)
complete_review(TaskID, CompletionData) :-
    % Verify safety margin is met
    check_safety_margin_met(TaskID),
    
    % Get task and work item
    state:get_review_task(TaskID, Task),
    get_dict(work_item_id, Task, WorkItemID),
    state:get_work_item(WorkItemID, WorkItem),
    
    % Update work item status to complete
    WorkItem = work_item(ID, Type, Origin, _OldStatus, Count, Timestamps, Tags, Confidence),
    
    % Calculate final count with safety margin applied
    get_dict(llm_generated_count, Task, LLMCount),
    get_dict(verified_extra_work, Task, ExtraWork),
    FinalCount is Count + LLMCount + ExtraWork,
    
    % Create updated work item
    get_time(CompletedTime),
    put_dict(completed, Timestamps, CompletedTime, NewTimestamps),
    
    create_work_item(ID, Type, Origin, complete, FinalCount, NewTimestamps, 
                    Tags, Confidence, UpdatedWorkItem),
    
    % Add completion metadata
    (get_dict(completion_notes, CompletionData, CompletionNotes) -> true ; CompletionNotes = "Review completed with safety margin"),
    
    % Update work item in state
    state:update_work_item(ID, UpdatedWorkItem),
    
    % Mark review task as completed
    put_dict(_{
        status: 'completed',
        completed_at: CompletedTime,
        completion_notes: CompletionNotes,
        final_count: FinalCount
    }, Task, CompletedTask),
    
    state:update_review_task(TaskID, CompletedTask),
    
    format(atom(Msg), 'Completed review ~w: work item ~w promoted to complete with count ~w',
           [TaskID, WorkItemID, FinalCount]),
    log_info(Msg),
    !.

% Fail if safety margin not met
complete_review(TaskID, _CompletionData) :-
    \+ check_safety_margin_met(TaskID),
    state:get_review_task(TaskID, Task),
    get_dict(required_extra_work, Task, Required),
    get_dict(verified_extra_work, Task, Verified),
    Remaining is Required - Verified,
    format(atom(Msg), 'Cannot complete review ~w: safety margin not met. Need ~w more units.',
           [TaskID, Remaining]),
    log_error(Msg),
    fail.

% ============================================================================
% Review Status Queries
% ============================================================================

% Get review status for a work item
% get_review_status(+WorkItemID, -Status)
get_review_status(WorkItemID, Status) :-
    state:get_review_tasks(AllTasks),
    include(for_work_item(WorkItemID), AllTasks, WorkItemTasks),
    (WorkItemTasks = [] ->
        Status = _{has_reviews: false}
    ;
        summarize_review_status(WorkItemTasks, Status)
    ).

for_work_item(WorkItemID, Task) :-
    get_dict(work_item_id, Task, WorkItemID).

% Summarize review status from multiple tasks
summarize_review_status(Tasks, Status) :-
    length(Tasks, TotalTasks),
    include(task_is_completed, Tasks, CompletedTasks),
    include(task_needs_review, Tasks, NeedsReviewTasks),
    include(task_is_verified, Tasks, VerifiedTasks),
    length(CompletedTasks, CompletedCount),
    length(NeedsReviewTasks, NeedsReviewCount),
    length(VerifiedTasks, VerifiedCount),
    
    % Calculate total safety margin requirements
    maplist(get_required_extra, Tasks, RequiredList),
    sum_list(RequiredList, TotalRequired),
    maplist(get_verified_extra, Tasks, VerifiedList),
    sum_list(VerifiedList, TotalVerified),
    
    Status = _{
        has_reviews: true,
        total_tasks: TotalTasks,
        completed: CompletedCount,
        verified: VerifiedCount,
        needs_review: NeedsReviewCount,
        total_required_extra: TotalRequired,
        total_verified_extra: TotalVerified,
        safety_margin_met: (TotalVerified >= TotalRequired)
    }.

task_is_completed(Task) :-
    get_dict(status, Task, 'completed').

task_needs_review(Task) :-
    get_dict(status, Task, 'needs_review').

task_is_verified(Task) :-
    get_dict(status, Task, 'verified').

get_required_extra(Task, Required) :-
    get_dict(required_extra_work, Task, Required).

get_verified_extra(Task, Verified) :-
    get_dict(verified_extra_work, Task, Verified).
