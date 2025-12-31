% gmail.pl - Gmail sent mail ingestion and work evidence extraction
:- module(gmail, [
    fetch_gmail_messages/0,
    fetch_gmail_messages/1,
    parse_gmail_json/2,
    extract_work_evidence/3,
    extract_schedule_changes/3,
    apply_evidence_rules/3,
    import_gmail_data/0
]).

:- use_module(library(readutil)).
:- use_module(library(http/json)).
:- use_module(library(process)).
:- use_module(library(pcre)).
:- use_module('../config').
:- use_module('../log').
:- use_module('../state').
:- use_module('../model').

% ============================================================================
% Main Import Interface
% ============================================================================

% Import Gmail data using configured settings
import_gmail_data :-
    log_info('Starting Gmail import...'),
    (fetch_gmail_messages ->
        log_info('Gmail import completed')
    ;
        log_error('Gmail import failed')
    ),
    !.

% Fetch Gmail messages using default configuration
fetch_gmail_messages :-
    config:get_config([gmail, to_address], ToAddress),
    config:get_config([gmail, days_back], DaysBack),
    config:get_config([gmail, max_results], MaxResults),
    fetch_gmail_messages(_{to: ToAddress, days: DaysBack, max: MaxResults}).

% Fetch Gmail messages with custom options
fetch_gmail_messages(Options) :-
    % Get helper script path
    config:get_config([gmail, helper_script], HelperScript),
    
    % Build command arguments
    build_fetch_command(HelperScript, Options, Args),
    
    % Execute helper and capture output
    format(atom(Msg), 'Executing: python3 ~w ~w', [HelperScript, Args]),
    log_debug(Msg),
    
    catch(
        (setup_call_cleanup(
            process_create(path(python3), [HelperScript | Args], 
                          [stdout(pipe(Out)), stderr(null), process(PID)]),
            read_string(Out, _, JSONString),
            (close(Out), process_wait(PID, _))
        ),
        parse_and_process_gmail_json(JSONString)),
        Error,
        (format(atom(ErrorMsg), 'Error fetching Gmail: ~w', [Error]),
         log_error(ErrorMsg),
         fail)
    ),
    !.

% Build fetch command from options
build_fetch_command(_HelperScript, Options, Args) :-
    Args0 = [],
    (get_dict(to, Options, To) -> 
        format(atom(ToArg), '--to=~w', [To]),
        append(Args0, [ToArg], Args1)
    ; Args1 = Args0),
    (get_dict(days, Options, Days) -> 
        format(atom(DaysArg), '--days=~w', [Days]),
        append(Args1, [DaysArg], Args2)
    ; Args2 = Args1),
    (get_dict(max, Options, Max) -> 
        format(atom(MaxArg), '--max=~w', [Max]),
        append(Args2, [MaxArg], Args3)
    ; Args3 = Args2),
    (get_dict(dry_run, Options, true) -> 
        append(Args3, ['--dry-run'], Args)
    ; Args = Args3).

% ============================================================================
% JSON Parsing
% ============================================================================

% Parse Gmail JSON and process messages
parse_and_process_gmail_json(JSONString) :-
    atom_string(JSONAtom, JSONString),
    atom_json_dict(JSONAtom, Data, []),
    get_dict(emails, Data, Emails),
    get_dict(count, Data, Count),
    format(atom(Msg), 'Received ~w emails from Gmail', [Count]),
    log_info(Msg),
    process_gmail_messages(Emails),
    !.

parse_gmail_json(JSONString, Emails) :-
    atom_string(JSONAtom, JSONString),
    atom_json_dict(JSONAtom, Data, []),
    get_dict(emails, Data, Emails).

% Process list of Gmail messages
process_gmail_messages([]).
process_gmail_messages([Email|Rest]) :-
    (process_single_email(Email) ->
        true
    ;
        format(atom(Msg), 'Failed to process email: ~w', [Email]),
        log_warn(Msg)
    ),
    process_gmail_messages(Rest).

% Process a single email message
process_single_email(Email) :-
    get_dict(message_id, Email, MessageID),
    get_dict(subject, Email, Subject),
    get_dict(snippet, Email, Snippet),
    get_dict(timestamp, Email, Timestamp),
    
    % Check if already processed
    atom_concat('gmail_', MessageID, EmailID),
    (find_existing_evidence(EmailID) ->
        format(atom(Msg), 'Skipping already processed email: ~w', [EmailID]),
        log_debug(Msg)
    ;
        % Extract work evidence
        extract_work_evidence(Subject, Snippet, WorkEvidence),
        store_work_evidence(WorkEvidence, EmailID, Timestamp),
        
        % Extract schedule changes
        extract_schedule_changes(Subject, Snippet, ScheduleChanges),
        store_schedule_changes(ScheduleChanges, EmailID, Timestamp)
    ),
    !.

% ============================================================================
% Evidence Extraction
% ============================================================================

% Extract work completion evidence from email text
extract_work_evidence(Subject, Snippet, Evidence) :-
    % Combine subject and snippet for analysis
    format(atom(FullText), '~w ~w', [Subject, Snippet]),
    
    % Load evidence rules
    load_evidence_rules(Rules),
    
    % Apply work completion patterns
    get_dict(work_completion_patterns, Rules, Patterns),
    apply_evidence_rules(FullText, Patterns, Evidence).

% Extract schedule change evidence from email text
extract_schedule_changes(Subject, Snippet, Changes) :-
    % Combine subject and snippet for analysis
    format(atom(FullText), '~w ~w', [Subject, Snippet]),
    
    % Load evidence rules
    load_evidence_rules(Rules),
    
    % Apply schedule change patterns
    get_dict(schedule_change_patterns, Rules, Patterns),
    apply_evidence_rules(FullText, Patterns, Changes).

% Apply evidence rules to text
apply_evidence_rules(_Text, [], []).
apply_evidence_rules(Text, [Pattern|Rest], Evidence) :-
    get_dict(pattern, Pattern, RegexPattern),
    get_dict(name, Pattern, Name),
    get_dict(type, Pattern, Type),
    get_dict(confidence, Pattern, Confidence),
    
    % Try to match pattern using PCRE library
    (catch(re_matchsub(RegexPattern, Text, Match, []), _, fail) ->
        % Extract count if present
        (get_dict('1', Match, CountMatch) ->
            % Convert to atom/number
            (atom(CountMatch) -> atom_number(CountMatch, Count) ; 
             string(CountMatch) -> atom_string(CountAtom, CountMatch), atom_number(CountAtom, Count) ;
             Count = CountMatch)
        ; get_dict(1, Match, CountMatch) ->
            (atom(CountMatch) -> atom_number(CountMatch, Count) ;
             string(CountMatch) -> atom_string(CountAtom, CountMatch), atom_number(CountAtom, Count) ;
             Count = CountMatch)
        ;
            Count = 1  % Default count if not specified
        ),
        
        % Extract work type if present
        (get_dict('2', Match, WorkTypeStr) ->
            normalize_work_type(WorkTypeStr, WorkType)
        ; get_dict(2, Match, WorkTypeStr) ->
            normalize_work_type(WorkTypeStr, WorkType)
        ;
            WorkType = unknown
        ),
        
        NewEvidence = evidence(Name, Type, Count, WorkType, Confidence, Match),
        apply_evidence_rules(Text, Rest, RestEvidence),
        Evidence = [NewEvidence|RestEvidence]
    ;
        apply_evidence_rules(Text, Rest, Evidence)
    ).

% Normalize work type from matched text
normalize_work_type(TypeStr, NormalizedType) :-
    load_evidence_rules(Rules),
    get_dict(work_type_mapping, Rules, Mapping),
    % Convert to atom if string
    (atom(TypeStr) -> TypeAtom = TypeStr ; atom_string(TypeAtom, TypeStr)),
    downcase_atom(TypeAtom, LowerType),
    (get_dict(LowerType, Mapping, NormalizedType) ->
        true
    ;
        NormalizedType = unknown
    ).

% ============================================================================
% Evidence Storage
% ============================================================================

% Store work evidence as work items
store_work_evidence([], _EmailID, _Timestamp).
store_work_evidence([Evidence|Rest], EmailID, Timestamp) :-
    Evidence = evidence(_Name, Type, Count, WorkType, Confidence, _Match),
    
    % Only store if work type is known and confidence meets threshold
    load_evidence_rules(Rules),
    get_dict(extraction_settings, Rules, Settings),
    get_dict(min_confidence, Settings, MinConfidence),
    
    (Confidence >= MinConfidence, member(WorkType, [algorithm, philosophy]) ->
        % Create or update work item
        create_work_evidence_item(EmailID, Type, Count, WorkType, Confidence, Timestamp),
        format(atom(Msg), 'Stored work evidence: ~w ~w (~w)', [Count, WorkType, Type]),
        log_info(Msg)
    ;
        true
    ),
    
    store_work_evidence(Rest, EmailID, Timestamp).

% Create work evidence item
create_work_evidence_item(EmailID, EvidenceType, Count, WorkType, Confidence, Timestamp) :-
    % Generate unique ID
    get_time(Now),
    format(atom(ItemID), 'gmail_work_~w_~w', [EmailID, Now]),
    
    % Determine status based on evidence type
    (EvidenceType = submission -> Status = submitted ; Status = complete),
    
    % Create timestamps dict
    Timestamps = _{
        created: Timestamp,
        modified: Timestamp,
        completed: Timestamp
    },
    
    % Create work item
    model:create_work_item(ItemID, WorkType, EmailID, Status, Count, 
                          Timestamps, [gmail, evidence], Confidence, WorkItem),
    
    % Store in state
    state:add_work_item(WorkItem).

% Store schedule changes
store_schedule_changes([], _EmailID, _Timestamp).
store_schedule_changes([Change|Rest], EmailID, Timestamp) :-
    Change = evidence(Name, Type, _Count, _WorkType, Confidence, Match),
    
    format(atom(Msg), 'Detected schedule change: ~w (~w, confidence: ~2f)', 
           [Name, Type, Confidence]),
    log_info(Msg),
    
    % Store as a note or update existing events
    % This is a simplified implementation - could be enhanced to actually modify schedule
    format(atom(ChangeID), 'gmail_schedule_~w_~w', [EmailID, Name]),
    record_schedule_change(ChangeID, Type, Match, Timestamp, Confidence),
    
    store_schedule_changes(Rest, EmailID, Timestamp).

% Record schedule change (simplified - stores metadata)
record_schedule_change(ChangeID, Type, _Match, Timestamp, Confidence) :-
    % For now, just log the schedule change
    % In a full implementation, this would update actual schedule events
    format(atom(Msg), 'Schedule change recorded: ~w (~w) at ~w with confidence ~2f', 
           [ChangeID, Type, Timestamp, Confidence]),
    log_info(Msg),
    !.

% ============================================================================
% Helper Predicates
% ============================================================================

% Load evidence rules from JSON file
load_evidence_rules(Rules) :-
    config:get_config([gmail, evidence_rules_file], RulesFile),
    exists_file(RulesFile),
    open(RulesFile, read, Stream),
    json_read_dict(Stream, Rules),
    close(Stream),
    !.

load_evidence_rules(_{
    work_completion_patterns: [],
    schedule_change_patterns: [],
    work_type_mapping: _{},
    extraction_settings: _{min_confidence: 0.7}
}) :-
    log_warn('Could not load evidence rules, using defaults').

% Find existing evidence by email ID
find_existing_evidence(EmailID) :-
    state:get_work_items(WorkItems),
    member(work_item(_, _, EmailID, _, _, _, _, _), WorkItems),
    !.

% Default case - no existing evidence found
find_existing_evidence(_) :- fail.
