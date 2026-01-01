% automate.pl - Automation hooks: file2phil.pl + GitHub Copilot Agent actions
:- module(automate, [
    run_file2phil/2,
    run_repo_task/2,
    is_automation_enabled/0,
    is_dry_run_mode/0,
    get_automation_events/1,
    clear_automation_events/0,
    validate_command/1
]).

:- use_module(library(process)).
:- use_module(library(http/json)).
:- use_module(library(readutil)).
:- use_module(library(time)).
:- use_module(config).
:- use_module(log).
:- use_module(state).

:- discontiguous execute_file2phil/4.

% ============================================================================
% Dynamic state for automation events
% ============================================================================

:- dynamic automation_event/7.  % (ID, Timestamp, Type, Command, Status, Output, Error)

% ============================================================================
% Configuration Access
% ============================================================================

% Check if automation is enabled
is_automation_enabled :-
    load_automation_config(Config),
    get_dict(automation, Config, AutoConfig),
    get_dict(enabled, AutoConfig, true),
    !.

% Check if dry-run mode is active
is_dry_run_mode :-
    load_automation_config(Config),
    get_dict(automation, Config, AutoConfig),
    get_dict(dry_run, AutoConfig, true),
    !.

% Load automation configuration
load_automation_config(Config) :-
    config:config_path(BaseConfigPath),
    atom_concat(BaseDir, '/config.json', BaseConfigPath),
    atom_concat(BaseDir, '/automation_allowlist.json', AutomationConfigPath),
    (exists_file(AutomationConfigPath) ->
        open(AutomationConfigPath, read, Stream),
        json_read_dict(Stream, Config),
        close(Stream)
    ; 
        log:log_error('Automation config file not found'),
        fail
    ).

% ============================================================================
% Command Validation
% ============================================================================

% Validate that a command is allowlisted
validate_command(CommandType) :-
    load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(CommandType, Allowlist, CommandConfig),
    get_dict(enabled, CommandConfig, true),
    !.

validate_command(CommandType) :-
    format(atom(Msg), 'Command type ~w is not allowlisted or enabled', [CommandType]),
    log:log_error(Msg),
    fail.

% ============================================================================
% File2Phil Automation
% ============================================================================

% Run file2phil.pl on a given file
% run_file2phil(+FilePath, +Options)
% Options: List with optional script_path(Path) and additional_args(Args)
% Note: timeout is controlled by config, not options
run_file2phil(FilePath, Options) :-
    log:log_info('Starting file2phil automation...'),
    
    % Check if automation is enabled
    (is_automation_enabled ->
        true
    ;
        log:log_error('Automation is not enabled in config'),
        fail
    ),
    
    % Validate command
    (validate_command(file2phil) ->
        true
    ;
        log:log_error('file2phil command is not allowlisted'),
        fail
    ),
    
    % Validate file exists
    (exists_file(FilePath) ->
        true
    ;
        format(atom(Msg), 'File does not exist: ~w', [FilePath]),
        log:log_error(Msg),
        fail
    ),
    
    % Validate file extension
    (validate_file_extension(FilePath) ->
        true
    ;
        log:log_error('File extension not allowed'),
        fail
    ),
    
    % Build command
    load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(file2phil, Allowlist, F2PConfig),
    build_file2phil_command(FilePath, F2PConfig, Options, Command, Args),
    
    % Check dry-run mode
    (is_dry_run_mode ->
        % Dry run - just log what would be executed
        format(atom(DryRunMsg), 'DRY RUN: Would execute: ~w ~w', [Command, Args]),
        log:log_info(DryRunMsg),
        record_automation_event(file2phil, Command, Args, dry_run, '', ''),
        log:log_info('File2phil dry-run completed')
    ;
        % Execute the command
        execute_file2phil(FilePath, Command, Args, F2PConfig),
        log:log_info('File2phil execution completed')
    ).

% Validate file extension against allowlist
validate_file_extension(FilePath) :-
    load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(file2phil, Allowlist, F2PConfig),
    get_dict(allowed_file_extensions, F2PConfig, AllowedExts),
    file_name_extension(_, Ext, FilePath),
    atom_concat('.', Ext, ExtWithDot),
    member(ExtWithDot, AllowedExts),
    !.

% Build command and arguments for file2phil
build_file2phil_command(FilePath, F2PConfig, Options, Command, FinalArgs) :-
    % Get script path
    (member(script_path(ScriptPath), Options) ->
        true
    ;
        get_dict(script_path_default, F2PConfig, ScriptPath)
    ),
    
    % Get command
    get_dict(command, F2PConfig, CommandRaw),
    (atom(CommandRaw) -> Command = CommandRaw ; atom_string(Command, CommandRaw)),
    
    % Get args template
    get_dict(args_template, F2PConfig, ArgsTemplate),
    
    % Replace placeholders
    substitute_placeholders(ArgsTemplate, ScriptPath, FilePath, SubstitutedArgs),
    
    % Add any additional args
    (member(additional_args(AddArgs), Options) ->
        append(SubstitutedArgs, AddArgs, FinalArgs)
    ;
        FinalArgs = SubstitutedArgs
    ).

% Substitute placeholders in args template
substitute_placeholders([], _, _, []).
substitute_placeholders([Arg|Rest], ScriptPath, FilePath, [SubArg|SubRest]) :-
    (atom(Arg) ->
        substitute_atom(Arg, ScriptPath, FilePath, SubArg)
    ;
        SubArg = Arg
    ),
    substitute_placeholders(Rest, ScriptPath, FilePath, SubRest).

% Substitute placeholders in a single atom
substitute_atom(Arg, ScriptPath, FilePath, SubArg) :-
    atom_string(Arg, ArgStr),
    % Replace {script_path}
    replace_placeholder(ArgStr, '{script_path}', ScriptPath, TmpStr1),
    % Replace {file_path}
    replace_placeholder(TmpStr1, '{file_path}', FilePath, SubStr),
    atom_string(SubArg, SubStr),
    !.
substitute_atom(Arg, _, _, Arg).  % No substitution needed

% Helper to replace a placeholder in a string
replace_placeholder(Input, Placeholder, Replacement, Output) :-
    atom_string(Replacement, ReplacementStr),
    atom_string(Placeholder, PlaceholderStr),
    % Find the placeholder in the input string
    (sub_string(Input, Before, Length, After, PlaceholderStr) ->
        % Found it - split and rebuild
        sub_string(Input, 0, Before, _, BeforeStr),
        AfterStart is Before + Length,
        sub_string(Input, AfterStart, After, 0, AfterStr),
        % Recursively replace in the after part
        replace_placeholder(AfterStr, Placeholder, Replacement, AfterReplaced),
        % Combine: before + replacement + after
        atomics_to_string([BeforeStr, ReplacementStr, AfterReplaced], Output)
    ;
        % Not found - return input as-is
        Output = Input
    ),
    !.
replace_placeholder(Input, _, _, Input).  % Fallback

% Execute file2phil command
execute_file2phil(FilePath, Command, Args, F2PConfig) :-
    get_dict(timeout_seconds, F2PConfig, Timeout),
    
    % Create process
    format(atom(LogMsg), 'Executing: ~w ~w (timeout: ~w seconds)', [Command, Args, Timeout]),
    log:log_info(LogMsg),
    
    % Execute with timeout using call_with_time_limit
    catch(
        call_with_time_limit(
            Timeout,
            execute_file2phil_process(FilePath, Command, Args)
        ),
        time_limit_exceeded,
        (
            format(atom(TimeoutMsg), 'file2phil execution timed out after ~w seconds for ~w', [Timeout, FilePath]),
            log:log_error(TimeoutMsg),
            record_automation_event(file2phil, Command, Args, timeout, '', 'Time limit exceeded'),
            fail
        )
    ).

% Helper predicate to execute the actual process
execute_file2phil_process(FilePath, Command, Args) :-
    process_create(path(Command), Args, [
        stdout(pipe(Out)),
        stderr(pipe(Err)),
        process(PID)
    ]),
    
    % Read output
    read_string(Out, _, OutputStr),
    read_string(Err, _, ErrorStr),
    
    % Wait for process
    process_wait(PID, Status),
    
    % Record event
    (Status = exit(0) ->
        EventStatus = success
    ;
        EventStatus = failed
    ),
    
    record_automation_event(file2phil, Command, Args, EventStatus, OutputStr, ErrorStr),
    
    % Log results
    (EventStatus = success ->
        format(atom(SuccMsg), 'file2phil completed successfully for ~w', [FilePath]),
        log:log_info(SuccMsg),
        log:log_info(OutputStr)
    ;
        format(atom(ErrMsg), 'file2phil failed for ~w: ~w', [FilePath, ErrorStr]),
        log:log_error(ErrMsg)
    ),
    !.

% Catch other exceptions during execution
execute_file2phil(_, Command, Args, _) :-
    catch(
        fail,
        Error,
        (
            format(atom(ExMsg), 'Exception during file2phil execution: ~w', [Error]),
            log:log_error(ExMsg),
            record_automation_event(file2phil, Command, Args, error, '', Error),
            fail
        )
    ).

% ============================================================================
% Repository Task Automation (Stub for GitHub Copilot Agent)
% ============================================================================

% Run a repository task via GitHub Copilot Agent
% run_repo_task(+Repo, +TaskSpec)
% This is a stub for future implementation
run_repo_task(Repo, TaskSpec) :-
    log:log_info('Starting repo task automation (stub)...'),
    
    % Validate command
    (validate_command(repo_task) ->
        true
    ;
        log:log_warn('repo_task command is not enabled - this is a stub implementation'),
        fail
    ),
    
    % Build command
    load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(repo_task, Allowlist, RTConfig),
    build_repo_task_command(Repo, TaskSpec, RTConfig, Command, Args),
    
    % Check dry-run mode
    (is_dry_run_mode ->
        % Dry run - just log what would be executed
        format(atom(DryRunMsg), 'DRY RUN: Would execute: ~w ~w for repo ~w with task ~w', 
               [Command, Args, Repo, TaskSpec]),
        log:log_info(DryRunMsg),
        record_automation_event(repo_task, Command, Args, dry_run, '', ''),
        log:log_info('Repo task dry-run completed')
    ;
        % In live mode, still treat as stub
        log:log_warn('Repo task execution is not yet implemented'),
        format(atom(StubMsg), 'STUB: Would execute repo task on ~w: ~w', [Repo, TaskSpec]),
        log:log_info(StubMsg),
        record_automation_event(repo_task, Command, Args, stub, StubMsg, ''),
        fail
    ).

% Build command and arguments for repo task
build_repo_task_command(Repo, TaskSpec, RTConfig, Command, FinalArgs) :-
    get_dict(command, RTConfig, CommandRaw),
    (atom(CommandRaw) -> Command = CommandRaw ; atom_string(Command, CommandRaw)),
    get_dict(args_template, RTConfig, ArgsTemplate),
    
    % Replace placeholders
    substitute_repo_placeholders(ArgsTemplate, Repo, TaskSpec, FinalArgs).

% Substitute placeholders for repo task
substitute_repo_placeholders([], _, _, []).
substitute_repo_placeholders([Arg|Rest], Repo, TaskSpec, [SubArg|SubRest]) :-
    (atom(Arg) ->
        substitute_repo_atom(Arg, Repo, TaskSpec, SubArg)
    ;
        SubArg = Arg
    ),
    substitute_repo_placeholders(Rest, Repo, TaskSpec, SubRest).

% Substitute placeholders for repo task atom
substitute_repo_atom(Arg, Repo, TaskSpec, SubArg) :-
    atom_string(Arg, ArgStr),
    atom_string(Repo, RepoStr),
    atom_string(TaskSpec, TaskStr),
    % Replace {repo}
    replace_placeholder(ArgStr, '{repo}', RepoStr, TmpStr1),
    % Replace {task_spec}
    replace_placeholder(TmpStr1, '{task_spec}', TaskStr, SubStr),
    atom_string(SubArg, SubStr),
    !.
substitute_repo_atom(Arg, _, _, Arg).  % No substitution needed

% ============================================================================
% Event Recording
% ============================================================================

% Record an automation event
record_automation_event(Type, Command, Args, Status, Output, Error) :-
    load_automation_config(Config),
    get_dict(recording, Config, RecConfig),
    get_dict(store_commands, RecConfig, true),
    
    % Generate event ID
    get_time(Timestamp),
    format(atom(ID), 'auto_~w_~w', [Type, Timestamp]),
    
    % Truncate output if needed
    get_dict(max_output_length, RecConfig, MaxLen),
    truncate_string(Output, MaxLen, TruncOutput),
    truncate_string(Error, MaxLen, TruncError),
    
    % Store event
    assertz(automation_event(ID, Timestamp, Type, Command-Args, Status, TruncOutput, TruncError)),
    
    % Log event
    format(atom(EventMsg), 'Recorded automation event: ~w (~w)', [ID, Status]),
    log:log_info(EventMsg),
    !.

record_automation_event(_, _, _, _, _, _) :-
    % Recording disabled, silently succeed
    !.

% Truncate string to max length
truncate_string(Str, MaxLen, Truncated) :-
    string(Str),
    string_length(Str, Len),
    (Len > MaxLen ->
        sub_string(Str, 0, MaxLen, _, Truncated)
    ;
        Truncated = Str
    ),
    !.
truncate_string(Str, _, Str).  % Not a string or already short

% Get all automation events
get_automation_events(Events) :-
    findall(
        event(ID, Timestamp, Type, Command, Status, Output, Error),
        automation_event(ID, Timestamp, Type, Command, Status, Output, Error),
        Events
    ).

% Clear all automation events
clear_automation_events :-
    retractall(automation_event(_, _, _, _, _, _, _)),
    log:log_info('Cleared all automation events').

% ============================================================================
% Integration Points
% ============================================================================

% Check if automation should recommend running file2phil
should_recommend_file2phil(FilePath, Reason) :-
    % Check if automation is enabled
    is_automation_enabled,
    
    % Check if file exists and is valid
    exists_file(FilePath),
    validate_file_extension(FilePath),
    
    % Check if it would improve goals (stub logic)
    % In real implementation, this would check against goal progress
    format(atom(Reason), 'Running file2phil on ~w may improve algorithm completion rate', [FilePath]).

% ============================================================================
% Utility Predicates
% ============================================================================

% (Utility predicates are integrated into the module above)
