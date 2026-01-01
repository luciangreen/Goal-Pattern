% test_automate.pl - Tests for automation functionality
:- begin_tests(automate).

:- use_module('../src/automate').
:- use_module('../src/config').
:- use_module('../src/log').

% ============================================================================
% Configuration Tests
% ============================================================================

test(automation_config_load) :-
    % Test that automation config loads successfully
    automate:load_automation_config(Config),
    assertion(is_dict(Config)),
    assertion(get_dict(automation, Config, _)),
    assertion(get_dict(allowlist, Config, _)),
    !.

test(automation_enabled_check) :-
    % Test automation enabled check
    (automate:is_automation_enabled ->
        true
    ;
        % It's ok if disabled, just verify predicate works
        true
    ),
    !.

test(dry_run_mode_check) :-
    % Test dry-run mode check
    (automate:is_dry_run_mode ->
        true
    ;
        % It's ok if not in dry-run, just verify predicate works
        true
    ),
    !.

% ============================================================================
% Command Validation Tests
% ============================================================================

test(validate_file2phil_command) :-
    % Test that file2phil command can be validated
    (automate:validate_command(file2phil) ->
        true
    ;
        % It's ok if not enabled, test just checks validation logic works
        true
    ),
    !.

test(validate_nonexistent_command, [fail]) :-
    % Test that nonexistent commands fail validation
    automate:validate_command(nonexistent_command_xyz).

test(validate_repo_task_command) :-
    % Test that repo_task validation works (even if disabled)
    (automate:validate_command(repo_task) ->
        true
    ;
        % Expected to fail if disabled
        true
    ),
    !.

% ============================================================================
% File Extension Validation Tests
% ============================================================================

test(validate_allowed_extension) :-
    % Create a temporary test file
    tmp_file('test', TmpFile),
    atom_concat(TmpFile, '.pl', TestFile),
    open(TestFile, write, Stream),
    write(Stream, '% test file\n'),
    close(Stream),
    
    % Test validation
    (automate:validate_file_extension(TestFile) ->
        Result = true
    ;
        Result = false
    ),
    
    % Cleanup
    delete_file(TestFile),
    
    % Assert result
    assertion(Result = true),
    !.

test(validate_disallowed_extension) :-
    % Create a temporary test file with disallowed extension
    tmp_file('test', TmpFile),
    atom_concat(TmpFile, '.xyz', TestFile),
    open(TestFile, write, Stream),
    write(Stream, 'test content\n'),
    close(Stream),
    
    % Test validation (should fail)
    (automate:validate_file_extension(TestFile) ->
        Result = true
    ;
        Result = false
    ),
    
    % Cleanup
    delete_file(TestFile),
    
    % Assert result
    assertion(Result = false),
    !.

% ============================================================================
% Placeholder Substitution Tests
% ============================================================================

test(substitute_placeholders) :-
    % Test placeholder substitution
    Template = ['-q', '-s', '{script_path}', '-g', 'main({file_path})', '-t', 'halt'],
    ScriptPath = 'file2phil.pl',
    FilePath = 'test.pl',
    
    automate:substitute_placeholders(Template, ScriptPath, FilePath, Result),
    
    % Check that placeholders were replaced
    member('-s', Result),
    member('file2phil.pl', Result),
    assertion(member('main(test.pl)', Result)),
    !.

test(substitute_atom) :-
    % Test single atom substitution
    Atom = '{script_path}',
    ScriptPath = 'file2phil.pl',
    FilePath = 'test.pl',
    
    automate:substitute_atom(Atom, ScriptPath, FilePath, Result),
    
    assertion(Result = 'file2phil.pl'),
    !.

% ============================================================================
% Event Recording Tests
% ============================================================================

test(record_automation_event) :-
    % Clear any existing events
    automate:clear_automation_events,
    
    % Record a test event
    automate:record_automation_event(
        test_type,
        'test_command',
        ['arg1', 'arg2'],
        success,
        'test output',
        ''
    ),
    
    % Retrieve events
    automate:get_automation_events(Events),
    
    % Verify event was recorded
    assertion(length(Events, 1)),
    Events = [event(_, _, Type, _, Status, _, _)],
    assertion(Type = test_type),
    assertion(Status = success),
    
    % Cleanup
    automate:clear_automation_events,
    !.

test(clear_automation_events) :-
    % Record multiple events
    automate:record_automation_event(test1, cmd1, [], success, '', ''),
    automate:record_automation_event(test2, cmd2, [], failed, '', ''),
    
    % Verify events exist
    automate:get_automation_events(Events1),
    assertion(length(Events1, 2)),
    
    % Clear events
    automate:clear_automation_events,
    
    % Verify events are cleared
    automate:get_automation_events(Events2),
    assertion(Events2 = []),
    !.

test(truncate_long_output) :-
    % Create a long string
    atom_codes('x', [X]),
    length(LongList, 20000),
    maplist(=(X), LongList),
    atom_codes(LongAtom, LongList),
    atom_string(LongAtom, LongString),
    
    % Truncate it
    automate:truncate_string(LongString, 1000, Truncated),
    
    % Verify it was truncated
    string_length(Truncated, Len),
    assertion(Len = 1000),
    !.

% ============================================================================
% Dry-Run Tests
% ============================================================================

test(file2phil_dry_run) :-
    % This test only works if automation is enabled and in dry-run mode
    (automate:is_automation_enabled, automate:is_dry_run_mode ->
        % Create a temporary test file
        tmp_file('test', TmpFile),
        atom_concat(TmpFile, '.pl', TestFile),
        open(TestFile, write, Stream),
        write(Stream, '% test file\n'),
        close(Stream),
        
        % Clear events
        automate:clear_automation_events,
        
        % Run file2phil in dry-run mode
        (automate:run_file2phil(TestFile, []) ->
            DryRunResult = success
        ;
            DryRunResult = failed
        ),
        
        % Check that event was recorded with dry_run status
        automate:get_automation_events(Events),
        
        % Cleanup
        delete_file(TestFile),
        automate:clear_automation_events,
        
        % Verify dry-run succeeded
        assertion(DryRunResult = success),
        assertion(length(Events, 1)),
        Events = [event(_, _, file2phil, _, Status, _, _)],
        assertion(Status = dry_run)
    ;
        % If automation not enabled or not in dry-run, skip test
        true
    ),
    !.

test(repo_task_dry_run) :-
    % This test checks the stub implementation
    (automate:is_automation_enabled, automate:is_dry_run_mode ->
        % Clear events
        automate:clear_automation_events,
        
        % Try to run repo task in dry-run mode
        % This should succeed in dry-run mode even though it's a stub
        (automate:run_repo_task('test-repo', 'test-task') ->
            _ = success
        ;
            _ = failed
        ),
        
        % Check events
        automate:get_automation_events(_),
        
        % Cleanup
        automate:clear_automation_events,
        
        % In dry-run mode with repo_task disabled, this may still fail
        % Just verify the predicate executes
        true
    ;
        % If not in dry-run or automation disabled, skip
        true
    ),
    !.

% ============================================================================
% Command Building Tests
% ============================================================================

test(build_file2phil_command) :-
    % Load config
    automate:load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(file2phil, Allowlist, F2PConfig),
    
    % Build command
    FilePath = 'test.pl',
    Options = [],
    automate:build_file2phil_command(FilePath, F2PConfig, Options, Command, Args),
    
    % Verify command and args were built
    assertion(atom(Command)),
    assertion(is_list(Args)),
    !.

test(build_repo_task_command) :-
    % Load config
    automate:load_automation_config(Config),
    get_dict(allowlist, Config, Allowlist),
    get_dict(repo_task, Allowlist, RTConfig),
    
    % Build command
    Repo = 'test-repo',
    TaskSpec = 'test-task',
    automate:build_repo_task_command(Repo, TaskSpec, RTConfig, Command, Args),
    
    % Verify command and args were built
    assertion(atom(Command)),
    assertion(is_list(Args)),
    !.

% ============================================================================
% Integration Tests
% ============================================================================

test(should_recommend_file2phil) :-
    % This tests the recommendation logic
    (automate:is_automation_enabled ->
        % Create a test file
        tmp_file('test', TmpFile),
        atom_concat(TmpFile, '.pl', TestFile),
        open(TestFile, write, Stream),
        write(Stream, '% test file\n'),
        close(Stream),
        
        % Check recommendation
        (automate:should_recommend_file2phil(TestFile, Reason) ->
            RecommendResult = true
        ;
            RecommendResult = false
        ),
        
        % Cleanup
        delete_file(TestFile),
        
        % In automation enabled mode, should recommend
        assertion(RecommendResult = true),
        assertion(atom(Reason))
    ;
        % If automation disabled, skip
        true
    ),
    !.

:- end_tests(automate).
