% test_disk_scan.pl - Tests for disk scanning functionality
:- begin_tests(disk_scan).

:- use_module('../src/config').
:- use_module('../src/log').
:- use_module('../src/state').
:- use_module('../src/model').
:- use_module('../src/modules/disk_scan').

% Test setup
setup_test_env :-
    config:config_path(ConfigPath),
    config:load_config(ConfigPath),
    log:init_log,
    state:init_state,
    % Clean up test directory if it exists
    (exists_directory('/tmp/test_disk_scan') -> 
        delete_directory_and_contents('/tmp/test_disk_scan') 
    ; true),
    make_directory('/tmp/test_disk_scan'),
    make_directory('/tmp/test_disk_scan/algorithms'),
    make_directory('/tmp/test_disk_scan/philosophies').

% Test teardown
cleanup_test_env :-
    (exists_directory('/tmp/test_disk_scan') -> 
        delete_directory_and_contents('/tmp/test_disk_scan')
    ; true),
    log:close_log,
    config:get_config([state, file_path], StatePath),
    (exists_file(StatePath) -> delete_file(StatePath) ; true),
    state:clear_state.

% ============================================================================
% Clause Counting Tests
% ============================================================================

test(count_clauses_empty_file, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create empty Prolog file
    FilePath = '/tmp/test_disk_scan/algorithms/empty.pl',
    open(FilePath, write, Stream),
    close(Stream),
    
    % Count clauses
    disk_scan:count_prolog_clauses(FilePath, Count),
    assertion(Count =:= 0),
    !.

test(count_clauses_single_fact, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create Prolog file with one fact
    FilePath = '/tmp/test_disk_scan/algorithms/single_fact.pl',
    open(FilePath, write, Stream),
    write(Stream, 'fact(value).\n'),
    close(Stream),
    
    % Count clauses
    disk_scan:count_prolog_clauses(FilePath, Count),
    assertion(Count =:= 1),
    !.

test(count_clauses_multiple_clauses, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create Prolog file with multiple clauses
    FilePath = '/tmp/test_disk_scan/algorithms/multiple.pl',
    open(FilePath, write, Stream),
    write(Stream, 'fact1(a).\n'),
    write(Stream, 'fact2(b).\n'),
    write(Stream, 'rule(X) :- fact1(X).\n'),
    write(Stream, 'rule2(X, Y) :- fact1(X), fact2(Y).\n'),
    close(Stream),
    
    % Count clauses
    disk_scan:count_prolog_clauses(FilePath, Count),
    assertion(Count =:= 4),
    !.

test(count_clauses_with_comments, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create Prolog file with comments
    FilePath = '/tmp/test_disk_scan/algorithms/with_comments.pl',
    open(FilePath, write, Stream),
    write(Stream, '% This is a comment\n'),
    write(Stream, 'fact(a).\n'),
    write(Stream, '/* Block comment */\n'),
    write(Stream, 'rule(X) :- fact(X).\n'),
    close(Stream),
    
    % Count clauses (comments shouldn't be counted)
    disk_scan:count_prolog_clauses(FilePath, Count),
    assertion(Count >= 2),
    !.

% ============================================================================
% Word Counting Tests
% ============================================================================

test(count_words_empty_file, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create empty text file
    FilePath = '/tmp/test_disk_scan/philosophies/empty.txt',
    open(FilePath, write, Stream),
    close(Stream),
    
    % Count words
    disk_scan:count_words(FilePath, Count),
    assertion(Count =:= 0),
    !.

test(count_words_single_word, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create text file with one word
    FilePath = '/tmp/test_disk_scan/philosophies/single.txt',
    open(FilePath, write, Stream),
    write(Stream, 'Hello'),
    close(Stream),
    
    % Count words
    disk_scan:count_words(FilePath, Count),
    assertion(Count =:= 1),
    !.

test(count_words_multiple_words, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create text file with multiple words
    FilePath = '/tmp/test_disk_scan/philosophies/multiple.txt',
    open(FilePath, write, Stream),
    write(Stream, 'Hello world this is a test'),
    close(Stream),
    
    % Count words
    disk_scan:count_words(FilePath, Count),
    assertion(Count =:= 6),
    !.

test(count_words_multiline, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create text file with multiple lines
    FilePath = '/tmp/test_disk_scan/philosophies/multiline.txt',
    open(FilePath, write, Stream),
    write(Stream, 'First line with words\n'),
    write(Stream, 'Second line with more words\n'),
    write(Stream, 'Third line\n'),
    close(Stream),
    
    % Count words (11 total: 4 + 5 + 2)
    disk_scan:count_words(FilePath, Count),
    assertion(Count =:= 11),
    !.

% ============================================================================
% Classification Tests
% ============================================================================

test(classify_algorithm_draft, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(algorithms, 5, Status),
    assertion(Status == draft),
    !.

test(classify_algorithm_partial, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(algorithms, 40, Status),
    assertion(Status == partial),
    !.

test(classify_algorithm_complete, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(algorithms, 100, Status),
    assertion(Status == complete),
    !.

test(classify_philosophy_draft, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(philosophies, 30, Status),
    assertion(Status == draft),
    !.

test(classify_philosophy_partial, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(philosophies, 250, Status),
    assertion(Status == partial),
    !.

test(classify_philosophy_complete, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    disk_scan:classify_completion(philosophies, 600, Status),
    assertion(Status == complete),
    !.

% ============================================================================
% File Scanning Tests
% ============================================================================

test(scan_algorithm_file, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create test algorithm file
    FilePath = '/tmp/test_disk_scan/algorithms/test_alg.pl',
    open(FilePath, write, Stream),
    write(Stream, 'test_fact(a).\n'),
    write(Stream, 'test_rule(X) :- test_fact(X).\n'),
    close(Stream),
    
    % Scan the file
    disk_scan:scan_file(FilePath, algorithms),
    
    % Verify work item was created
    state:get_work_items([WorkItem|_]),
    WorkItem = work_item(_, algorithm, FilePath, Status, Count, _, _, _),
    assertion(Count =:= 2),
    assertion(member(Status, [draft, partial, complete])),
    !.

test(scan_philosophy_file, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create test philosophy file
    FilePath = '/tmp/test_disk_scan/philosophies/test_phil.txt',
    open(FilePath, write, Stream),
    write(Stream, 'This is a test philosophy document with several words in it.'),
    close(Stream),
    
    % Scan the file
    disk_scan:scan_file(FilePath, philosophies),
    
    % Verify work item was created
    state:get_work_items([WorkItem|_]),
    WorkItem = work_item(_, philosophy, FilePath, Status, Count, _, _, _),
    assertion(Count > 0),
    assertion(member(Status, [draft, partial, complete])),
    !.

% ============================================================================
% Delta Detection Tests
% ============================================================================

test(rescan_detects_no_delta, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create test file
    FilePath = '/tmp/test_disk_scan/algorithms/delta_test.pl',
    open(FilePath, write, Stream),
    write(Stream, 'fact(a).\n'),
    close(Stream),
    
    % First scan
    disk_scan:scan_file(FilePath, algorithms),
    state:get_work_items([FirstItem|_]),
    FirstItem = work_item(_, _, _, _, FirstCount, _, _, _),
    
    % Second scan without changes
    disk_scan:scan_file(FilePath, algorithms),
    state:get_work_items([SecondItem|_]),
    SecondItem = work_item(_, _, _, _, SecondCount, _, _, _),
    
    % Counts should be the same
    assertion(FirstCount =:= SecondCount),
    !.

test(rescan_detects_positive_delta, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create test file
    FilePath = '/tmp/test_disk_scan/algorithms/delta_test2.pl',
    open(FilePath, write, Stream),
    write(Stream, 'fact(a).\n'),
    close(Stream),
    
    % First scan
    disk_scan:scan_file(FilePath, algorithms),
    state:get_work_items(Items1),
    length(Items1, Len1),
    member(work_item(ID1, _, FilePath, _, FirstCount, _, _, _), Items1),
    
    % Add more content
    sleep(0.1),  % Small delay to ensure different mtime
    open(FilePath, append, Stream2),
    write(Stream2, 'fact(b).\n'),
    write(Stream2, 'rule(X) :- fact(X).\n'),
    close(Stream2),
    
    % Second scan with changes
    disk_scan:scan_file(FilePath, algorithms),
    state:get_work_items(Items2),
    length(Items2, Len2),
    
    % Should still have same number of items (item updated, not added)
    assertion(Len2 =:= Len1),
    
    % Find the updated item
    member(work_item(ID2, _, FilePath, _, SecondCount, _, _, _), Items2),
    
    % IDs should match (same item)
    assertion(ID1 == ID2),
    
    % Count should have increased
    assertion(SecondCount > FirstCount),
    !.

% ============================================================================
% Directory Scanning Tests
% ============================================================================

test(scan_directory_finds_files, [setup(setup_test_env), cleanup(cleanup_test_env)]) :-
    % Create test files
    open('/tmp/test_disk_scan/algorithms/file1.pl', write, S1),
    write(S1, 'fact1(a).\n'),
    close(S1),
    
    open('/tmp/test_disk_scan/algorithms/file2.pl', write, S2),
    write(S2, 'fact2(b).\n'),
    close(S2),
    
    % Scan directory
    PathConfig = _{path: '/tmp/test_disk_scan/algorithms', type: algorithms},
    disk_scan:scan_directory(PathConfig),
    
    % Verify work items were created
    state:get_work_items(Items),
    length(Items, Count),
    assertion(Count >= 2),
    !.

:- end_tests(disk_scan).
