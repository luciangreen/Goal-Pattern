% disk_scan.pl - Local disk scanning for algorithms and essays
:- module(disk_scan, [
    scan_directories/0,
    scan_directory/1,
    count_prolog_clauses/2,
    count_words/2,
    classify_completion/3,
    index_file/2
]).

:- use_module(library(filesex)).
:- use_module(library(readutil)).
:- use_module('../config').
:- use_module('../log').
:- use_module('../state').
:- use_module('../model').

% ============================================================================
% Main Scanning Interface
% ============================================================================

% Scan all configured directories
scan_directories :-
    log_info('Starting disk scan...'),
    (config:get_config([disk_scan, paths], Paths) ->
        (maplist(scan_directory, Paths),
         log_info('Disk scan completed'))
    ;
        log_warn('No disk scan paths configured')
    ),
    !.

% Scan a single directory
scan_directory(PathConfig) :-
    format(atom(TraceMsg1), 'scan_directory called with: ~w', [PathConfig]),
    log_info(TraceMsg1),
    get_dict(path, PathConfig, Path),
    format(atom(TraceMsg2), '  path extracted: ~w', [Path]),
    log_info(TraceMsg2),
    get_dict(type, PathConfig, Type),
    format(atom(TraceMsg3), '  type extracted: ~w', [Type]),
    log_info(TraceMsg3),
    (exists_directory(Path) ->
        (format(atom(Msg), 'Scanning ~w directory: ~w', [Type, Path]),
         log_info(Msg),
         format(atom(TraceMsg4), '  calling scan_directory_recursive(~w, ~w)', [Path, Type]),
         log_info(TraceMsg4),
         scan_directory_recursive(Path, Type),
         log_info('  scan_directory_recursive completed'))
    ;
        (format(atom(Msg), 'Directory not found: ~w', [Path]),
         log_warn(Msg))
    ),
    !.

% Recursively scan directory
scan_directory_recursive(Dir, Type) :-
    directory_files(Dir, Files),
    process_all_files(Files, Dir, Type),
    !.

% Process all files in a list
process_all_files([], _, _) :- !.
process_all_files([File|Rest], Dir, Type) :-
    (File = '.' ; File = '..') ->
        process_all_files(Rest, Dir, Type)
    ;
        (process_file_or_dir(Dir, File, Type) ->
            true
        ;
            true  % Continue even if one file fails
        ),
        process_all_files(Rest, Dir, Type).

% Process file or subdirectory
process_file_or_dir(Dir, Name, Type) :-
    atomic_list_concat([Dir, '/', Name], FullPath),
    format(atom(DbgMsg), 'Processing: ~w (type=~w)', [FullPath, Type]),
    log_info(DbgMsg),
    (exists_directory(FullPath) ->
        (log_info('  -> is directory'),
         scan_directory_recursive(FullPath, Type))
    ;
        (is_scannable_file(FullPath, Type) ->
            (log_info('  -> is scannable file'),
             scan_file(FullPath, Type))
        ;
            log_info('  -> skipping (not scannable)')
        )
    ).

% ============================================================================
% File Type Detection
% ============================================================================

% Check if file should be scanned based on type
is_scannable_file(Path, Type) :-
    is_algorithm_type(Type),
    file_name_extension(_, pl, Path),
    !.

is_scannable_file(Path, Type) :-
    is_philosophy_type(Type),
    file_name_extension(_, Ext, Path),
    member(Ext, [md, txt, text]),
    !.

% Helper predicates to match type (handles various representations)
is_algorithm_type(algorithms).
is_algorithm_type('algorithms').
is_algorithm_type(X) :- atom(X), atom_string(X, "algorithms").

is_philosophy_type(philosophies).
is_philosophy_type('philosophies').
is_philosophy_type(X) :- atom(X), atom_string(X, "philosophies").

% ============================================================================
% File Scanning
% ============================================================================

% Scan a single file
scan_file(Path, Type) :-
    catch(
        scan_file_safe(Path, Type),
        Error,
        (format(atom(Msg), 'Error scanning file ~w: ~w', [Path, Error]),
         log_error(Msg))
    ).

scan_file_safe(Path, algorithms) :-
    count_prolog_clauses(Path, ClauseCount),
    time_file(Path, MTime),
    
    % Check if this file was scanned before
    (find_existing_work_item(Path, OldItem) ->
        update_existing_algorithm(OldItem, Path, ClauseCount, MTime)
    ;
        create_new_algorithm(Path, ClauseCount, MTime)
    ),
    !.

scan_file_safe(Path, philosophies) :-
    count_words(Path, WordCount),
    time_file(Path, MTime),
    
    % Check if this file was scanned before
    (find_existing_work_item(Path, OldItem) ->
        update_existing_philosophy(OldItem, Path, WordCount, MTime)
    ;
        create_new_philosophy(Path, WordCount, MTime)
    ),
    !.

% ============================================================================
% Work Item Management
% ============================================================================

% Find existing work item by path
find_existing_work_item(Path, WorkItem) :-
    state:get_work_items(Items),
    member(WorkItem, Items),
    WorkItem = work_item(_, _, Path, _, _, _, _, _),
    !.

% Create new algorithm work item
create_new_algorithm(Path, ClauseCount, MTime) :-
    gensym(alg_, ID),
    classify_completion(algorithms, ClauseCount, Status),
    get_time(Now),
    Timestamps = _{created: Now, modified: MTime, last_scanned: Now},
    Tags = [algorithm, scanned],
    Confidence = 1.0,
    
    model:create_work_item(ID, algorithm, Path, Status, ClauseCount, 
                          Timestamps, Tags, Confidence, WorkItem),
    state:add_work_item(WorkItem),
    
    format(atom(Msg), 'New algorithm found: ~w (~w clauses, status: ~w)', 
           [Path, ClauseCount, Status]),
    log_info(Msg),
    !.

% Update existing algorithm work item
update_existing_algorithm(OldItem, Path, NewClauseCount, MTime) :-
    OldItem = work_item(ID, Type, _, _OldStatus, OldClauseCount, OldTimestamps, Tags, _),
    
    % Calculate delta
    Delta is NewClauseCount - OldClauseCount,
    
    % Classify new status
    classify_completion(algorithms, NewClauseCount, NewStatus),
    
    % Update timestamps
    get_time(Now),
    put_dict(modified, OldTimestamps, MTime, Temp),
    put_dict(last_scanned, Temp, Now, NewTimestamps),
    
    % Determine confidence based on delta
    (Delta > 0 -> Confidence = 1.0 ; Confidence = 0.9),
    
    % Create updated work item
    model:create_work_item(ID, Type, Path, NewStatus, NewClauseCount,
                          NewTimestamps, Tags, Confidence, NewWorkItem),
    state:update_work_item(ID, NewWorkItem),
    
    (Delta =\= 0 ->
        (format(atom(Msg), 'Algorithm updated: ~w (delta: ~w, total: ~w, status: ~w)', 
                [Path, Delta, NewClauseCount, NewStatus]),
         log_info(Msg))
    ;
        true
    ),
    !.

% Create new philosophy work item
create_new_philosophy(Path, WordCount, MTime) :-
    gensym(phil_, ID),
    classify_completion(philosophies, WordCount, Status),
    get_time(Now),
    Timestamps = _{created: Now, modified: MTime, last_scanned: Now},
    Tags = [philosophy, scanned],
    Confidence = 1.0,
    
    model:create_work_item(ID, philosophy, Path, Status, WordCount,
                          Timestamps, Tags, Confidence, WorkItem),
    state:add_work_item(WorkItem),
    
    format(atom(Msg), 'New philosophy found: ~w (~w words, status: ~w)', 
           [Path, WordCount, Status]),
    log_info(Msg),
    !.

% Update existing philosophy work item
update_existing_philosophy(OldItem, Path, NewWordCount, MTime) :-
    OldItem = work_item(ID, Type, _, _OldStatus, OldWordCount, OldTimestamps, Tags, _),
    
    % Calculate delta
    Delta is NewWordCount - OldWordCount,
    
    % Classify new status
    classify_completion(philosophies, NewWordCount, NewStatus),
    
    % Update timestamps
    get_time(Now),
    put_dict(modified, OldTimestamps, MTime, Temp),
    put_dict(last_scanned, Temp, Now, NewTimestamps),
    
    % Determine confidence based on delta
    (Delta > 0 -> Confidence = 1.0 ; Confidence = 0.9),
    
    % Create updated work item
    model:create_work_item(ID, Type, Path, NewStatus, NewWordCount,
                          NewTimestamps, Tags, Confidence, NewWorkItem),
    state:update_work_item(ID, NewWorkItem),
    
    (Delta =\= 0 ->
        (format(atom(Msg), 'Philosophy updated: ~w (delta: ~w, total: ~w, status: ~w)', 
                [Path, Delta, NewWordCount, NewStatus]),
         log_info(Msg))
    ;
        true
    ),
    !.

% ============================================================================
% Clause Counting
% ============================================================================

% Count Prolog clauses in a file
count_prolog_clauses(FilePath, Count) :-
    catch(
        count_prolog_clauses_safe(FilePath, Count),
        _Error,
        (log_debug('Failed to parse Prolog file, counting as 0'),
         Count = 0)
    ).

count_prolog_clauses_safe(FilePath, Count) :-
    open(FilePath, read, Stream),
    count_clauses_from_stream(Stream, 0, Count),
    close(Stream),
    !.

% Count clauses from stream
count_clauses_from_stream(Stream, Acc, Count) :-
    catch(
        read_term(Stream, Term, []),
        _,
        fail
    ),
    !,
    (Term == end_of_file ->
        Count = Acc
    ;
        (is_clause(Term) ->
            NewAcc is Acc + 1
        ;
            NewAcc = Acc
        ),
        count_clauses_from_stream(Stream, NewAcc, Count)
    ).

count_clauses_from_stream(_, Count, Count).

% Check if term is a clause (not a directive or query)
is_clause(Term) :-
    Term \= end_of_file,
    functor(Term, F, _),
    F \= ':-',
    F \= '?-',
    !.

is_clause((_ :- _)) :- !.

% ============================================================================
% Word Counting
% ============================================================================

% Count words in a text file
count_words(FilePath, Count) :-
    catch(
        count_words_safe(FilePath, Count),
        _Error,
        (log_debug('Failed to count words, counting as 0'),
         Count = 0)
    ).

count_words_safe(FilePath, Count) :-
    open(FilePath, read, Stream, [encoding(utf8)]),
    read_string(Stream, _, Content),
    close(Stream),
    split_string(Content, " \t\n\r", " \t\n\r", Words),
    exclude(=(""), Words, NonEmptyWords),
    length(NonEmptyWords, Count),
    !.

% ============================================================================
% Completion Classification
% ============================================================================

% Classify completion status based on type and count
% These thresholds are configurable via config file

classify_completion(algorithms, Count, Status) :-
    (config:get_config([disk_scan, thresholds, algorithm_draft], DraftThreshold) ->
        true ; DraftThreshold = 10),
    (config:get_config([disk_scan, thresholds, algorithm_partial], PartialThreshold) ->
        true ; PartialThreshold = 30),
    (config:get_config([disk_scan, thresholds, algorithm_complete], CompleteThreshold) ->
        true ; CompleteThreshold = 80),
    classify_by_thresholds(Count, DraftThreshold, PartialThreshold, CompleteThreshold, Status),
    !.

classify_completion(philosophies, Count, Status) :-
    (config:get_config([disk_scan, thresholds, philosophy_draft], DraftThreshold) ->
        true ; DraftThreshold = 50),
    (config:get_config([disk_scan, thresholds, philosophy_partial], PartialThreshold) ->
        true ; PartialThreshold = 200),
    (config:get_config([disk_scan, thresholds, philosophy_complete], CompleteThreshold) ->
        true ; CompleteThreshold = 500),
    classify_by_thresholds(Count, DraftThreshold, PartialThreshold, CompleteThreshold, Status),
    !.

% Helper to classify by thresholds
classify_by_thresholds(Count, DraftThreshold, PartialThreshold, CompleteThreshold, Status) :-
    (Count < DraftThreshold ->
        Status = draft
    ; Count < PartialThreshold ->
        Status = draft
    ; Count < CompleteThreshold ->
        Status = partial
    ;
        Status = complete
    ).

% ============================================================================
% File Indexing
% ============================================================================

% Index file metadata
index_file(Path, Metadata) :-
    exists_file(Path),
    !,
    file_base_name(Path, Name),
    size_file(Path, Size),
    time_file(Path, MTime),
    file_name_extension(_, Ext, Path),
    Metadata = _{
        name: Name,
        path: Path,
        size: Size,
        mtime: MTime,
        extension: Ext
    }.

index_file(_, _{}).
