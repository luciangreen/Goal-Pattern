% api.pl - Internal API for scan, plan, and report
:- module(api, [
    source_scan/2,
    planner_recommendations/2,
    generate_report/1
]).

:- use_module(log).
:- use_module(state).
:- use_module(config).

% Scan a source and return items
% Now with actual disk scanning support
source_scan(Source, Items) :-
    log_debug('source_scan called'),
    format(atom(Msg), 'Scanning source: ~w', [Source]),
    log_info(Msg),
    
    % Check if disk_scan module is enabled
    (config:get_config([modules, enabled], EnabledModules) ->
        (member(disk_scan, EnabledModules) ->
            (use_module(modules/disk_scan),
             catch(
                 disk_scan:scan_directories,
                 Error,
                 (format(atom(ErrMsg), 'Error during disk scan: ~w', [Error]),
                  log_error(ErrMsg))
             ))
        ;
            log_debug('disk_scan module not enabled')
        )
    ;
        log_debug('No modules configured')
    ),
    
    % Return discovered items
    state:get_work_items(Items),
    !.

% Generate planner recommendations based on state
% This is a stub implementation that will be expanded in future specs
planner_recommendations(State, Recommendations) :-
    log_debug('planner_recommendations called'),
    format(atom(Msg), 'Generating recommendations for state: ~w', [State]),
    log_info(Msg),
    % Return empty list for now
    Recommendations = [],
    !.

% Generate a report
% This is a stub implementation that will be expanded in future specs
generate_report(ReportType) :-
    log_debug('generate_report called'),
    format(atom(Msg), 'Generating report of type: ~w', [ReportType]),
    log_info(Msg),
    % Just log for now
    !.
