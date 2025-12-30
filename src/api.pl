% api.pl - Internal API for scan, plan, and report
:- module(api, [
    source_scan/2,
    planner_recommendations/2,
    generate_report/1
]).

:- use_module(log).
:- use_module(state).

% Scan a source and return items
% This is a stub implementation that will be expanded in future specs
source_scan(Source, Items) :-
    log_debug('source_scan called'),
    format(atom(Msg), 'Scanning source: ~w', [Source]),
    log_info(Msg),
    % Return empty list for now
    Items = [],
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
