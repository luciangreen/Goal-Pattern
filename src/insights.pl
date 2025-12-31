% insights.pl - Generate actionable insights from correlations
:- module(insights, [
    analyze_patterns/4,
    top_positive_predictors/3,
    top_negative_predictors/3,
    pattern_avoidance_suggestions/3,
    generate_insights_report/2,
    format_insight/2,
    calculate_overall_confidence/3,
    print_insights_report/1,
    format_suggestion/2
]).

:- use_module(library(lists)).
:- use_module(timeline).
:- use_module(stats).
:- use_module(state).
:- use_module(model).
:- use_module(log).

% ============================================================================
% Constants
% ============================================================================

% Timeline confidence: minutes in a week (7 days * 24 hours * 60 minutes)
timeline_week_minutes(10080).

% Work item confidence: threshold for reliable sample
reliable_work_item_count(50).

% Minimum absolute correlation to trigger avoidance suggestion
min_avoidance_correlation(0.3).

% ============================================================================
% Pattern Analysis
% ============================================================================

% Analyze all patterns and their correlations with goal completion
% analyze_patterns(StartStamp, EndStamp, Insights, Confidence)
analyze_patterns(StartStamp, EndStamp, Insights, OverallConfidence) :-
    % Build timeline
    timeline:build_timeline(StartStamp, EndStamp, Timeline),
    
    % Get work items
    state:get_work_items(WorkItems),
    
    % Analyze all unique tags
    extract_all_tags(Timeline, AllTags),
    
    % Calculate correlations for each tag
    findall(Correlation,
            (member(Tag, AllTags),
             stats:tag_work_correlation(Tag, Timeline, WorkItems, Correlation)),
            Correlations),
    
    % Calculate overall confidence based on data quality
    calculate_overall_confidence(Timeline, WorkItems, OverallConfidence),
    
    % Structure insights
    Insights = insights(Correlations, Timeline, WorkItems).

% Extract all unique tags from timeline
extract_all_tags(Timeline, UniqueTags) :-
    findall(Tag,
            (member(timeline_minute(_, _, Tags, _), Timeline),
             member(Tag, Tags)),
            AllTags),
    list_to_set(AllTags, UniqueTags).

% Calculate overall confidence in analysis
calculate_overall_confidence(Timeline, WorkItems, Confidence) :-
    length(Timeline, TimelineLength),
    length(WorkItems, WorkItemCount),
    
    % Confidence increases with more data
    timeline_week_minutes(WeekMinutes),
    reliable_work_item_count(ReliableItems),
    TimelineConfidence is min(1.0, TimelineLength / WeekMinutes),
    WorkConfidence is min(1.0, WorkItemCount / ReliableItems),
    
    % Overall confidence is geometric mean
    Confidence is sqrt(TimelineConfidence * WorkConfidence).

% ============================================================================
% Top Predictors
% ============================================================================

% Get top N positive predictors of goal completion
% top_positive_predictors(Insights, N, TopPredictors)
top_positive_predictors(insights(Correlations, _, _), N, TopPredictors) :-
    % Filter positive correlations
    include(is_positive_correlation, Correlations, PositiveCorrs),
    
    % Sort by correlation strength (descending)
    sort_correlations_desc(PositiveCorrs, SortedCorrs),
    
    % Take top N
    take_n(N, SortedCorrs, TopPredictors).

% Check if correlation is positive
is_positive_correlation(correlation(_, R, _)) :-
    R > 0.0.

% Get top N negative predictors of goal completion
% top_negative_predictors(Insights, N, TopPredictors)
top_negative_predictors(insights(Correlations, _, _), N, TopPredictors) :-
    % Filter negative correlations
    include(is_negative_correlation, Correlations, NegativeCorrs),
    
    % Sort by correlation strength (by absolute value, descending)
    sort_correlations_by_abs_desc(NegativeCorrs, SortedCorrs),
    
    % Take top N
    take_n(N, SortedCorrs, TopPredictors).

% Check if correlation is negative
is_negative_correlation(correlation(_, R, _)) :-
    R < 0.0.

% Sort correlations by R value (descending)
sort_correlations_desc(Correlations, Sorted) :-
    predsort(compare_correlations_desc, Correlations, Sorted).

% Sort correlations by absolute R value (descending)
sort_correlations_by_abs_desc(Correlations, Sorted) :-
    predsort(compare_correlations_abs_desc, Correlations, Sorted).

% Compare two correlations (for descending sort)
compare_correlations_desc(Order, correlation(_, R1, _), correlation(_, R2, _)) :-
    (R1 > R2 -> Order = (<) ;
     R1 < R2 -> Order = (>) ;
     Order = (=)).

% Compare two correlations by absolute value (for descending sort)
compare_correlations_abs_desc(Order, correlation(_, R1, _), correlation(_, R2, _)) :-
    AbsR1 is abs(R1),
    AbsR2 is abs(R2),
    (AbsR1 > AbsR2 -> Order = (<) ;
     AbsR1 < AbsR2 -> Order = (>) ;
     Order = (=)).

% Take first N elements from list
take_n(0, _, []).
take_n(_, [], []).
take_n(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take_n(N1, T, Rest).

% ============================================================================
% Pattern Avoidance Suggestions
% ============================================================================

% Generate pattern avoidance suggestions
% pattern_avoidance_suggestions(Insights, MinCorrelation, Suggestions)
pattern_avoidance_suggestions(insights(Correlations, _, _), MinCorrelation, Suggestions) :-
    % Find strongly negative correlations
    min_avoidance_correlation(MinAbsCorr),
    findall(suggestion(Tag, R, Confidence, Reason),
            (member(correlation(Tag, R, Confidence), Correlations),
             R < MinCorrelation,
             abs(R) > MinAbsCorr,  % Only suggest if correlation is strong enough
             avoidance_reason(Tag, R, Reason)),
            Suggestions).

% Generate reason for avoiding a pattern
avoidance_reason(Tag, R, Reason) :-
    format(atom(Reason), 
           'Pattern "~w" shows negative correlation (~2f) with goal completion. Consider reducing time spent in this context.',
           [Tag, R]).

% ============================================================================
% Insights Report Generation
% ============================================================================

% Generate a comprehensive insights report
% generate_insights_report(Insights, Report)
generate_insights_report(Insights, Report) :-
    % Get top predictors
    top_positive_predictors(Insights, 10, TopPositive),
    top_negative_predictors(Insights, 10, TopNegative),
    
    % Get avoidance suggestions
    pattern_avoidance_suggestions(Insights, -0.2, Suggestions),
    
    % Get overall confidence
    Insights = insights(_, Timeline, WorkItems),
    calculate_overall_confidence(Timeline, WorkItems, Confidence),
    
    % Structure report
    Report = insights_report(
        confidence(Confidence),
        positive_predictors(TopPositive),
        negative_predictors(TopNegative),
        avoidance_suggestions(Suggestions)
    ).

% ============================================================================
% Formatting
% ============================================================================

% Format a single insight for display
format_insight(correlation(Tag, R, Confidence), FormattedString) :-
    % Determine strength descriptor
    AbsR is abs(R),
    (AbsR >= 0.7 -> Strength = 'strong' ;
     AbsR >= 0.4 -> Strength = 'moderate' ;
     AbsR >= 0.2 -> Strength = 'weak' ;
     Strength = 'very weak'),
    
    % Determine direction
    (R > 0 -> Direction = 'positive' ; Direction = 'negative'),
    
    % Format confidence percentage
    ConfidencePercent is round(Confidence * 100),
    
    % Create formatted string
    format(atom(FormattedString),
           '~w: ~w ~w correlation (~2f), confidence: ~d%',
           [Tag, Strength, Direction, R, ConfidencePercent]).

% Format suggestion for display
format_suggestion(suggestion(Tag, R, Confidence, Reason), FormattedString) :-
    ConfidencePercent is round(Confidence * 100),
    format(atom(FormattedString),
           'AVOID "~w" (correlation: ~2f, confidence: ~d%): ~w',
           [Tag, R, ConfidencePercent, Reason]).

% ============================================================================
% Report Output
% ============================================================================

% Print insights report to console
print_insights_report(Report) :-
    Report = insights_report(
        confidence(OverallConfidence),
        positive_predictors(Positive),
        negative_predictors(Negative),
        avoidance_suggestions(Suggestions)
    ),
    
    % Print header
    ConfPercent is round(OverallConfidence * 100),
    format('~n=== PATTERN ANALYSIS INSIGHTS ===~n', []),
    format('Overall Confidence: ~d%~n~n', [ConfPercent]),
    
    % Print positive predictors
    format('TOP POSITIVE PREDICTORS (behaviors that help goal completion):~n', []),
    (Positive = [] ->
        format('  (No significant positive predictors found)~n', [])
    ;
        maplist(print_formatted_insight, Positive)
    ),
    format('~n', []),
    
    % Print negative predictors
    format('TOP NEGATIVE PREDICTORS (behaviors that hinder goal completion):~n', []),
    (Negative = [] ->
        format('  (No significant negative predictors found)~n', [])
    ;
        maplist(print_formatted_insight, Negative)
    ),
    format('~n', []),
    
    % Print suggestions
    format('PATTERN AVOIDANCE SUGGESTIONS:~n', []),
    (Suggestions = [] ->
        format('  (No specific avoidance suggestions at this time)~n', [])
    ;
        maplist(print_formatted_suggestion, Suggestions)
    ),
    format('~n', []).

% Helper to print a formatted insight
print_formatted_insight(Correlation) :-
    format_insight(Correlation, Formatted),
    format('  • ~w~n', [Formatted]).

% Helper to print a formatted suggestion
print_formatted_suggestion(Suggestion) :-
    format_suggestion(Suggestion, Formatted),
    format('  • ~w~n', [Formatted]).
