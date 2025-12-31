% test_insights.pl - Tests for insights generation
:- use_module(library(plunit)).

:- use_module('../src/insights').
:- use_module('../src/timeline').
:- use_module('../src/stats').
:- use_module('../src/state').
:- use_module('../src/model').

% ============================================================================
% Insights Analysis Tests
% ============================================================================

:- begin_tests(insights).

test(analyze_patterns_empty, [true]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Analyze with no data
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    insights:analyze_patterns(Start, End, Insights, _Confidence),
    Insights = insights(Corrs, _, _),
    Corrs = [].

test(analyze_patterns_with_data, [true(Insights = insights(Corrs, _, _))]) :-
    % Clean state
    state:clear_state,
    state:init_state,
    
    % Add some schedule events
    get_time(Now),
    Start is Now - 7200,
    End is Now,
    
    EventStart is Now - 3600,
    EventEnd is Now,
    model:create_schedule_event(event1, EventStart, EventEnd, 'Work', 'Home',
                               [home, work], ics, 1.0, Event),
    state:add_schedule_event(Event),
    
    % Add completed work item
    Timestamps = _{created: Start, modified: Now, completed: Now},
    model:create_work_item(work1, algorithm, 'test.pl', complete, 10,
                          Timestamps, [test], 0.9, WorkItem),
    state:add_work_item(WorkItem),
    
    % Analyze patterns
    insights:analyze_patterns(Start, End, Insights, _Confidence).

% ============================================================================
% Top Predictors Tests
% ============================================================================

test(top_positive_predictors_empty, [true(Top = [])]) :-
    % No correlations
    Insights = insights([], [], []),
    insights:top_positive_predictors(Insights, 10, Top).

test(top_positive_predictors_filtering, [true(length(Top, 2))]) :-
    % Mix of positive and negative correlations
    Correlations = [
        correlation(tag1, 0.8, 0.9),
        correlation(tag2, -0.5, 0.8),
        correlation(tag3, 0.6, 0.85),
        correlation(tag4, -0.3, 0.7)
    ],
    Insights = insights(Correlations, [], []),
    insights:top_positive_predictors(Insights, 10, Top).

test(top_positive_predictors_limit, [true(length(Top, 2))]) :-
    % Request only top 2
    Correlations = [
        correlation(tag1, 0.8, 0.9),
        correlation(tag2, 0.6, 0.85),
        correlation(tag3, 0.4, 0.8)
    ],
    Insights = insights(Correlations, [], []),
    insights:top_positive_predictors(Insights, 2, Top).

test(top_negative_predictors_filtering, [true(length(Top, 2))]) :-
    % Mix of positive and negative correlations
    Correlations = [
        correlation(tag1, 0.8, 0.9),
        correlation(tag2, -0.7, 0.8),
        correlation(tag3, 0.6, 0.85),
        correlation(tag4, -0.4, 0.7)
    ],
    Insights = insights(Correlations, [], []),
    insights:top_negative_predictors(Insights, 10, Top).

test(top_negative_predictors_sorted_by_abs, [true(R1Abs > R2Abs)]) :-
    % Should be sorted by absolute value
    Correlations = [
        correlation(tag1, -0.4, 0.7),
        correlation(tag2, -0.8, 0.9)
    ],
    Insights = insights(Correlations, [], []),
    insights:top_negative_predictors(Insights, 10, [correlation(_, R1, _), correlation(_, R2, _)|_]),
    R1Abs is abs(R1),
    R2Abs is abs(R2).

% ============================================================================
% Avoidance Suggestions Tests
% ============================================================================

test(pattern_avoidance_no_strong_negatives, [true(Suggestions = [])]) :-
    % Weak negative correlations shouldn't trigger suggestions
    Correlations = [
        correlation(tag1, -0.1, 0.8),
        correlation(tag2, -0.2, 0.7)
    ],
    Insights = insights(Correlations, [], []),
    insights:pattern_avoidance_suggestions(Insights, -0.25, Suggestions).

test(pattern_avoidance_with_strong_negatives, [true]) :-
    % Strong negative correlations should trigger suggestions
    Correlations = [
        correlation(late_night, -0.6, 0.9),
        correlation(tag2, -0.1, 0.7)
    ],
    Insights = insights(Correlations, [], []),
    insights:pattern_avoidance_suggestions(Insights, -0.2, Suggestions),
    length(Suggestions, L),
    L > 0.

% ============================================================================
% Report Generation Tests
% ============================================================================

test(generate_insights_report, [true(Report = insights_report(_, _, _, _))]) :-
    % Generate report from insights
    Correlations = [
        correlation(tag1, 0.8, 0.9),
        correlation(tag2, -0.6, 0.85)
    ],
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    timeline:build_timeline(Start, End, Timeline),
    
    Insights = insights(Correlations, Timeline, []),
    insights:generate_insights_report(Insights, Report).

test(generate_insights_report_structure, [
    true(Report = insights_report(confidence(_), positive_predictors(_), 
                                  negative_predictors(_), avoidance_suggestions(_)))
]) :-
    % Check report structure
    Correlations = [
        correlation(tag1, 0.5, 0.8)
    ],
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    timeline:build_timeline(Start, End, Timeline),
    
    Insights = insights(Correlations, Timeline, []),
    insights:generate_insights_report(Insights, Report).

% ============================================================================
% Formatting Tests
% ============================================================================

test(format_insight_positive, [true(atom(Formatted))]) :-
    insights:format_insight(correlation(home, 0.8, 0.95), Formatted).

test(format_insight_negative, [true(atom(Formatted))]) :-
    insights:format_insight(correlation(travel, -0.6, 0.85), Formatted).

test(format_insight_weak, [true(atom(Formatted))]) :-
    insights:format_insight(correlation(sauna, 0.15, 0.5), Formatted).

% ============================================================================
% Confidence Calculation Tests
% ============================================================================

test(calculate_confidence_low_data, [true(Confidence < 0.3)]) :-
    % With little data, confidence should be low
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    Start is Now - 600,  % 10 minutes
    End is Now,
    timeline:build_timeline(Start, End, Timeline),
    
    insights:calculate_overall_confidence(Timeline, [], Confidence).

test(calculate_confidence_more_data, [true(Confidence >= PrevConfidence)]) :-
    % More data should give higher or equal confidence
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    
    % Small timeline
    Start1 is Now - 600,
    End1 is Now,
    timeline:build_timeline(Start1, End1, Timeline1),
    insights:calculate_overall_confidence(Timeline1, [], PrevConfidence),
    
    % Larger timeline
    Start2 is Now - 86400,  % 1 day
    End2 is Now,
    timeline:build_timeline(Start2, End2, Timeline2),
    insights:calculate_overall_confidence(Timeline2, [], Confidence).

:- end_tests(insights).
