% test_stats.pl - Tests for statistical functions
:- use_module(library(plunit)).

:- use_module('../src/stats').
:- use_module('../src/timeline').
:- use_module('../src/state').
:- use_module('../src/model').

% ============================================================================
% Basic Statistics Tests
% ============================================================================

:- begin_tests(stats).

test(mean_empty, [true(Mean =:= 0.0)]) :-
    stats:mean([], Mean).

test(mean_simple, [true(abs(Mean - 3.0) < 0.001)]) :-
    stats:mean([1, 2, 3, 4, 5], Mean).

test(mean_floats, [true(abs(Mean - 2.5) < 0.001)]) :-
    stats:mean([1.0, 2.0, 3.0, 4.0], Mean).

test(stddev_empty, [true(StdDev =:= 0.0)]) :-
    stats:stddev([], StdDev).

test(stddev_constant, [true(StdDev =:= 0.0)]) :-
    stats:stddev([5, 5, 5, 5], StdDev).

test(stddev_simple, [true(StdDev > 0)]) :-
    stats:stddev([1, 2, 3, 4, 5], StdDev).

test(covariance_empty, [true(Cov =:= 0.0)]) :-
    stats:covariance([], [], Cov).

test(covariance_positive, [true(Cov > 0)]) :-
    % Positive relationship: both increase together
    stats:covariance([1, 2, 3, 4, 5], [2, 4, 6, 8, 10], Cov).

test(covariance_negative, [true(Cov < 0)]) :-
    % Negative relationship: one increases, other decreases
    stats:covariance([1, 2, 3, 4, 5], [10, 8, 6, 4, 2], Cov).

% ============================================================================
% Correlation Tests
% ============================================================================

test(pearson_empty, [true(R =:= 0.0)]) :-
    stats:pearson_correlation([], [], R).

test(pearson_perfect_positive, [true(abs(R - 1.0) < 0.001)]) :-
    stats:pearson_correlation([1, 2, 3, 4, 5], [2, 4, 6, 8, 10], R).

test(pearson_perfect_negative, [true(abs(R + 1.0) < 0.001)]) :-
    stats:pearson_correlation([1, 2, 3, 4, 5], [10, 8, 6, 4, 2], R).

test(pearson_no_correlation, [true(abs(R) < 0.5)]) :-
    stats:pearson_correlation([1, 2, 3, 4, 5], [3, 1, 4, 2, 5], R).

test(pearson_zero_variance, [true(R =:= 0.0)]) :-
    % When one variable has no variance
    stats:pearson_correlation([5, 5, 5, 5], [1, 2, 3, 4], R).

% ============================================================================
% Chi-Square Tests
% ============================================================================

test(chi_square_identical, [true(ChiSq =:= 0.0)]) :-
    % Identical observed and expected = no difference
    stats:chi_square_test([[10, 10], [10, 10]], [[10, 10], [10, 10]], ChiSq).

test(chi_square_different, [true(ChiSq > 0)]) :-
    % Different distributions should give positive chi-square
    stats:chi_square_test([[15, 5], [5, 15]], [[10, 10], [10, 10]], ChiSq).

% ============================================================================
% Contingency Table Tests
% ============================================================================

test(contingency_table_basic, [true(Table = [[1, 0], [1, 0]])]) :-
    % Two outcomes: one with tag and work, one without tag and work
    Outcomes = [
        outcome([tag1], true),
        outcome([tag2], true)
    ],
    stats:contingency_table(tag1, Outcomes, Table).

test(contingency_table_mixed, [true(Table = [[1, 1], [0, 1]])]) :-
    % Mixed outcomes
    Outcomes = [
        outcome([tag1], true),   % tag present, work yes
        outcome([tag1], false),  % tag present, work no
        outcome([tag2], false)   % tag absent, work no
    ],
    stats:contingency_table(tag1, Outcomes, Table).

% ============================================================================
% Lag Analysis Tests
% ============================================================================

test(lag_correlation_zero_lag, [true(abs(R - 1.0) < 0.001)]) :-
    % With zero lag, identical sequences should correlate perfectly
    Seq = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
    stats:lag_correlation(Seq, Seq, 0, R).

test(lag_correlation_positive_lag, [true]) :-
    % With lag, correlation should be computable
    Tags = [0, 0, 1, 1, 1, 0, 0, 0],
    Work = [1, 2, 3, 4, 5, 6, 7, 8],
    stats:lag_correlation(Tags, Work, 1, _R).

% ============================================================================
% Domain-Specific Correlation Tests
% ============================================================================

test(tag_work_correlation_no_data, [true]) :-
    % With no data, correlation should be 0
    state:clear_state,
    state:init_state,
    
    get_time(Now),
    Start is Now - 3600,
    End is Now,
    timeline:build_timeline(Start, End, Timeline),
    
    stats:tag_work_correlation(test_tag, Timeline, [], Corr),
    Corr = correlation(test_tag, R, _Conf),
    R =:= 0.0.

test(phi_coefficient_independent, [true(abs(Phi) < 0.1)]) :-
    % Independent variables (equal distribution)
    stats:phi_coefficient([[10, 10], [10, 10]], Phi).

test(phi_coefficient_perfect_positive, [true(abs(Phi - 1.0) < 0.001)]) :-
    % Perfect positive association
    stats:phi_coefficient([[10, 0], [0, 10]], Phi).

test(phi_coefficient_perfect_negative, [true(abs(Phi + 1.0) < 0.001)]) :-
    % Perfect negative association
    stats:phi_coefficient([[0, 10], [10, 0]], Phi).

:- end_tests(stats).
