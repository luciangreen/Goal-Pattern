% stats.pl - Statistical correlation analysis
:- module(stats, [
    pearson_correlation/3,
    chi_square_test/3,
    lag_correlation/4,
    contingency_table/3,
    mean/2,
    stddev/2,
    covariance/3,
    tag_work_correlation/4,
    category_work_correlation/4
]).

:- use_module(library(lists)).
:- use_module(timeline).
:- use_module(model).

% ============================================================================
% Basic Statistical Functions
% ============================================================================

% Calculate mean of a list
mean([], 0.0).
mean(List, Mean) :-
    List \= [],
    sum_list(List, Sum),
    length(List, N),
    Mean is Sum / N.

% Calculate standard deviation
stddev(List, StdDev) :-
    length(List, N),
    N > 0,
    mean(List, Mean),
    maplist(squared_diff(Mean), List, SquaredDiffs),
    sum_list(SquaredDiffs, SumSquares),
    Variance is SumSquares / N,
    StdDev is sqrt(Variance).

stddev([], 0.0).

% Helper: squared difference from mean
squared_diff(Mean, X, Diff) :-
    Diff is (X - Mean) ** 2.

% Calculate covariance between two lists
covariance(X, Y, Cov) :-
    length(X, N),
    length(Y, N),
    N > 0,
    mean(X, MeanX),
    mean(Y, MeanY),
    maplist(diff_from_mean(MeanX), X, DiffsX),
    maplist(diff_from_mean(MeanY), Y, DiffsY),
    maplist(multiply, DiffsX, DiffsY, Products),
    sum_list(Products, SumProducts),
    Cov is SumProducts / N.

covariance([], [], 0.0).

% Helper: difference from mean
diff_from_mean(Mean, X, Diff) :-
    Diff is X - Mean.

% Helper: multiply two values
multiply(X, Y, Product) :-
    Product is X * Y.

% ============================================================================
% Correlation Coefficients
% ============================================================================

% Pearson correlation coefficient
% pearson_correlation(X, Y, R)
% R is between -1 and 1
pearson_correlation([], [], 0.0) :- !.
pearson_correlation(X, Y, R) :-
    length(X, N),
    length(Y, N),
    N > 1,
    !,
    covariance(X, Y, Cov),
    stddev(X, StdX),
    stddev(Y, StdY),
    ((StdX =:= 0.0 ; StdY =:= 0.0) -> 
        R = 0.0 
    ;
        R is Cov / (StdX * StdY)
    ).
pearson_correlation(_, _, 0.0).

% ============================================================================
% Chi-Square Test
% ============================================================================

% Chi-square test for independence
% chi_square_test(ObservedTable, ExpectedTable, ChiSquare)
% Tables are lists of lists (contingency tables)
chi_square_test(Observed, Expected, ChiSquare) :-
    flatten(Observed, ObsList),
    flatten(Expected, ExpList),
    maplist(chi_square_term, ObsList, ExpList, Terms),
    sum_list(Terms, ChiSquare).

% Calculate single chi-square term
chi_square_term(O, E, Term) :-
    E > 0 ->
        Term is ((O - E) ** 2) / E
    ;
        Term = 0.0.

% ============================================================================
% Contingency Tables
% ============================================================================

% Build a 2x2 contingency table for tag presence vs work outcome
% contingency_table(Tag, WorkOutcomes, Table)
% Table = [[tag_present_work_yes, tag_present_work_no],
%          [tag_absent_work_yes, tag_absent_work_no]]
contingency_table(Tag, WorkOutcomes, [[TPW, TPN], [TAW, TAN]]) :-
    % Count occurrences
    findall(1, 
            (member(outcome(Tags, true), WorkOutcomes), member(Tag, Tags)),
            TagPresentWorkYes),
    length(TagPresentWorkYes, TPW),
    
    findall(1,
            (member(outcome(Tags, false), WorkOutcomes), member(Tag, Tags)),
            TagPresentWorkNo),
    length(TagPresentWorkNo, TPN),
    
    findall(1,
            (member(outcome(Tags, true), WorkOutcomes), \+ member(Tag, Tags)),
            TagAbsentWorkYes),
    length(TagAbsentWorkYes, TAW),
    
    findall(1,
            (member(outcome(Tags, false), WorkOutcomes), \+ member(Tag, Tags)),
            TagAbsentWorkNo),
    length(TagAbsentWorkNo, TAN).

% ============================================================================
% Lag Analysis
% ============================================================================

% Calculate correlation with time lag
% lag_correlation(TagSequence, WorkSequence, LagHours, Correlation)
% TagSequence: list of 0/1 indicating tag presence per hour
% WorkSequence: list of work productivity per hour
lag_correlation(TagSequence, WorkSequence, LagHours, Correlation) :-
    length(TagSequence, N),
    length(WorkSequence, N),
    N > LagHours,
    
    % Shift tag sequence by lag
    drop(LagHours, TagSequence, ShiftedTags),
    
    % Truncate work sequence to match
    LaggedLength is N - LagHours,
    take(LaggedLength, WorkSequence, TruncatedWork),
    
    % Calculate correlation
    pearson_correlation(ShiftedTags, TruncatedWork, Correlation).

% Helper: drop first N elements
drop(0, List, List).
drop(N, [_|Tail], Result) :-
    N > 0,
    N1 is N - 1,
    drop(N1, Tail, Result).

% Helper: take first N elements
take(0, _, []).
take(N, [H|T], [H|Rest]) :-
    N > 0,
    N1 is N - 1,
    take(N1, T, Rest).
take(_, [], []).

% ============================================================================
% Domain-Specific Correlations
% ============================================================================

% Analyze correlation between a tag and work outcomes
% tag_work_correlation(Tag, Timeline, WorkItems, Correlation)
tag_work_correlation(Tag, Timeline, WorkItems, correlation(Tag, R, Confidence)) :-
    % Get work outcome windows
    timeline:work_outcome_windows(Timeline, WorkItems, Windows),
    
    % Create binary sequences: tag present/absent in each window
    % and work completed/not completed
    findall(outcome(Tags, true),
            member(window(_, _, _, Tags), Windows),
            SuccessOutcomes),
    
    % For now, use contingency table approach
    % Build contingency table
    contingency_table(Tag, SuccessOutcomes, Table),
    
    % Calculate association strength (phi coefficient)
    phi_coefficient(Table, R),
    
    % Confidence based on sample size
    length(Windows, N),
    Confidence is min(1.0, N / 30.0).  % More windows = higher confidence

% Calculate phi coefficient from 2x2 contingency table
phi_coefficient([[A, B], [C, D]], Phi) :-
    N is A + B + C + D,
    N > 0,
    !,
    Numerator is (A * D - B * C),
    Denominator is sqrt((A + B) * (C + D) * (A + C) * (B + D)),
    (Denominator =:= 0.0 -> 
        Phi = 0.0 
    ;
        Phi is Numerator / Denominator
    ).
phi_coefficient(_, 0.0).

% Analyze correlation between a category and work outcomes
category_work_correlation(Category, Timeline, WorkItems, correlation(Category, R, Confidence)) :-
    % Get work outcome windows
    timeline:work_outcome_windows(Timeline, WorkItems, Windows),
    
    % Calculate time spent in category per window
    findall(TimeRatio,
            (member(window(StartMin, EndMin, _, Tags), Windows),
             category_time_ratio(Category, Tags, TimeRatio)),
            CategoryTimes),
    
    % Create work productivity scores (1 for completed, 0 for not)
    length(Windows, N),
    findall(1, between(1, N, _), WorkScores),
    
    % Calculate correlation
    (CategoryTimes \= [], WorkScores \= [] ->
        pearson_correlation(CategoryTimes, WorkScores, R)
    ;
        R = 0.0
    ),
    
    % Confidence based on sample size and variance
    Confidence is min(1.0, N / 30.0).

% Helper: calculate what ratio of window was spent in a category
category_time_ratio(Category, Tags, Ratio) :-
    % Count how many tags map to this category
    findall(1,
            (member(Tag, Tags), timeline:category_from_tag(Tag, Category)),
            Matches),
    length(Matches, MatchCount),
    length(Tags, TotalCount),
    (TotalCount > 0 ->
        Ratio is MatchCount / TotalCount
    ;
        Ratio = 0.0
    ).
