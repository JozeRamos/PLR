:- use_module(library(clpfd)).
:- use_module(library(lists)).

color_mazes(Maze):-

    length(Maze, Size),
    N is sqrt(Size)
    Start is Size - N + 1,
    Finish is N,
    shortest_path(Start, Finish, LowerBound),
    length(Path, Length),
    Length #>= LowerBound,
    Length #=< Size,
    domain(Path, 1, Size),
    element(1, Path, Start),
    element(Length, Path, Finish),

    % maximum(NumColors, Maze),
    % countAll(Path, NumColors, Color_Max),
    % length(Colors, NumColors),
    % all_equal(Colors),
    % maximum(Color_Maximum, Colors),
    % Color_Maximum #=< Color_Max,

    subcircuit(Path),
    labeling([], Path),
    write(Path), nl.
    % circuit()


shortest_path(P, P, 0).
shortest_path(P, N, F):-
    P #< N,
    write(P), nl,
    P1 is P + 1,
    shortest_path(P1, N, F1),
    F is F1 + 1.
shortest_path(P, N, F):-
    P #> N,
    write(P), nl,
    N1 is N + 1,
    P1 is P - N1,
    shortest_path(P1, N, F1),
    F is F1 + 1.

countAll(List, N, Min) :-
    length(L, N),
    domain(L, 1, N),
    all_distinct(L),
    labeling([], L),
    count_min(List, Count, L),
    minimum(Min,Count).

count_min(_, [], []).
count_min(List, [H1|T1], [H|T]) :-
    count(H, List, #=, H1),
    count_min(List, T1, T).