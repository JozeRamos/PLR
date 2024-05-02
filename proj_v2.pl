:- use_module(library(clpfd)).
:- use_module(library(lists)).

color_mazes(Maze):-

    length(Maze, Size),
    N is sqrt(Size),
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

% color constraint

color_constraint(NewMaze) :-
    Maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2],
    Path = [6,2,3,4,5,1,7,8,9,10,11,12,13,14,15,16],
    length(Path, Size),
    length(NewMaze, Size),
    color_constraint_aux(Maze, Path, NewMaze, 1).

color_constraint_aux(_, [], [], _).
color_constraint_aux(Maze, [H|T], [H1|T1], N) :-
    N #= H,
    H1 is 0,
    N1 is N + 1,
    color_constraint_aux(Maze, T, T1, N1).    
color_constraint_aux(Maze, [H|T], [H1|T1], N) :-
    element(H, Maze, H1),
    N1 is N + 1,
    color_constraint_aux(Maze, T, T1, N1).


% count maze colors
count_color_mazes:-
    Maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2],
    maximum(NumColors, Maze),
    length(Colors, NumColors),
    countAll(Maze, NumColors, Colors),
    write(Colors).

countAll(List, N, Colors) :-
    length(L, N),
    domain(L, 1, N),
    all_distinct(L),
    labeling([], L),
    count_min(List, Colors, L), !.

count_min(_, [], []).
count_min(List, [H1|T1], [H|T]) :-
    count(H, List, #=, H1),
    count_min(List, T1, T).