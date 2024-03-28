:- use_module(library(clpfd)).
:- use_module(library(lists)).

color_mazes(Maze, N):-
    maximum(NumColors, Maze),
    length(Colors, NumColors),
    all_equal(Colors),
    fill(Colors, 0, NumColors),
    MazeSize is N * N,
    length(Path, MazeSize),
    Start is N * (N - 1),
    Finish is N - 1,
    find_path(Maze, Colors, Start, Finish, Path, N).

fill([], _, 0).
fill([X|Xs], X, N) :- N > 0, N0 is N - 1,fill(Xs, X, N0).

find_path(_, _, Finish, Finish, _).
find_path(Maze, Colors, Position, Finish, [H|T], N) :-
    H is Position,
    update_colors(Maze, Colors, Position).
    % adjacent_positions(Position, N, AdjacentList),
    % findall(X, (
    %     member(X, AdjacentList), 
    %     find_path(Maze, Colors, X, Finish, T, N)
    %     ), PathsList).
    
update_colors(Maze, Colors, Position):-
    element(Position, Maze, ColorValue),
    (
        ColorValue =:= 0 -> true;
        ColorPosition is ColorValue - 1,
        element(ColorPosition, Colors, ColorCount),
        NewColorCount is ColorCount + 1
    ).

adjacent_positions(Position, N, Adjacent):-
    Up is Position - N,
    Down is Position + N,
    Left is Position - 1,
    Right is Position + 1,
    All = [Up, Down, Left, Right],
    findall(X, (member(X, All), is_valid_position(X, Position, N)), Adjacent).

is_valid_position(X, P, N):-
    X >= 0,
    X < N * N,
    X_Mod is X mod N,
    P_Mod is P mod N,
    Diff is abs(X_Mod - P_Mod),
    Diff #< 2.

%     X-N > 0
% X-1  X  X+1
%     X+N < N*N-1

% N = 4
% X = 3

% N mod N = 0
% X mod N = 3

% 2 verificações

% color_mazes([1,2,2,0,2,3,3,3,1,2,2,2,0,0,1,2],3).
% 1,2,2,0,
% 2,3,3,3,
% 1,2,2,2,
% 0,0,1,2