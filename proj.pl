:- use_module(library(clpfd)).

% color_mazes(Maze, N):-
%     maximum(NumColors, Maze),
%     length(Colors, NumColors),
%     all_equal(Colors),
%     MazeSize is N * N,
%     length(Path, MazeSize),
%     Start is N * (N - 1),
%     Finish is N - 1,
%     find_path(Maze, Colors, Start, Finish, Path, N),
%     labeling([], Path).

% find_path(_, _, Finish, Finish, _).
% find_path(Maze, Colors, Position, Finish, Path, N) :-
%     adjacent_positions(Position, N, AdjacentList).


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