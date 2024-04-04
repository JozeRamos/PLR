:- use_module(library(clpfd)).
:- use_module(library(lists)).

color_mazes(Maze, N):-
    maximum(NumColors, Maze),
    length(Colors, NumColors),
    all_equal(Colors),
    fill(Colors, 0, NumColors),
    MazeSize is N * N,
    Path = [],
    Start #= N * (N - 1),
    Finish #= N - 1,
    find_path(Maze, Colors, Start, Finish, Path, N).

fill([], _, 0).
fill([X|Xs], X, N) :- N > 0, N0 is N - 1,fill(Xs, X, N0).

find_path(_, _, _, F, P, _) :- last(P, F), write(P), nl.
find_path(Maze, Colors, Position, Finish, Path, N) :-
    \+ member(Position, Path),  % Ensure Position is not already in the path
    append(Path, [Position], P), % Add Position to the path
    update_colors(Maze, Colors, Position, NewColors), % Update the colors
    adjacent_positions(Position, N, AdjacentList),  % Get the adjacent positions
    findall(X, (
        member(X, AdjacentList),
        find_path(Maze, NewColors, X, Finish, P, N)
        ), PathsList). % Recursively call find_path for each adjacent position

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).

update_colors(Maze, Colors, Position, NewColors):-
    nth0(Position, Maze, ColorValue),
    (
        ColorValue =:= 0 -> NewColors = Colors;
        ColorPosition is ColorValue - 1,
        nth0(ColorPosition, Colors, ColorCount),
        NewColorCount is ColorCount + 1,
        replace(ColorPosition, Colors, NewColorCount, NewColors)
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

% color_mazes([1,2,2,0,2,3,3,3,1,2,2,2,0,0,1,2],4).
% 1,2,2,0,
% 2,3,3,3,
% 1,2,2,2,
% 0,0,1,2