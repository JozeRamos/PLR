:- use_module(library(clpfd)).
:- use_module(library(lists)).

color_mazes(Maze, N):-
    maximum(NumColors, Maze),
    length(Colors, NumColors),
    all_equal(Colors),
    fill(Colors, 0, NumColors),
    Path = [],
    Start #= N * (N - 1),
    Finish #= N - 1,
    find_path(Maze, Colors, Start, Finish, Path, N),
    % STATISTICS
    fd_statistics,
    statistics(total_runtime,[_,T]),
    TS is ((T//10)*10)/1000,
    write('Time: '), write(TS), write('s').


fill([], _, 0).
fill([X|Xs], X, N) :- N > 0, N0 #= N - 1, fill(Xs, X, N0).

find_path(_, C, _, F, P, _) :- last(P, F), all_equal(C), write(C), nl, write(P), nl, nl.
find_path(Maze, Colors, Position, Finish, Path, N) :-
    \+ member(Position, Path),  % Ensure Position is not already in the path
    append(Path, [Position], P), % Add Position to the path
    update_colors(Maze, Colors, Position, NewColors), % Update the colors
    adjacent_positions(Position, N, AdjacentList),  % Get the adjacent positions
    findall(X, (
        member(X, AdjacentList),
        find_path(Maze, NewColors, X, Finish, P, N)
        ), _). % Recursively call find_path for each adjacent position

replace(I, L, E, K) :-
  nth0(I, L, _, R),
  nth0(I, K, E, R).

update_colors(Maze, Colors, Position, NewColors):-
    nth0(Position, Maze, ColorValue),
    (
        ColorValue =:= 0 -> NewColors = Colors;
        ColorPosition #= ColorValue - 1,
        nth0(ColorPosition, Colors, ColorCount),
        NewColorCount #= ColorCount + 1,
        replace(ColorPosition, Colors, NewColorCount, NewColors)
    ).

adjacent_positions(Position, N, Adjacent):-
    Up #= Position - N,
    Down #= Position + N,
    Left #= Position - 1,
    Right #= Position + 1,
    All = [Up, Down, Left, Right],
    findall(X, (member(X, All), is_valid_position(X, Position, N)), Adjacent).

is_valid_position(X, P, N):-
    X >= 0,
    X < N * N,
    X_Mod #= X mod N,
    P_Mod #= P mod N,
    Diff #= abs(X_Mod - P_Mod),
    Diff #< 2.

% EXAMPLES

% color_mazes([1,2,2,0,2,3,3,3,1,2,2,2,0,0,1,2],4).
% SOLUTION: [12,8,9,13,14,10,6,7,3]
% 1,2,2,0,
% 2,3,3,3,
% 1,2,2,2,
% 0,0,1,2

% color_mazes([1,1,2,2,0,2,2,3,3,4,1,0,0,2,4,4,2,4,2,2,0,3,3,0,1],5).
% 1 - blue, 2 - red, 3 - yellow, 4 - green  
% SOLUTION: [20,15,10,11,6,1,2,7,8,9,4]
