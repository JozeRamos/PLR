:- use_module(library(clpfd)).
:- use_module(library(lists)).

solve_maze :-
    % Define the maze.
    Maze = [1,2,2,0,2,3,3,3,1,2,2,2,0,0,1,2],

    % Calculate Maze Size and Side Lenght
    length(Maze, Size),
    N is round(sqrt(Size)),

    % Define the start and finish nodes.
    Start is Size - N + 1,
    Finish is N,

    % Model the Path
    length(Path, Size),
    domain(Path, 1, Size),
    % element(Finish, Path, Start),
    path_constraints(Path, Path, N),

    % TODO: Model Colors
    % maximum(NumColors, Maze),
    % length(Colors, NumColors),
    % calculate_colors(Path, Maze, Colors),
    % all_equal(Colors),

    % Ensure that the path represents a subcircuit.
    subcircuit(Path),

    % Find a solution.
    labeling([], Path),

    % Print the solution.
    % write('Path: '), write(Path), nl.
    % write('Start: '), write(Start), nl,
    % write('Finish: '), write(Finish), nl.

    print_path(Start, Path, Finish).
    % fd_statistics.

% ------------------------------------------------
% Path as a List Constraints
path_constraints([], _, _).
path_constraints([Current | Rest], Path, N) :-
    element(Position, Path, Current),
    neighbor(Position, Current, N),
    path_constraints(Rest, Path, N).

% Close the path
neighbor(Finish, Start, N) :-
    Finish #= N, Start #= N * N - (N - 1).

% Check if two nodes are neighbors
neighbor(Position, Neighbor, N) :-
    Diff #= abs((Position-1) mod N - (Neighbor-1) mod N),
    Position #= Neighbor - N #\/ % Up
    % Position #= Neighbor + N #\/ % Down
    Position #= Neighbor + N #/\ Position mod N #\= 0 #\/ % Down with optimization
    Position #= Neighbor + 1 #/\ Diff #= 1 #\/ % Right
    Position #= Neighbor - 1 #/\ Diff #= 1 #\/ % Left
    Position #= Neighbor. % Self

% ------------------------------------------------
% TODO Model Colors
% calculate_colors([], _, _).
% calculate_colors([Current | Rest], Maze, Colors) :-
%     element(Current, Maze, ColorValue),
%     ColorValue #= 0 #\/
%     element(ColorValue, Colors, ColorCount),
%     NewColorCount #= ColorCount + 1,
%     element(ColorValue, Colors, NewColorCount),
%     calculate_colors(Rest, Maze, Colors).

% ------------------------------------------------
% TODO Print the path.
print_path(Finish, _, Finish):- write(Finish), nl.
print_path(Position, Path, Finish) :-
    element(Position, Path, Next),
    (
        Position \= Finish ->
        write(Position),
        write(' -> ')
    ),
    print_path(Next, Path, Finish).
