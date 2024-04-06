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
    element(1, Path, Start),
    element(Size, Path, Finish),

    % Model edges
    path_constraints(Path, Maze, N),

    % Ensure that the path represents a subcircuit.
    % !Removed because it breaks the code
    % subcircuit(Path), 

    % TODO: Model Colors
    % maximum(NumColors, Maze),
    % length(Colors, NumColors),
    % calculate_colors(Path, Maze, Colors),
    % all_equal(Colors),

    % Find a solution.
    labeling([], Path),

    % Print the solution.
    write(Path), nl.

% ------------------------------------------------
path_constraints([_], _, _).
path_constraints([Current, Next | Rest], _, N) :-
    Current #= N, Next #= Current,
    path_constraints([Next | Rest], _, N).

path_constraints([Current, Next | Rest], Maze, N) :-
    neighbor(Current, Next, Maze, N),
    path_constraints([Next | Rest], Maze, N).

neighbor(Current, Next, Maze, N) :-
    Size is N * N,
    domain([Current, Next], 1, Size),
    Diff #= abs((Current-1) mod N - (Next-1) mod N),
    Current #= Next + 1 #/\ Diff #= 1 #\/
    Current #= Next - 1 #/\ Diff #= 1 #\/
    Current #= Next + N #\/
    Current #= Next - N .

% ------------------------------------------------
% TODO: Model Colors
% calculate_colors([], _, _).
% calculate_colors([Current | Rest], Maze, Colors) :-
%     element(Current, Maze, ColorValue),
%     ColorValue #= 0 #\/ 
%     element(ColorValue, Colors, ColorCount),
%     NewColorCount #= ColorCount + 1,
%     element(ColorValue, Colors, NewColorCount),
%     calculate_colors(Rest, Maze, Colors).
