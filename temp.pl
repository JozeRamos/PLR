:- use_module(library(clpfd)).

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
    element(1, Path, Start),
    length(Path, PathLength),
    element(PathLength, Path, Finish),

    % TODO: Model Edges
    % for each position, the domain is List
    % adjacent(Start, N, List),

    % Ensure that the path represents a subcircuit.
    subcircuit(Path), 

    % TODO: Model Colors
    maximum(NumColors, Maze),
    length(Colors, NumColors),
    all_equal(Colors),

    % Find a solution.
    labeling([], Path),

    % Print the solution.
    write(Path), nl.

% ------------------------------------------------

edge(Position, N, Value) :- % Up
    Value is Position - N.

edge(Position, N, Value) :- % Down
    Value is Position + N.

edge(Position, N, Value) :- % Left
    Value is Position - 1,
    Position mod N =\= 1.

edge(Position, N, Value) :- % Right
    Value is Position + 1,
    Position mod N =\= 0.

adjacent(Position, N, List) :-
    append([Position], List, Adjacent),
    Size is N * N,
    Value in 1..Size,
    findall(Value, edge(Position, N, Value), List).


example_subcircuit :-
    % Define a list of domain variables.
    Succ = [S1, S2, S3, S4, S5],
    Pred = [P1, P2, P3, P4, P5],

    % Constrain the domain of the variables.
    domain(Succ, 1, 5),
    domain(Pred, 1, 5),

    % Constrain the variables to form at most one Hamiltonian subcircuit.
    subcircuit(Succ),
    subcircuit(Succ, Pred),

    % Find a solution.
    labeling([], Succ),
    labeling([], Pred),

    % Print the solution.
    write('Succ: '), write(Succ), nl,
    write('Pred: '), write(Pred), nl.