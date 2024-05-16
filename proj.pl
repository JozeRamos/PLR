:- use_module(library(clpfd)).
:- use_module(library(lists)).

solve_maze :-
  % Define the maze
  Maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2],
  % Ma = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],

  % Calculate Maze Size and Side Lenght
  length(Maze, Size),
  N is round(sqrt(Size)),

  % Define the start and finish nodes
  Start is Size - N + 1,
  Finish is N,

  % Model the Path
  length(Path, Size),
  domain(Path, 1, Size),
  element(Finish, Path, Start),

  % Constraints
  subcircuit(Path),
  maplist(path_constraints(Path, N), Path),

  % Find a possible solution
  labeling([], Path),

  % TODO Model the Colors
  length(NewMaze, Size),
  color_constraint(Maze, Path, NewMaze, 1),
  maximum(NumColors, Maze),
  length(Colors, NumColors),
  countAll(NewMaze, NumColors, Colors),
  all_equal(Colors),

  write(Path), 
  nl, fail.

% ------------------------------------------------
% Path as a List Constraints
path_constraints(Path, N, Next) :-
  element(Position, Path, Next),
  neighbor(Position, Next, N).

% Close the path
neighbor(N, Start, N) :-
  Start #= N * N - N + 1.

% Check if two nodes are neighbors
neighbor(Position, Neighbor, N) :-
  Diff #= abs((Position-1) mod N - (Neighbor-1) mod N),
  Position #= Neighbor - N #\/ % Up
  Position #= Neighbor + N #\/ % Down
  (Position #= Neighbor + 1 #/\ Diff #= 1) #\/ % Right
  (Position #= Neighbor - 1 #/\ Diff #= 1) #\/ % Left
  Position #= Neighbor. % Self

% ------------------------------------------------

color_constraint(_, [], [], _).
color_constraint(Maze, [H|T], [H1|T1], N) :-
    N #= H,
    H1 is 0,
    N1 is N + 1,
    color_constraint(Maze, T, T1, N1),!.    
color_constraint(Maze, [H|T], [H1|T1], N) :-
    element(H, Maze, H1),
    N1 is N + 1,
    color_constraint(Maze, T, T1, N1).

% count maze colors
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

% ------------------------------------------------
% Auxiliar Predicates
filter(Path, Position) :-
  element(Position, Path, Position).

filter_maze(Maze, FilteredMaze, Position) :-
  element(Position, Maze, FilteredMaze).

% TODO findall of each value in the list, and length of the result to get the count
count_(List, Value, Count) :-
  aggregate_all(count, member(Value, List), Count).

write_path(_, _, Finish, Finish) :- write(Finish), !.
write_path(Path, Maze, Position, Finish) :-
  element(Position, Maze, Color),
  write(Position), write(' ('), write(Color), write(') -> '),
  element(Position, Path, Next),
  write_path(Path, Maze, Next, Finish).