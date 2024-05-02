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

  % TODO Model the Colors
  maximum(MaxColor, Maze),
  length(Colors, Size),
  domain(Colors, 1, MaxColor),
  
  % length(ColorCounts, MaxColor),
  % Upper is Size // MaxColor,
  % domain(ColorCounts, 1, Upper),
  % all_equal(ColorCounts),

  % Constraints
  maplist(path_constraints(Path, N), Path),
  subcircuit(Path),

  % Find a possible solution
  labeling([], Path),
  
  % Validate Solution
  % exclude(filter(Path), Path, FilteredPath),
  % maplist(filter_maze(Maze), FilteredMaze, FilteredPath),
  % maplist(count(FilteredMaze), Colors, ColorCounts),

  write(Path), nl,
  % write_path(Path, Maze, Start, Finish), nl,
  % write (ColorCounts), nl,
  % write (Colors), nl,
  fail.

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

% Count the number of times a value appears in a list
count_color_mazes(Maze):-
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