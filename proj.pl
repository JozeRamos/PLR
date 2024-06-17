:- use_module(library(clpfd)).
:- use_module(library(lists)).

solve_maze(Maze) :-
  reset_timer,
  % Define the maze
  % Maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2],
  % Ma = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16],

  % Maze = [1,0,1,2,0,3,2,2,3,3,2,2,3,0,3,1,3,2,2,2,0,3,3,3,1],
  % Path = [2,3,8,21,5,1,7,13,4,10,6,12,14,9,15,11,17,18,19,20,16,22,23,24,25],
  % 21 -> 16 -> 11 -> 6 -> 1 -> 2 -> 3 -> 8 -> 13 -> 14 -> 9 -> 4 -> 5

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

  subcircuit(Path),
  maplist(path_constraints(Path, N), Path),

  % Filter the Maze
  length(NewMaze, Size),
  filter_maze(Path, Path, Maze, NewMaze),

  % Model the Colors
  maximum(NumColors, Maze),
  count_colors(NewMaze, NumColors, _), !,

  % Find a possible solution
  % labeling([max_regret, median], Path),
  % labeling([ffc, bisect], Path),
  labeling([ffc, bisect, down], Path),

  write(Maze), nl,
  write_path(Path, NewMaze, Start, Finish),
  nl, print_time('Time: '), 
  fd_statistics, !.

% ------------------------------------------------
% Path as a List Constraints
path_constraints(Path, N, Next) :-
  element(Position, Path, Next), !,
  neighbor(Position, Next, N).

% Close the path
neighbor(N, Start, N) :-
  Start #= N * N - N + 1, !.

% Check if two nodes are neighbors
neighbor(Position, Neighbor, N) :-
  Diff #= abs((Position-1) mod N - (Neighbor-1) mod N),
  Mod #= Position mod N,
  Position #= Neighbor + N #\/ % Up
  (Position #= Neighbor - N #/\ (Mod #\= 0 #\/ Mod #\= 1)) #\/ % Down
  (Position #= Neighbor + 1 #/\ Diff #= 1) #\/ % Right
  (Position #= Neighbor - 1 #/\ Diff #= 1) #\/ % Left
  Position #= Neighbor. % Self

% ------------------------------------------------
% Removes unuseful colors from the maze
filter_maze(_, [], _, _) :- !.
filter_maze(Path, [H|T], Maze, NewMaze) :-
  element(Position, Path, H),
  element(Position, Maze, Color),
  element(Position, NewMaze, NewColor),
  Position #= H #<=> Bool, !,
  if_then_else(Bool, 0, Color, NewColor),
  filter_maze(Path, T, Maze, NewMaze).

% Count the number of colors in the maze
% and ensures they are the same amount (K)
count_colors(_,0,_) :- !.
count_colors(Maze,NumColors, K) :-
  count(NumColors, Maze, #=, K),
  NumColors1 is NumColors - 1,
  count_colors(Maze,NumColors1,K).

% ------------------------------------------------
% Write the path beautifully
write_path(_, _, Finish, Finish) :- write(Finish), !.
write_path(Path, Maze, Position, Finish) :-
  element(Position, Maze, Color),
  write(Position), write(' -> '),
  % write(' (C: '), write(Color), write(') -> '),
  element(Position, Path, Next),
  write_path(Path, Maze, Next, Finish).

% ------------------------------------------------
% Measure the time of the solution
reset_timer:-
  statistics(total_runtime, _).

print_time(Msg):-
  statistics(total_runtime,[_,T]),
  TS is ((T//10)*10)/1000, nl,
  write(Msg),
  write(TS),
  write('s'), nl.

% ------------------------------------------------
% Examples
solve_maze :- solve_maze([1,2,2,0,2,3,3,3,1,2,2,2,0,0,1,2]).

solve_maze_1:-
  solve_maze([
    1, 2, 1, 0,
    2, 0, 1, 1,
    1, 0, 1, 2,
    0, 1, 1, 2
  ]).

solve_maze_2:-
  solve_maze([
    1, 1, 3, 0,
    2, 3, 2, 3,
    1, 3, 3, 1,
    0, 3, 1, 2
  ]).

solve_maze_3:-
  solve_maze([
    1, 2, 1, 3, 0,
    0, 3, 2, 3, 0,
    3, 0, 0, 0, 2,
    3, 3, 1, 3, 2,
    0, 3, 3, 2, 1
  ]).

solve_maze_4:-
  solve_maze([
    3, 3, 1, 1, 0,
    1, 1, 2, 2, 4,
    3, 0, 0, 1, 4,
    4, 1, 4, 1, 1,
    0, 2, 2, 0, 3
  ]).

solve_maze_5:-
solve_maze([
    3, 5, 4, 3, 0,
    2, 1, 1, 4, 1,
    2, 5, 4, 0, 5,
    4, 2, 5, 5, 2,
    0, 4, 2, 5, 3
  ]).

solve_maze_6:-
  solve_maze([
    1, 3, 2, 6, 0,
    6, 2, 4, 6, 5,
    1, 6, 4, 3, 6,
    5, 5, 2, 3, 4,
    0, 0, 6, 5, 1
  ]).

solve_maze_7:-
  solve_maze([
    2, 3, 4, 1, 4, 0,
    0, 3, 4, 1, 1, 1,
    4, 1, 0, 1, 4, 4,
    3, 2, 1, 3, 0, 1,
    3, 4, 1, 1, 4, 2,
    0, 4, 1, 2, 1, 0
  ]).

solve_all:-
  solve_maze_1, nl,
  solve_maze_2, nl,
  solve_maze_3, nl,
  solve_maze_4, nl,
  solve_maze_5, nl,
  solve_maze_6, nl,
  solve_maze_7, nl.

% ------------------------------------------------
% GENERATE MAZE
% ------------------------------------------------
% Generate a valid maze for a given size
generate_maze(N, NumColors, Maze):-
  Size is N * N,
  length(Maze, Size),
  domain(Maze, 0, NumColors),

  % Start and Finish must be 0
  Start is Size - N + 1,
  Finish is N,
  element(Start, Maze, 0),
  element(Finish, Maze, 0),

  ZeroCount #= N + 2,
  count(0, Maze, #<, ZeroCount),

  generate_count_colors(Maze, NumColors), !,

  labeling([], Maze),
  generate_solve_maze(Maze),
  write(Maze), nl, fail.

generate_count_colors(_,0) :- !.
generate_count_colors(Maze,NumColors) :-
  count(NumColors, Maze, #>, 0),
  NumColors1 #= NumColors - 1,
  generate_count_colors(Maze,NumColors1).

generate_solve_maze(Maze) :-

  % Calculate Maze Size and Side Length
  length(Maze, Size),
  N is round(sqrt(Size)),

  % Define the start and finish nodes
  Start is Size - N + 1,
  Finish is N,

  % Model the Path
  length(Path, Size),
  domain(Path, 1, Size),
  element(Finish, Path, Start),

  subcircuit(Path),
  maplist(path_constraints(Path, N), Path),

  % Filter the Maze
  length(NewMaze, Size),
  filter_maze(Path, Path, Maze, NewMaze),

  % Model the Colors
  maximum(NumColors, Maze),
  count_colors(NewMaze, NumColors, _), !,

  % Find a possible solution
  length(Solutions, 1),
  findall(Path, labeling([], Path), Solutions),
  % length(Solutions, NumSolutions),
  % NumSolutions =:= 1.
  member(Path, Solutions),
  write_path(Path, NewMaze, Start, Finish).