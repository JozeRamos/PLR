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
  subcircuit(Path),
  maplist(path_constraints(Path, N), Path),

  % Find a possible solution
  labeling([], Path),

  length(NewMaze, Size),
  % filter_maze(Path, Maze, NewMaze),
  color_constraint(Maze, Path, NewMaze, 1),

  % Model the Colors
  maximum(NumColors, Maze),
  count_colors(NewMaze, NumColors, _),

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
  H1 #= 0,
  N1 #= N + 1,
  color_constraint(Maze, T, T1, N1),!.    
color_constraint(Maze, [H|T], [H1|T1], N) :-
  element(H, Maze, H1),
  N1 #= N + 1,
  color_constraint(Maze, T, T1, N1).

% filter_maze(Path, Maze, NewMaze).
% filter_maze(Path, Maze, NewMaze) :-
%   element(Position, Path, Next),
%   element(Position, Maze, Color),
%   element(Position, NewMaze, NewColor),
%   Position #= Next #<=> Bool,
%   if_then_else(Bool, 0, Color, NewColor).

count_colors(_,0,_) :- !.
count_colors(Maze,NumColors, K) :-
    count(NumColors, Maze, #=, K),
    NumColors1 is NumColors - 1,
    count_colors(Maze,NumColors1,K).

% ------------------------------------------------
% write_path(_, _, Finish, Finish) :- write(Finish), !.
% write_path(Path, Maze, Position, Finish) :-
%   element(Position, Maze, Color),
%   write(Position), write(' ('), write(Color), write(') -> '),
%   element(Position, Path, Next),
%   write_path(Path, Maze, Next, Finish).