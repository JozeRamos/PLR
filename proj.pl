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
    
    

adjacent_positions(Position, N, AdjacentList):-
    length(AdjacentList, 4),
    validate_position_up(Position, A, N),
    validate_position_right(Position, B, N),
    validate_position_down(Position, C, N),
    validate_position_left(Position, D, N),
    AdjacentList = [A, B, C, D].

validate_position_up(Position, -1, N):-
    Position #< N.
validate_position_up(Position, Adjacent, N):-
    Position #>= N,
    Adjacent is Position - N.
    
validate_position_down(Position, -1, N):-
    N1 is N - 1,
    N2 is N * N1 - 1,
    Position #> N2.
validate_position_down(Position, Adjacent, N):-
    N1 is N - 1,
    N2 is N * N1 - 1,
    Position #=< N2,
    Adjacent is Position + N.

validate_position_left(Position, -1, N):-
    PositionMod is Position mod N,
    PositionMod #= 0.
validate_position_left(Position, Adjacent, N):-
    PositionMod is Position mod N,
    PositionMod #\= 0,
    Adjacent is Position - 1.

validate_position_right(Position, -1, N):-
    PositionMod is Position mod N,
    N1 is N - 1,
    PositionMod #= N1.
validate_position_right(Position, Adjacent, N):-
    PositionMod is Position mod N,
    N1 is N - 1,
    PositionMod #\= N1,
    Adjacent is Position + 1.

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