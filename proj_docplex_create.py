from docplex.cp.model import CpoModel, CpoSolveResult

import time

def solve_maze(maze):
  """
  Solves a maze using constraint programming and checks if there is only one solution.

  Args:
      maze (list): List representing the maze.

  Returns:
      str: The solve status of the model and whether the solution is unique.
  """

  # Create CPO model
  mdl = CpoModel('Maze Solver')

  # Define the maze
  max_number = max(maze)
  size = len(maze)

  # Define the start and finish nodes
  finish = round(size ** 0.5) - 1
  start = size - finish - 1

  path = mdl.integer_var_list(size, 0, size - 1, "path")

  mdl.add(path[finish] == start)
  mdl.add(mdl.sub_circuit(path))

  # Add the path constraints
  for i in range(size):
    if i != finish:
      position = path[i]
      N = finish + 1
      diff = (position) % N
      mdl.add((position == i - N) |  # Up
          (position == i + N) |  # Down
          ((position == i + 1) & (diff > 0)) |  # Right
          ((position == i - 1) & (diff < N - 1)) |  # Left
          (position == i))  # Self

  new_maze = mdl.integer_var_list(size, 0, size - 1, "new_path")

  # Add the constraints
  for i in range(size):
    position = path[i]
    mdl.add(mdl.if_then(position == i, new_maze[i] == 0))
    mdl.add(mdl.if_then(position != i, new_maze[i] == maze[i]))

  count_colors = mdl.integer_var_list(max_number, 0, size - 1, "count_colors")
  for i in range(1, max_number+1):
    mdl.add(count_colors[i-1] == mdl.count(new_maze, i))

  # Add a constraint that all elements in count_colors are the same
  for i in range(1, max_number):
    mdl.add(count_colors[i] == count_colors[0])

  # Solve the model
  solution: CpoSolveResult = mdl.solve(log_output=None)

  # Return the solve status
  if solution:
      initial_solution = [solution.get_value(path[i]) for i in range(size)]

      # Add a constraint to exclude the initial solution
      mdl.add(mdl.sum([path[i] != initial_solution[i] for i in range(size)]) >= 1)

      # Solve again to check for uniqueness
      new_solution = mdl.solve(log_output=None)
      if new_solution:
          return [0,initial_solution]
      else:
          return [1,initial_solution]
  else:
      return [0,[]]



if __name__ == "__main__":
  start_time = time.time()

  N=3
  NumColors=2

  mdl = CpoModel()

  # Create the maze
  Size = N * N
  Maze = mdl.integer_var_list(Size, 0, NumColors, "Maze")

  # Start and Finish must be 0
  Start = Size - N
  Finish = N - 1
  mdl.add(Maze[Start] == 0)
  mdl.add(Maze[Finish] == 0)

  # There must be less than N + 2 zeros in the maze
  mdl.add(mdl.count(Maze, 0) < N + 2)

  # Ensure that each color from NumColors to 1 appears at least once in the Maze
  for color in range(NumColors, 0, -1):
      mdl.add(mdl.count(Maze, color) >= 1)

  found = 0

  solutions = []

  while True:
      # Solve the model
      solution: CpoSolveResult = mdl.solve(log_output=True)

      # Check if a solution was found
      if solution:
          maze_solution = [solution.get_value(Maze[i]) for i in range(Size)]

          found, sol = solve_maze(maze_solution)

          # Add a constraint to exclude the current solution
          mdl.add(mdl.sum([Maze[i] != maze_solution[i] for i in range(Size)]) >= 1)
      else:
          break  # No more solutions found
      
      if found:
        end_time = time.time()
        print(solution)
        print("Execution time: ", end_time - start_time, "seconds")
        found = 0
        print("Maze: ", maze_solution)
        print("Solution: ", sol)
        user_input = input("Do you want to continue? (y/n): ")
        if user_input.lower() != 'y':
            break        
        start_time = time.time()




