from docplex.cp.model import CpoModel
import time

def solve_maze():
  """
  Solves a maze using constraint programming.

  Args:
    maze (list): List representing the maze.

  Returns:
    str: The solve status of the model.
  """
  # maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2]
  maze = [1,0,1,2,0,3,2,2,3,3,2,2,3,0,3,1,3,2,2,2,0,3,3,3,1]
  # Path = [1, 2, 7, 4, 20, 0, 6, 12, 3, 9, 5, 11, 13, 8, 14, 10, 16, 17, 18, 19, 15, 21, 22, 23, 24],
  # Create CPO model
  mdl = CpoModel('Maze Solver')
  start_time = time.time()

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
  solution = mdl.solve()

  # Return the solve status
  if solution:    
    end_time = time.time()
    print("Execution time: ", end_time - start_time, "seconds")
    print([solution.get_value(path[i]) for i in range(size)])
    return solution.get_solve_status()
  else:
    return "No solution found"


if __name__ == "__main__":
    print(solve_maze())