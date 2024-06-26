from docplex.cp.model import CpoModel
import time

def solve_maze(maze):
  """
  Solves a maze using constraint programming.

  Args:
    maze (list): List representing the maze.

  Returns:
    str: The solve status of the model.
  """

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
          ((position == i + N) & (position % N > 0)) |  # Down
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
  solution = mdl.solve(log_output=None)

  # Return the solve status
  if solution:    
    end_time = time.time()
    print("Execution time: ", end_time - start_time, "seconds")
    print([solution.get_value(path[i]) for i in range(size)])
    
    return solution.get_solve_status()
  else:
    print("No solution found")
    return solution.get_solve_status()


if __name__ == "__main__":
  puzzles = {
    '1': [
      1, 2, 1, 0,
      2, 0, 1, 1,
      1, 0, 1, 2,
      0, 1, 1, 2
    ],
    '2': [
      1, 1, 3, 0,
      2, 3, 2, 3,
      1, 3, 3, 1,
      0, 3, 1, 2
    ],
    '3': [
      1, 2, 1, 3, 0,
      0, 3, 2, 3, 0,
      3, 0, 0, 0, 2,
      3, 3, 1, 3, 2,
      0, 3, 3, 2, 1
    ],
    '4': [
      3, 3, 1, 1, 0,
      1, 1, 2, 2, 4,
      3, 0, 0, 1, 4,
      4, 1, 4, 1, 1,
      0, 2, 2, 0, 3
    ],
    '5': [
      3, 5, 4, 3, 0,
      2, 1, 1, 4, 1,
      2, 5, 4, 0, 5,
      4, 2, 5, 5, 2,
      0, 4, 2, 5, 3
    ],
    '6': [
      1, 3, 2, 6, 0,
      6, 2, 4, 6, 5,
      1, 6, 4, 3, 6,
      5, 5, 2, 3, 4,
      0, 0, 6, 5, 1
    ],
    '7': [
      2, 4, 3, 1, 3, 0,
      0, 3, 4, 1, 1, 1,
      4, 1, 0, 1, 4, 4,
      3, 2, 1, 3, 0, 1,
      3, 4, 1, 1, 4, 2,
      0, 4, 1, 2, 1, 0
    ]
  }

  while True:
    puzzle_choice = input("Enter the puzzle number or 'q' to quit: ")
    if puzzle_choice.lower() == 'q':
      break
    elif puzzle_choice in puzzles:
      maze = puzzles[puzzle_choice]
      print(f"Solving puzzle {puzzle_choice}...")
      solve_maze(maze)
    elif puzzle_choice == '8':
      for i in range(1, 8):
        maze = puzzles[str(i)]
        print(f"Solving puzzle {i}...")
        solve_maze(maze)
    else:
      print("Invalid choice. Please enter a valid puzzle number.")