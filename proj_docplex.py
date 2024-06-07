from docplex.cp.model import CpoModel

# Create CPO model
mdl = CpoModel('Maze Solver')

# Final check not working

# Define the maze
maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2]

max_number = max(maze)

# Calculate maze size and side length
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
  mdl.add(mdl.if_then(position == 0, new_maze[i] == 0))
  mdl.add(mdl.if_then(position != 0, new_maze[i] == maze[i]))


count_colors = mdl.integer_var_list(max_number, 0, size - 1, "count_colors")
for i in range(1, max_number+1):
  mdl.add(count_colors[i-1] == mdl.count(new_maze, i))

# # Add a constraint that all elements in count_colors are the same
# for i in range(1, len(count_colors)):
#     mdl.add(count_colors[i] == count_colors[0])
    
# Solve the model
solution = mdl.solve()

# Print the solution
if solution:
  print(solution.get_solve_status())
  solution.print_solution()
else:
  print("No solution found")