from docplex.cp.model import CpoModel

# Create CPO model
mdl = CpoModel('Maze Solver')


# Define the maze
maze = [1,2,2,0,2,3,3,3,1,2, 2, 2, 0, 0, 1, 2]

# Calculate maze size and side length
size = len(maze)

# Define the start and finish nodes
finish = round(size ** 0.5) - 1
start = size - finish - 1

path = mdl.integer_var_list(size, 0, size - 1, "path")

mdl.add(path[finish] == start)
mdl.add(mdl.sub_circuit(path))

  
# Add the path constraints
for i in range(len(path)):
  if i != finish:
    position = path[i]
    N = finish + 1
    
    diff = (position) % N

    
    mdl.add((position == i - N) |  # Up
            (position == i + N) |  # Down
            ((position == i + 1) & (diff > 0)) |  # Right
            ((position == i - 1) & (diff < N - 1)) |  # Left
            (position == i))  # Self

# Solve the model
solution = mdl.solve()

# Print the solution
if solution:
  print(solution.get_solve_status())
  solution.print_solution()
else:
  print("No solution found")