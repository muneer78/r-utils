# Define the objective function
objective_function <- function(x) {
  # Objective to maximize: 0.2 * Coffee + 1.1 * Mello Yello Zero
  return(-(0.2 * x[1] + 1.1 * x[2]))
}

# Define the gradient of the objective function (optional)
objective_gradient <- function(x) {
  return(c(-0.2, -1.1))
}

# Define the constraint functions
constraint_matrix <- matrix(c(1.08, 3.27,
                              -1, 0,
                               0, -1), nrow = 3, byrow = TRUE)
constraint_vector <- c(7.35, -2, -1)

# Initial guess
initial_guess <- c(1, 1)

# Lower bounds
lower_bounds <- c(1, 1)

# Solve the problem
result <- constrOptim(theta = initial_guess,
                      f = objective_function,
                      grad = objective_gradient,
                      ui = constraint_matrix,
                      ci = constraint_vector,
                      method = "L-BFGS-B",
                      lower = lower_bounds,
                      control = list(fnscale = -1))

# Extract solution values
coffee_purchased <- round(result$par[1])
mello_yello_purchased <- round(result$par[2])
total_items <- coffee_purchased + mello_yello_purchased

# Calculate total cost and remaining budget
costs <- c(1.08, 3.27)
total_cost <- costs[1] * coffee_purchased + costs[2] * mello_yello_purchased
remaining_budget <- 7.35 - total_cost

# Print results
cat("Status:", ifelse(result$convergence == 0, "Optimal", "Not Optimal"), "\n")
cat("Coffee:", coffee_purchased, "\n")
cat("Mello Yello Zero:", mello_yello_purchased, "\n")
cat("Total items:", total_items, "\n")
cat("Remaining budget:", round(remaining_budget, 2), "\n")
