# Load required packages
library(parallel)
library(doParallel)
library(foreach)

# Set a small tolerance for floating-point comparison
tolerance <- 1e-10

# Function to perform simulations
simulate <- function(iterations) {
  success_count <- 0
  trial_count <- 0  # Keep track of the number of trials (iterations actually processed)
  
  for (i in 1:iterations) {
    # Generate random coordinates for blue point B and red point R
    xB <- runif(1)
    yB <- runif(1)
    xR <- runif(1)
    yR <- runif(1)
    
    # Compute distances from B to each side of the square
    dLeft   <- xB       # Distance to x = 0 (left side)
    dRight  <- 1 - xB   # Distance to x = 1 (right side)
    dBottom <- yB       # Distance to y = 0 (bottom side)
    dTop    <- 1 - yB   # Distance to y = 1 (top side)
    
    # Find the minimum distance(s) to identify the closest side(s)
    minDist <- min(dLeft, dRight, dBottom, dTop)
    
    # Identify the sides that are closest to B
    closest_sides <- c()
    if (abs(dLeft - minDist) < tolerance)   closest_sides <- c(closest_sides, "Left")
    if (abs(dRight - minDist) < tolerance)  closest_sides <- c(closest_sides, "Right")
    if (abs(dBottom - minDist) < tolerance) closest_sides <- c(closest_sides, "Bottom")
    if (abs(dTop - minDist) < tolerance)    closest_sides <- c(closest_sides, "Top")
    
    # Compute the perpendicular bisector of BR
    mid_x <- (xB + xR) / 2
    mid_y <- (yB + yR) / 2
    dx <- xR - xB
    dy <- yR - yB
    
    if (abs(dx) < tolerance && abs(dy) < tolerance) {
      # B and R are the same point; skip this iteration
      next
    }
    
    trial_count <- trial_count + 1  # Increment trial count
    
    # Slope of BR
    if (abs(dx) < tolerance) {
      slope_BR <- Inf
    } else {
      slope_BR <- dy / dx
    }
    
    # Slope of the perpendicular bisector
    if (abs(slope_BR) < tolerance) {
      slope_perp <- Inf
    } else if (is.infinite(slope_BR)) {
      slope_perp <- 0
    } else {
      slope_perp <- -1 / slope_BR
    }
    
    # Flag to check if any side satisfies the condition
    found <- FALSE
    
    # Check intersection with the closest sides
    for (side in closest_sides) {
      if (side == "Left") {
        xS <- 0
        if (is.infinite(slope_perp)) {
          # Perpendicular bisector is vertical
          x_intersect <- mid_x
          y_intersect <- NA
        } else {
          yS <- slope_perp * (xS - mid_x) + mid_y
          y_intersect <- yS
        }
        if (!is.na(y_intersect) && y_intersect >= -tolerance && y_intersect <= 1 + tolerance) {
          found <- TRUE
          break
        }
      } else if (side == "Right") {
        xS <- 1
        if (is.infinite(slope_perp)) {
          x_intersect <- mid_x
          y_intersect <- NA
        } else {
          yS <- slope_perp * (xS - mid_x) + mid_y
          y_intersect <- yS
        }
        if (!is.na(y_intersect) && y_intersect >= -tolerance && y_intersect <= 1 + tolerance) {
          found <- TRUE
          break
        }
      } else if (side == "Bottom") {
        yS <- 0
        if (is.infinite(slope_perp)) {
          y_intersect <- mid_y
          x_intersect <- NA
        } else {
          if (slope_perp == 0) {
            y_intersect <- mid_y
            x_intersect <- NA
          } else {
            xS <- (yS - mid_y) / slope_perp + mid_x
            x_intersect <- xS
          }
        }
        if (!is.na(x_intersect) && x_intersect >= -tolerance && x_intersect <= 1 + tolerance) {
          found <- TRUE
          break
        }
      } else if (side == "Top") {
        yS <- 1
        if (is.infinite(slope_perp)) {
          y_intersect <- mid_y
          x_intersect <- NA
        } else {
          if (slope_perp == 0) {
            y_intersect <- mid_y
            x_intersect <- NA
          } else {
            xS <- (yS - mid_y) / slope_perp + mid_x
            x_intersect <- xS
          }
        }
        if (!is.na(x_intersect) && x_intersect >= -tolerance && x_intersect <= 1 + tolerance) {
          found <- TRUE
          break
        }
      }
    }
    
    # Increment success count if condition is satisfied
    if (found) {
      success_count <- success_count + 1
    }
  }
  
  # Return a named vector
  return(c(success_count = success_count, trial_count = trial_count))
}

# Determine the number of cores available
num_cores <- detectCores() - 1  # Use one less than the total number of cores to prevent system overload

# Set up the cluster
cl <- makeCluster(num_cores)
registerDoParallel(cl)

# Set the number of simulations
N <- 1e8  # 100 million simulations for higher accuracy

# Divide the simulations among the cores
simulations_per_core <- floor(N / num_cores)

# Adjust N in case it doesn't divide evenly
N <- simulations_per_core * num_cores

# Run simulations in parallel using foreach
results_matrix <- foreach(i = 1:num_cores, .combine = rbind) %dopar% {
  simulate(simulations_per_core)
}

# Stop the cluster
stopCluster(cl)

# Aggregate results
total_successes <- sum(results_matrix[, "success_count"])
total_trials <- sum(results_matrix[, "trial_count"])

# Estimate the probability
probability <- total_successes / total_trials

# Calculate standard error
standard_error <- sqrt(probability * (1 - probability) / total_trials)

# Calculate 95% confidence interval
z_score <- qnorm(0.975)  # 97.5th percentile for two-tailed 95% CI
lower_bound <- probability - z_score * standard_error
upper_bound <- probability + z_score * standard_error

# Print the estimated probability and confidence interval
cat(sprintf("Estimated Probability: %.10f\n", probability))
cat(sprintf("Standard Error: %.10e\n", standard_error))
cat(sprintf("95%% Confidence Interval: [%.10f, %.10f]\n", lower_bound, upper_bound))

