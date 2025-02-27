library(parallel)

simulate_circle <- function(x) {
  point1 <- runif(2)
  point2 <- runif(2)
  
  point1_x <- point1[[1]]
  point1_y <- point1[[2]]
  
  point2_x <- point2[[1]]
  point2_y <- point2[[2]]
  
  x_diff <- point2_x - point1_x
  y_diff <- point2_y - point1_y
  
  circumference <- sqrt(x_diff^2 + y_diff^2)
  radius <- circumference/2
  
  centre <- list(0.5 * (point1_x + point2_x), 0.5 * (point1_y + point2_y))
  centre_x <- centre[[1]]
  centre_y <- centre[[2]]
  
  closest_dist <- min(centre_x, centre_y, 1 - centre_x, 1 - centre_y)
  
  if(closest_dist < radius) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

num_iterations <- 100000000

n_reps <- rep(1, num_iterations)

results <- mclapply(n_reps, simulate_circle, mc.cores = 20)

proportion_true <- sum(unlist(results)) / num_iterations
cat("Proportion of TRUE:", proportion_true)
