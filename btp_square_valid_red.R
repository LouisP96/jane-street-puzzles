# Set fixed coordinates for blue point B
xB <- 0.4
yB <- 0.2

# Number of simulations
N <- 1e5  # Adjust as needed

# Initialize vectors to store xR and yR that satisfy the condition
xR_valid <- numeric()
yR_valid <- numeric()

# Set a small tolerance for floating-point comparison
tolerance <- 1e-10

# Compute distances from B to each side
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

# Run the simulations
for (i in 1:N) {
  # Generate random coordinates for red point R
  xR <- runif(1)
  yR <- runif(1)
  
  # Compute the perpendicular bisector of BR
  mid_x <- (xB + xR) / 2
  mid_y <- (yB + yR) / 2
  dx <- xR - xB
  dy <- yR - yB
  
  if (abs(dx) < tolerance && abs(dy) < tolerance) {
    # B and R are the same point; skip this iteration
    next
  }
  
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
  
  # Flag to check if condition is satisfied
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
          # Perpendicular bisector is horizontal
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
  
  # If condition is satisfied, store xR and yR
  if (found) {
    xR_valid <- c(xR_valid, xR)
    yR_valid <- c(yR_valid, yR)
  }
}

# Plot the points
plot(NA, xlim = c(0, 1), ylim = c(0, 1), xlab = "X", ylab = "Y",
     main = "Red Points Where Condition is Satisfied", asp = 1)
rect(0, 0, 1, 1, border = "black", lwd = 2)  # Draw the square
points(xR_valid, yR_valid, col = "red", pch = 16, cex = 0.5)
points(xB, yB, col = "blue", pch = 16, cex = 1.5)
text(xB, yB, labels = "B", pos = 3, col = "blue", font = 2)
