library(MASS)

# ---------------------------------------------------------
# Read data
# ---------------------------------------------------------
fires <- read.csv("forestfires.csv")

# Keep only large fires (and nonzero implicitly)
fires_large <- subset(fires, area >= 20)

# Log-transformed response (no zeros here)
fires_large$log_area <- log(fires_large$area)

####################################################################
## CV-based bandwidth selection for ksmooth (from lecture, adapted)
####################################################################

CV.ksmooth <- function(x, y) {
  # Leave-one-out cross-validation for ksmooth bandwidth h
  error <- function(h) {
    n <- length(y)
    estimated <- numeric(n)
    for (i in 1:n) {
      x_reduced <- x[-i]
      y_reduced <- y[-i]
      # ksmooth evaluated at x[i] using training data only
      est_i <- ksmooth(x_reduced, y_reduced, "normal",
                       bandwidth = h, x.points = x[i])$y
      estimated[i] <- est_i
    }
    # mean squared prediction error
    return(sum((y - estimated)^2) / n)
  }
  
  # Optimize h over a reasonable range for FWI variables
  optimum_h <- optimize(error, interval = c(0.1, 200))$minimum
  
  # Fit final ksmooth model using optimal h
  CV_model <- ksmooth(x, y, "normal", bandwidth = optimum_h)
  list(x = CV_model$x, y = CV_model$y, h = optimum_h)
}

####################################################################
## Helper: plot log(area) vs predictor + CV-chosen ksmooth curve
####################################################################

plot_large_with_ksmooth <- function(df, xvar) {
  x <- df[[xvar]]
  y <- df$log_area
  
  # Remove non-finite values if any
  ok <- is.finite(x) & is.finite(y)
  x <- x[ok]
  y <- y[ok]
  
  # Run CV.ksmooth to pick optimal bandwidth and fit curve
  out <- CV.ksmooth(x, y)
  
  # New window for each variable
  x11()
  plot(x, y,
       xlab = xvar,
       ylab = "log(area)",
       main = paste("Large Fires (area ≥ 20 ha): log(area) vs", xvar))
  lines(out$x, out$y, lwd = 3)
  
  cat("\n==========================================\n")
  cat("Large fires - CV.ksmooth result for", xvar, "\n")
  cat("Optimal bandwidth h =", out$h, "\n")
  cat("==========================================\n\n")
  
  # ---- NEW: fit and print linear model summary for metrics ----
  lm_fit <- lm(y ~ x)
  cat("Linear model summary for", xvar, " (large fires):\n")
  print(summary(lm_fit))
  cat("--------------------------------------------------\n\n")
}

###############################################
## Apply to FWI variables for LARGE fires only
###############################################

# FFMC
plot_large_with_ksmooth(fires_large, "FFMC")

# DMC
plot_large_with_ksmooth(fires_large, "DMC")

# DC
plot_large_with_ksmooth(fires_large, "DC")

# ISI
plot_large_with_ksmooth(fires_large, "ISI")
