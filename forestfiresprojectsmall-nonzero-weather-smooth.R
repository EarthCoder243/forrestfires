library(MASS)

# ---------------------------------------------------------
# Read data
# ---------------------------------------------------------
fires <- read.csv("forestfires.csv")

# Keep only nonzero small fires: 0 < area < 20
fires_small_pos <- subset(fires, area > 0 & area < 20)

# Log-transformed response
fires_small_pos$log_area1 <- log(fires_small_pos$area + 1)

########################################################
## Nonzero Small Fires, Meteorological Variables Only ##
########################################################

# Full weather-only linear model
weather_full <- lm(
  log_area1 ~ temp + RH + wind + rain,
  data = fires_small_pos
)
summary(weather_full)

# Stepwise AIC on the weather-only model
weather_step <- stepAIC(weather_full, direction = "both")
summary(weather_step)

# AIC and MSE for reporting
AIC(weather_full)
AIC(weather_step)

weather_full_MSE <- sum(residuals(weather_full)^2) / weather_full$df.residual
weather_step_MSE <- sum(residuals(weather_step)^2) / weather_step$df.residual

weather_full_MSE
weather_step_MSE

###########################
## Kernel + Spline Fits  ##
###########################

# Helper: function to make scatter + ksmooth + spline in a new window
plot_smooth <- function(x, y, xlab, main, bandwidth = 2) {
  # keep only finite values
  ok <- is.finite(x) & is.finite(y)
  x  <- x[ok]
  y  <- y[ok]
  
  # Kernel regression (Nadaraya–Watson) using ksmooth
  ks <- ksmooth(x, y, kernel = "normal", bandwidth = bandwidth)
  
  # New graphics window
  x11()
  plot(x, y,
       xlab = xlab,
       ylab = "log(area + 1)",
       main = main)
  
  # Add kernel smoother
  lines(ks, lwd = 2)
  
  # Try smoothing spline; if it fails, just skip it
  if (length(unique(x)) > 3) {
    sp <- try(smooth.spline(x, y), silent = TRUE)
    if (!inherits(sp, "try-error")) {
      lines(sp, lty = 2)
    }
  }
}

####################################
## temp vs log(area+1) with smoothers
####################################
plot_smooth(
  x    = fires_small_pos$temp,
  y    = fires_small_pos$log_area1,
  xlab = "Temperature (temp)",
  main = "Nonzero Small Fires: log(area+1) vs temp\nksmooth (solid) & spline (dashed)",
  bandwidth = 4
)

####################################
## RH vs log(area+1) with smoothers
####################################
plot_smooth(
  x    = fires_small_pos$RH,
  y    = fires_small_pos$log_area1,
  xlab = "Relative Humidity (RH)",
  main = "Nonzero Small Fires: log(area+1) vs RH\nksmooth (solid) & spline (dashed)",
  bandwidth = 10
)

####################################
## wind vs log(area+1) with smoothers
####################################
plot_smooth(
  x    = fires_small_pos$wind,
  y    = fires_small_pos$log_area1,
  xlab = "Wind",
  main = "Nonzero Small Fires: log(area+1) vs wind\nksmooth (solid) & spline (dashed)",
  bandwidth = 2
)

####################################
## rain vs log(area+1) with smoothers
####################################
plot_smooth(
  x    = fires_small_pos$rain,
  y    = fires_small_pos$log_area1,
  xlab = "Rain",
  main = "Nonzero Small Fires: log(area+1) vs rain\nksmooth (solid) & spline (dashed)",
  bandwidth = 1
)
