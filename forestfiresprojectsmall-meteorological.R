library(MASS)

# Read data
fires <- read.csv("/Users/owendouphner/Desktop/CSUN Fall 2025/Math444/Project 1 (Forest Fires)/forest+fires/forestfires.csv")

# Keep only nonzero small fires: 0 < area < 20
fires_small_pos <- subset(fires, area > 0 & area < 20)

# Add log-transformed response
fires_small_pos$log_area1 <- log(fires_small_pos$area + 1)

########################################################
## Nonzero Small Fires, Meteorological Variables Only ##
########################################################

# Full weather-only model (temp, RH, wind, rain)
weather_full <- lm(
  log_area1 ~ temp + RH + wind + rain,
  data = fires_small_pos
)
summary(weather_full)

# Stepwise AIC on the weather-only model
weather_step <- stepAIC(weather_full, direction = "both")
summary(weather_step)

# Optionally compute AIC and MSE for reporting
AIC(weather_full)
AIC(weather_step)

weather_full_MSE <- sum(residuals(weather_full)^2) / weather_full$df.residual
weather_step_MSE <- sum(residuals(weather_step)^2) / weather_step$df.residual

weather_full_MSE
weather_step_MSE

#####################
## Diagnostic Plots ##
#####################

# Each plot in a new X11 window (Mac with XQuartz)
x11()
plot(fires_small_pos$temp, fires_small_pos$log_area1,
     xlab = "Temperature (temp)",
     ylab = "log(area + 1)",
     main = "Nonzero Small Fires: log(area+1) vs temp")

x11()
plot(fires_small_pos$RH, fires_small_pos$log_area1,
     xlab = "Relative Humidity (RH)",
     ylab = "log(area + 1)",
     main = "Nonzero Small Fires: log(area+1) vs RH")

x11()
plot(fires_small_pos$wind, fires_small_pos$log_area1,
     xlab = "Wind",
     ylab = "log(area + 1)",
     main = "Nonzero Small Fires: log(area+1) vs wind")

x11()
plot(fires_small_pos$rain, fires_small_pos$log_area1,
     xlab = "Rain",
     ylab = "log(area + 1)",
     main = "Nonzero Small Fires: log(area+1) vs rain")
