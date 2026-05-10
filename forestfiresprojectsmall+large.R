library(MASS)
library(car)
# Read data
fires <- read.csv("forestfires.csv")

attach(fires)

if (interactive()) View(fires)

# ------------------------------------------------------------------
# Split the data into "small" and "large" fires
# Here: small fires are area < 20 ha, large fires are area >= 20 ha
# ------------------------------------------------------------------
fires_small <- subset(fires, area < 20)
fires_large <- subset(fires, area >= 20)

########################################################
## Small Fires Analysis (area < 20) - Main Phenomenon ##
########################################################

# Basic model on small fires
smallBasicModel <- lm(
  log(area + 1) ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain,
  data = fires_small
)
summary(smallBasicModel)

# Stepwise AIC model on small fires
smallStepModel <- stepAIC(smallBasicModel, direction = "both")
summary(smallStepModel)

# "Improved" model using same formula as before, but fit on small fires
improvedModel_small <- lm(
  formula = log(area + 1) ~ month + DMC + temp + DC + wind,
  data = fires_small
)
summary(improvedModel_small)

# Individual predictor models on small fires
ffmc_small      <- lm(log(area + 1) ~ FFMC, data = fires_small)
dmc_small       <- lm(log(area + 1) ~ DMC,  data = fires_small)
dc_small        <- lm(log(area + 1) ~ DC,   data = fires_small)
isi_small       <- lm(log(area + 1) ~ ISI,  data = fires_small)
tempModel_small <- lm(log(area + 1) ~ temp, data = fires_small)
rh_small        <- lm(log(area + 1) ~ RH,   data = fires_small)
windModel_small <- lm(log(area + 1) ~ wind, data = fires_small)
rainModel_small <- lm(log(area + 1) ~ rain, data = fires_small)

summary(ffmc_small)
summary(dmc_small)
summary(dc_small)
summary(isi_small)
summary(tempModel_small)
summary(windModel_small)
summary(rainModel_small)

# Plots for small fires (each in a new X11 window)
x11()
plot(fires_small$temp, log(fires_small$area + 1))

x11()
plot(fires_small$DMC, log(fires_small$area + 1))

x11()
plot(fires_small$wind, log(fires_small$area + 1))

x11()
plot(fires_small$FFMC, log(fires_small$area + 1))

x11()
plot(fires_small$ISI, log(fires_small$area + 1))

x11()
plot(fires_small$DC, log(fires_small$area + 1))

x11()
plot(fires_small$RH, log(fires_small$area + 1))

x11()
plot(fires_small$rain, log(fires_small$area + 1))


##################################
##Final Model after stepwise AIC##
##################################

# For your main report, treat the small-fire stepwise model as the final model
finalModel <- smallStepModel
summary(finalModel)
vif(lm(log(area+1)~., data = fires))

#####################################################
## Large Fires Analysis (area >= 20) - Second Part ##
#####################################################

# Basic model on large fires
largeBasicModel <- lm(
  log(area + 1) ~ X + Y + month + day + FFMC + DMC + DC + ISI + temp + RH + wind + rain,
  data = fires_large
)
summary(largeBasicModel)

# Stepwise AIC model on large fires
largeStepModel <- stepAIC(largeBasicModel, direction = "both")
summary(largeStepModel)

# "Improved" model on large fires, using the same formula structure
improvedModel_large <- lm(
  formula = log(area + 1) ~ month + DMC + temp + DC + wind,
  data = fires_large
)
summary(improvedModel_large)

# Individual predictor models on large fires
ffmc_large      <- lm(log(area + 1) ~ FFMC, data = fires_large)
dmc_large       <- lm(log(area + 1) ~ DMC,  data = fires_large)
dc_large        <- lm(log(area + 1) ~ DC,   data = fires_large)
isi_large       <- lm(log(area + 1) ~ ISI,  data = fires_large)
tempModel_large <- lm(log(area + 1) ~ temp, data = fires_large)
rh_large        <- lm(log(area + 1) ~ RH,   data = fires_large)
windModel_large <- lm(log(area + 1) ~ wind, data = fires_large)
rainModel_large <- lm(log(area + 1) ~ rain, data = fires_large)

summary(ffmc_large)
summary(dmc_large)
summary(dc_large)
summary(isi_large)
summary(tempModel_large)
summary(windModel_large)
summary(rainModel_large)

# Plots for large fires (each in a new X11 window)
x11()
plot(fires_large$temp, log(fires_large$area + 1))

x11()
plot(fires_large$DMC, log(fires_large$area + 1))

x11()
plot(fires_large$wind, log(fires_large$area + 1))

x11()
plot(fires_large$FFMC, log(fires_large$area + 1))

x11()
plot(fires_large$ISI, log(fires_large$area + 1))

x11()
plot(fires_large$DC, log(fires_large$area + 1))

x11()
plot(fires_large$RH, log(fires_large$area + 1))

x11()
plot(fires_large$rain, log(fires_large$area + 1))
