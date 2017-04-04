## ---- eval = TRUE--------------------------------------------------------
library(reinforcelearn)
options(gym.api.path = "C:/Users/M/Downloads/WinPython-64bit-3.6.0.1Qt5/scripts/gym-http-api")
MountainCar = makeEnvironment("MountainCarEasy-v0")

## ---- results = "hide", eval = FALSE-------------------------------------
#  res = qnetwork(MountainCar, makeGridTiling, n.grid = 10, n.episodes = 100,
#    state.space.bounds = MountainCar$state.space.bounds, n.weights = 100L)

## ---- eval = FALSE-------------------------------------------------------
#  plot(1:100, res$steps.per.episode[1:100], ylim = c(0, 1000),
#     type = "l", xlab = "Episode", ylab = "Steps per Episode")

