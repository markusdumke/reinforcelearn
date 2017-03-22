## ---- eval = FALSE-------------------------------------------------------
#  library(reinforcelearn)
#  options(gym.api.path = "C:/Users/M/Downloads/WinPython-64bit-3.6.0.1Qt5/scripts/gym-http-api")
#  MountainCar = makeEnvironment("MountainCarEasy-v0", max.steps.episode = 10000)

## ---- results = "hide", eval = FALSE-------------------------------------
#  weights = qlearning_approx(MountainCar, make_feature_vector, n.features = 10,
#     state.space.bounds = MountainCar$state.space.bounds, n.grid = 10, n.episodes = 10)

