## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("markdumke/reinforcelearn")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
env = makeEnvironment(transitions = gridworld$transitions, rewards = gridworld$rewards)

## ---- eval = FALSE-------------------------------------------------------
#  MountainCar = makeEnvironment("MountainCar-v0")

## ---- eval = FALSE-------------------------------------------------------
#  MountainCar$action.space # Discrete
#  MountainCar$state.space.bounds # [-1.2, 0.6], [-0.07, 0.07]

## ---- eval = FALSE-------------------------------------------------------
#  MountainCar$reset()
#  for (i in 1:200) {
#    action = sample(MountainCar$actions, 1)
#    MountainCar$step(action)
#  }
#  MountainCar$close()

## ------------------------------------------------------------------------
# Solve the gridworld task using Value Iteration.
res = iterateValue(env)

