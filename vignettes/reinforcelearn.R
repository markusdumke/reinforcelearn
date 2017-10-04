## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("markdumke/reinforcelearn")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
library(reinforcelearn)

P = array(0, c(2,2,2))
P[, , 1] = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow = TRUE)
P[, , 2] = matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow = TRUE)
R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)  
env = makeEnvironment(transitions = P, rewards = R)

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
# Solve the windy gridworld task using Sarsa.
env = windyGridworld()
res = sarsa(env)

