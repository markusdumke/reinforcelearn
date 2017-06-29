## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("markdumke/reinforcelearn")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
transitions = gridworld$transitions
rewards = gridworld$rewards
env = makeEnvironment(transition.array = transitions, 
  reward.matrix = rewards)

## ---- eval = FALSE-------------------------------------------------------
#  mcar = makeEnvironment("MountainCar-v0")

## ---- eval = FALSE-------------------------------------------------------
#  mcar$action.space # Discrete
#  mcar$state.space.bounds # [-1.2, 0.6], [-0.07, 0.07]

## ---- eval = FALSE-------------------------------------------------------
#  mcar$reset()
#  for (i in 1:200) {
#    action = sample(mcar$actions, 1)
#    mcar$step(action)
#  }
#  mcar$close()

## ------------------------------------------------------------------------
res = iterateValue(env)

## ---- eval = FALSE-------------------------------------------------------
#  

