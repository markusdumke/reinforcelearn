## ---- eval = FALSE-------------------------------------------------------
#  install.packages("devtools")
#  devtools::install_github("markdumke/reinforcelearn")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
t = gridworld$transitions
r = gridworld$rewards
Env = makeEnvironment(transition.array = t, reward.matrix = r)

## ---- eval = FALSE-------------------------------------------------------
#  MCar = makeEnvironment("MountainCar-v0")

## ---- eval = FALSE-------------------------------------------------------
#  MCar$action.space # Discrete
#  MCar$state.space.bounds # [-1.2, 0.6], [-0.07, 0.07]

## ---- eval = FALSE-------------------------------------------------------
#  MCar$reset()
#  for (i in 1:200) {
#    action = sample(MCar$actions, 1)
#    MCar$step(action)
#  }
#  MCar$close()

## ---- eval = FALSE-------------------------------------------------------
#  res = iterateValue(Env)

## ---- eval = FALSE-------------------------------------------------------
#  

