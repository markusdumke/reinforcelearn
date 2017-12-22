## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = TRUE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
set.seed(12)
library(reinforcelearn)

## ------------------------------------------------------------------------
env = makeEnvironment("gridworld", shape = c(3, 3), goal.states = 0L)
agent = makeAgent(policy = "softmax", val.fun = "table", algorithm = "qlearning")

## ------------------------------------------------------------------------
interact(env, agent, n.episodes = 5L)

## ------------------------------------------------------------------------
getValueFunction(agent)

## ------------------------------------------------------------------------
# Uniform random policy
makePolicy("random")

# Epsilon-greedy policy
makePolicy("epsilon.greedy", epsilon = 0.2)

# Softmax policy
makePolicy("softmax")

## ------------------------------------------------------------------------
makeValueFunction("table", n.states = 9L, n.actions = 4L)

## ---- eval = FALSE-------------------------------------------------------
#  library(keras)
#  model = keras_model_sequential() %>%
#    layer_dense(shape = 10L, input_shape = 4L, activation = "linear") %>%
#    compile(optimizer = optimizer_sgd(lr = 0.1), loss = "mae")
#  makeValueFunction("neural.network", model)

## ------------------------------------------------------------------------
makeAlgorithm("qlearning")

## ------------------------------------------------------------------------
policy = makePolicy("epsilon.greedy", epsilon = 0.2)
val.fun = makeValueFunction("table", n.states = 9L, n.actions = 4L)
algorithm = makeAlgorithm("qlearning")

agent = makeAgent(policy, val.fun, algorithm)

## ------------------------------------------------------------------------
agent = makeAgent("epsilon.greedy", "table", "qlearning", 
  policy.args = list(epsilon = 0.2))

## ------------------------------------------------------------------------
env = makeEnvironment("gridworld", shape = c(3, 2), goal.states = 0L)
agent = makeAgent("random")

interact(env, agent, n.steps = 3L, visualize = TRUE)

## ------------------------------------------------------------------------
env = makeEnvironment("gridworld", shape = c(4, 4), goal.states = 0L, 
  initial.state = 15L)
agent = makeAgent("random")

for (i in 1:3L) {
  ## comment in the next line to wait on enter press before taking the next action.
  # invisible(readline(prompt = "Press [enter] to take the next action"))
  interact(env, agent, n.steps = 1L, learn = FALSE, visualize = TRUE)
}

## ------------------------------------------------------------------------
(memory = makeReplayMemory(size = 2L, batch.size = 1L))

agent = makeAgent("random", replay.memory = memory)

interact(env, agent, n.steps = 2L, visualize = TRUE)

getReplayMemory(agent)

## ---- message = FALSE----------------------------------------------------
env = makeEnvironment("gridworld", shape = c(4, 4), goal.states = c(0, 15))

policy = makePolicy("epsilon.greedy", epsilon = 0.1)
memory = makeReplayMemory(size = 100L, batch.size = 20L)

agent = makeAgent(policy, "table", "qlearning", replay.memory = memory)

for (i in 1:100) {
  interact(env, agent, n.steps = 20L, learn = FALSE)
  interact(env, agent, n.steps = 1L, learn = TRUE)
}
action.vals = getValueFunction(agent)
matrix(getStateValues(action.vals), ncol = 4L)

