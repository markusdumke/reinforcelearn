# user interface
library(reinforcelearn)
env = makeEnvironment("Gridworld", shape = c(4, 4),
  goal.states = 0L, initial.state = 15L, discount = 0.99)

policy = makePolicy("epsilon.greedy", epsilon = 0.4)
policy = makePolicy("random")

agent = makeAgent(policy)

# be able to act without specifying value.fun or algorithm
interact(env, agent, n.steps = 200L)

val = makeValueFunction("table", n.states = 16L, n.actions = 4L)

alg = makeAlgorithm("qlearning")

agent = makeAgent(policy, val, alg)

env = makeEnvironment("Gridworld", shape = c(4, 4),
  goal.states = c(0, 15), initial.state = 1:14, discount = 1)

interact(env, agent, n.episodes = 50L)

getStateValues(agent$val.fun$Q)
