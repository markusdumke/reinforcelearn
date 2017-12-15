# user interface
library(reinforcelearn)
env = makeEnvironment("Gridworld", shape = c(4, 4),
  goal.states = 0L, initial.state = 15L)

policy = makePolicy("random")

agent = makeAgent(policy)

interact(env, agent, n.steps = 200L)
