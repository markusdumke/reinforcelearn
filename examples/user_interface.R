# user interface
library(reinforcelearn)

# without learning
env = makeEnvironment("Gridworld", shape = c(4, 4),
  goal.states = 0L, initial.state = 15L, discount = 0.99)
policy = makePolicy("random")
agent = makeAgent(policy)
interact(env, agent, n.steps = 200L)

# qlearning simple
val = makeValueFunction("table", n.states = 16L, n.actions = 4L)
alg = makeAlgorithm("qlearning")
agent = makeAgent(policy, val, alg)
env = makeEnvironment("Gridworld", shape = c(4, 4),
  goal.states = c(0, 15), initial.state = 1:14, discount = 1)
interact(env, agent, n.episodes = 50L)
getStateValues(agent$val.fun$Q)

# qlearning simple
env = makeEnvironment("WindyGridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("qlearning")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# sarsa simple
env = makeEnvironment("WindyGridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("sarsa")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# sarsa simple with softmax policy
env = makeEnvironment("WindyGridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("softmax")
alg = makeAlgorithm("sarsa")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# qlearning eligibility traces
env = makeEnvironment("WindyGridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("qlearning")
# elig = makeEligibility("")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)


# makeAgent(policy = "softmax", val.fun = "table", algorithm = "qlearning")
# setParam(agent, "policy", epsilon = 0.2)


# actor critic
env = makeEnvironment("WindyGridworld")
val = makeValueFunction("table", n.states = env$n.states)
policy = makePolicy("softmax")
alg = makeAlgorithm("qlearning")
