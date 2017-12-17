# user interface
library(reinforcelearn)

# without learning
env = makeEnvironment("gridworld", shape = c(4, 4),
  goal.states = 0L, initial.state = 15L, discount = 0.99)
policy = makePolicy("random")
agent = makeAgent(policy)
interact(env, agent, n.steps = 200L)

# qlearning simple
val = makeValueFunction("table", n.states = 16L, n.actions = 4L)
alg = makeAlgorithm("qlearning")
agent = makeAgent(policy, val, alg)
env = makeEnvironment("gridworld", shape = c(4, 4),
  goal.states = c(0, 15), initial.state = 1:14, discount = 1)
interact(env, agent, n.episodes = 50L)
getStateValues(agent$val.fun$Q)

# qlearning simple
env = makeEnvironment("windy.gridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("qlearning")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# sarsa simple
env = makeEnvironment("windy.gridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("sarsa")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# sarsa simple with softmax policy
env = makeEnvironment("windy.gridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("softmax")
alg = makeAlgorithm("sarsa")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# qlearning eligibility traces
env = makeEnvironment("windy.gridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("qlearning", lambda = 0.9, traces = "accumulate")
agent = makeAgent(policy, val, alg)
interact(env, agent, n.episodes = 100L)

# character arguments
env = makeEnvironment("windy.gridworld")
agent = makeAgent("softmax", "table", "qlearning")
interact(env, agent, n.episodes = 10L)

env = makeEnvironment("windy.gridworld")
alg = makeAlgorithm("qlearning", lambda = 0.9, traces = "replace")
agent = makeAgent("softmax", "table", alg)
interact(env, agent, n.episodes = 10L)

# qlearning experience replay
env = makeEnvironment("windy.gridworld")
val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
policy = makePolicy("epsilon.greedy", epsilon = 0.1)
alg = makeAlgorithm("qlearning")
replay = makeReplayMemory(size = 200L, batch.size = 5L)
agent = makeAgent(policy, val, alg, experience.replay = replay)
interact(env, agent, n.episodes = 100L)


# # keras neural network
# library(keras)
# model = keras_model_sequential()
# # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
# model %>%
#   layer_dense(units = 64L, activation = 'relu', input_shape = c(input_shape)) %>%
#   layer_dense(units = output_shape, activation = 'linear')
# model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
