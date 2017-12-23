#' #' ---
#' #' title: "User interface"
#' #' author: Markus Dumke
#' #' output: github_document
#' #' ---
#'
#' #+ setup, include=FALSE
#' library(knitr)
#' opts_chunk$set(comment = "#>", collapse = FALSE, message = FALSE)
#'
#' library(reinforcelearn)
#'
#' env = makeEnvironment("windy.gridworld")
#'
#' # policy without val.fun or algorithm
#' agent = makeAgent("random")
#' interact(env, agent, n.steps = 10L)
#'
#' # policy with val.fun, without algorithm
#' agent = makeAgent("softmax", "table")
#' interact(env, agent, n.steps = 10L)
#'
#' # policy, table, qlearning
#' agent = makeAgent("softmax", "table", "qlearning")
#' interact(env, agent, n.steps = 10L)
#'
#' # policy, table, qlearning, eligibility
#' alg = makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")
#' agent = makeAgent("softmax", "table", alg)
#' interact(env, agent, n.steps = 10L)
#'
#' # policy, table, qlearning, exp.replay
#' mem = makeReplayMemory(size = 5, batch.size = 5)
#' agent = makeAgent("softmax", "table", "qlearning", experience.replay = mem)
#' interact(env, agent, n.steps = 10L)
#'
#' # policy, neuralnet, qlearning
#' library(keras)
#' model = keras_model_sequential() %>%
#'   layer_dense(units = env$n.actions, activation = "linear",
#'     input_shape = c(env$n.states), kernel_initializer = initializer_zeros(),
#'     use_bias = FALSE) %>%
#'   compile(loss = "mae", optimizer = optimizer_sgd(lr = 1))
#' val = makeValueFunction("neural.network", model = model)
#' preprocess = function(x) to_categorical(x, num_classes = env$n.states)
#' agent = makeAgent("softmax", val, "qlearning", preprocess = preprocess)
#'
#' # policy, neuralnet, qlearning, exp. replay
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#' #
#' #
#' #
#' #
#' # # run random policy without learning
#' # env = makeEnvironment("gridworld", shape = c(4, 4),
#' #   goal.states = 0L, initial.state = 15L, discount = 0.99)
#' # policy = makePolicy("random")
#' # agent = makeAgent(policy)
#' # interact(env, agent, n.steps = 200L)
#' #
#' # # qlearning table
#' # env = makeEnvironment("gridworld", shape = c(4, 4),
#' #   goal.states = c(0, 15), initial.state = 1:14, discount = 1)
#' # val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
#' # alg = makeAlgorithm("qlearning")
#' # agent = makeAgent(policy, val, alg)
#' # interact(env, agent, n.episodes = 50L) # fail
#' # getStateValues(agent$val.fun$Q)
#' #
#' # # qlearning simple
#' # env = makeEnvironment("windy.gridworld")
#' # val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
#' # policy = makePolicy("epsilon.greedy", epsilon = 0.1)
#' # alg = makeAlgorithm("qlearning")
#' # agent = makeAgent(policy, val, alg)
#' # interact(env, agent, n.episodes = 100L)
#' #
#' # # sarsa simple
#' # env = makeEnvironment("windy.gridworld")
#' # val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
#' # policy = makePolicy("epsilon.greedy", epsilon = 0.1)
#' # alg = makeAlgorithm("sarsa")
#' # agent = makeAgent(policy, val, alg)
#' # interact(env, agent, n.episodes = 100L)
#' #
#' # # sarsa simple with softmax policy
#' # env = makeEnvironment("windy.gridworld")
#' # val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
#' # policy = makePolicy("softmax")
#' # alg = makeAlgorithm("sarsa")
#' # agent = makeAgent(policy, val, alg)
#' # interact(env, agent, n.episodes = 100L)
#' #
#' # # qlearning eligibility traces
#' # env = makeEnvironment("windy.gridworld")
#' # val = makeValueFunction("table", n.states = env$n.states, n.actions = env$n.actions)
#' # policy = makePolicy("epsilon.greedy", epsilon = 0.1)
#' # alg = makeAlgorithm("qlearning", lambda = 0.9, traces = "accumulate")
#' # agent = makeAgent(policy, val, alg)
#' # interact(env, agent, n.episodes = 100L)
#' #
#' # # character arguments
#' # env = makeEnvironment("windy.gridworld")
#' # agent = makeAgent("softmax", "table", "qlearning")
#' # interact(env, agent, n.episodes = 10L)
#' #
#' # env = makeEnvironment("windy.gridworld")
#' # alg = makeAlgorithm("qlearning", lambda = 0.9, traces = "replace")
#' # agent = makeAgent("softmax", "table", alg)
#' # interact(env, agent, n.episodes = 10L)
#' #
#' # # qlearning experience replay
#' # env = makeEnvironment("windy.gridworld")
#' # policy = makePolicy("epsilon.greedy", epsilon = 0.1)
#' # replay = makeReplayMemory(size = 200L, batch.size = 150L)
#' # agent = makeAgent(policy, "table", "qlearning", experience.replay = replay) # a bit slow
#' # interact(env, agent, n.episodes = 100L)
#' #
#' # # exp replay train every 10 steps
#' # env = makeEnvironment("windy.gridworld")
#' # policy = makePolicy("epsilon.greedy", epsilon = 0.1)
#' # replay = makeReplayMemory(size = 100L, batch.size = 100L)
#' # agent = makeAgent(policy, "table", "qlearning", experience.replay = replay) # a bit slow
#' # for (i in 1:10000) {
#' #   interact(env, agent, n.steps = 100L, learn = FALSE)
#' #   interact(env, agent, n.steps = 1L, learn = TRUE)
#' # }
#' #
#' #
#' # # keras neural network
#' # env = makeEnvironment("windy.gridworld")
#' # library(keras)
#' # model = keras_model_sequential()
#' # # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
#' # model %>%
#' #   layer_dense(units = env$n.actions, activation = "linear", input_shape = c(env$n.states),
#' #     kernel_initializer = initializer_zeros(), use_bias = FALSE)
#' #   #layer_dense(units = env$n.actions, activation = "linear")
#' # model$compile(loss = "mae", optimizer = optimizer_sgd(lr = 1))
#' # val = makeValueFunction("neural.network", model = model)
#' # replay = makeReplayMemory(size = 100L, batch.size = 10L)
#' # preprocess = function(x) to_categorical(x, num_classes = env$n.states)
#' # agent = makeAgent("softmax", val, "qlearning",
#' #   preprocess = preprocess, experience.replay = replay)
#' # for (i in 1:100) {
#' #   interact(env, agent, n.steps = 10L, learn = FALSE, max.steps.per.episode = 100L)
#' #   interact(env, agent, n.steps = 1L, learn = TRUE, max.steps.per.episode = 100L)
#' # }
#' # agent$val.fun$model %>% get_weights()
#' #
#' # # solve mountain car with exp replay
#' # m = makeEnvironment("gym", "MountainCar-v0")
#' # library(keras)
#' # model = keras_model_sequential()
#' # # "input_shape" parameter for layer_dense should be  c(batchsize(None), input_dim), dim in keras is row major
#' # model %>%
#' #   layer_dense(units = 64L, activation = 'relu', input_shape = c(2L)) %>%
#' #   layer_dense(units = 3L, activation = 'linear')
#' # model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))
#' # val = makeValueFunction("neural.network", model = model)
#' # replay = makeReplayMemory(size = 100L, batch.size = 10L)
#' # preprocess = function(x) matrix(x, ncol = 2)
#' # agent = makeAgent("softmax", val, "qlearning",
#' #   preprocess = preprocess, experience.replay = replay)
#' # for (i in 1:1000) {
#' #   interact(env, agent, n.steps = 10L, learn = FALSE)
#' #   interact(env, agent, n.steps = 1L, learn = TRUE)
#' # }
#' # #agent$val.fun$model %>% get_weights()
