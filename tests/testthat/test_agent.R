# #-------------
# # Test observing
#
# env = makeEnvironment("windy.gridworld")
#
# agent = makeAgent("random")
# interact(env, agent, n.steps = 10L, learn = FALSE)
#
# agent = makeAgent("softmax", "table")
# interact(env, agent, n.steps = 10L, learn = FALSE)
#
# agent = makeAgent("random", "table", "qlearning")
# interact(env, agent, n.steps = 10L, learn = FALSE)
#
# agent = makeAgent("random", "table", "qlearning", lambda = 0.8, traces = "replace")
# interact(env, agent, n.steps = 2L, learn = FALSE)
# getEligibilityTraces(agent)
#
# mem = makeReplayMemory(size = 2, batch.size = 1)
# agent = makeAgent("random", "table", "qlearning", experience.replay = mem)
# interact(env, agent, n.steps = 10L, learn = FALSE)
# getReplayMemory(agent)
#
#
# #-------------
# # Test learning
#
# # qlearning table base
# agent = makeAgent("random", "table", "qlearning")
# interact(env, agent, n.steps = 2L, learn = TRUE)
# getValueFunction(agent)
#
# # qlearning table eligibility
# agent = makeAgent("random", "table", "qlearning", lambda = 0.8, traces = "replace")
# interact(env, agent, n.steps = 2L, learn = TRUE)
# getValueFunction(agent)
#
# # qlearning table exp replay
# mem = makeReplayMemory(size = 2L, batch.size = 2L)
# agent = makeAgent("random", "table", "qlearning", experience.replay = mem)
# interact(env, agent, n.steps = 2L, learn = TRUE)
# getValueFunction(agent)
#
# # qlearning neural.network base
# library(keras)
# model = keras_model_sequential() %>%
#   layer_dense(units = env$n.actions, activation = "linear",
#     input_shape = c(env$n.states), kernel_initializer = initializer_zeros(),
#     use_bias = FALSE) %>%
#   compile(loss = "mae", optimizer = optimizer_sgd(lr = 1))
# val = makeValueFunction("neural.network", model = model)
# preprocess = function(x) to_categorical(x, num_classes = env$n.states)
# agent = makeAgent("softmax", val, "qlearning", preprocess = preprocess)
# interact(env, agent, n.steps = 2L, learn = TRUE)
# getValueFunction(agent)
#
# # qlearning neural.network exp.replay
# library(keras)
# model = keras_model_sequential() %>%
#   layer_dense(units = env$n.actions, activation = "linear",
#     input_shape = c(env$n.states), kernel_initializer = initializer_zeros(),
#     use_bias = FALSE) %>%
#   compile(loss = "mae", optimizer = optimizer_sgd(lr = 1))
# mem = makeReplayMemory(size = 2L, batch.size = 2L)
# val = makeValueFunction("neural.network", model = model)
# preprocess = function(x) to_categorical(x, num_classes = env$n.states)
# agent = makeAgent("softmax", val, "qlearning",
#   preprocess = preprocess, experience.replay = mem)
# interact(env, agent, n.steps = 2L, learn = TRUE)
# getValueFunction(agent)
