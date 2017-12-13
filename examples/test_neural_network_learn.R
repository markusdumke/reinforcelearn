# test that experience replay with neural network works
library(keras)
model = keras_model_sequential()
model %>% layer_dense(3, input_shape = 2, activation = "linear",
  use_bias = FALSE, kernel_initializer = initializer_zeros())
keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.02))

val = ActionValueNetwork$new(model)
val$getWeights()
val$predictQ(matrix(1, ncol = 2))
val$train(matrix(1, ncol = 2), target = matrix(c(-1, 0, 0), ncol = 3))
val$getWeights()
val$predictQ(matrix(1, ncol = 2))

####
# solve very simple gridworld
set.seed(123)
env = Gridworld$new(shape = c(2, 2), goal.states = 0L)
library(keras)
model = keras_model_sequential()
model %>% layer_dense(4, input_shape = 4, activation = "linear",
  use_bias = FALSE, kernel_initializer = initializer_zeros())
keras::compile(model, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.02))

preprocess = function(state) {
  nHot(state + 1L, 4L)
}

env$reset()
preprocess(env$state)

val = ActionValueNetwork$new(model, preprocess)
policy = RandomPolicy$new()
alg = QLearning$new()
replay = ExperienceReplay$new(size = 10L, batch.size = 5L)

agent = Agent$new(alg, val, policy, replay)

agent$getWeights()
# val$predictQ(preprocess(env$state))
# debug(interact)
interact(env, agent, n.episodes = 100L)
agent$getWeights()
getStateValues(agent$getWeights()[[1]])

# ok works without exp replay
# works not with exp replay
