# Solve Mountain Car with Deep Q-Learning
devtools::load_all()

# ----
# mountain car
env = MountainCar$new() # my implementation
env$reset()
# env = GymEnvironment$new("MountainCar-v0") # gym implementation
# env = GymEnvironment$new("MountainCarEasy-v0") # gym implementation without step limit
# env$reset()

# ----
# deep neural network
library(keras)
model = keras_model_sequential()
model %>%
  layer_dense(units = 64L, activation = 'relu', input_shape = c(2L)) %>%
  layer_dense(units = 3L, activation = 'linear')
model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))

# needed for gym
preprocess = function(state) {
  matrix(state, ncol = 2L)
}

#----
# linear with grid tiling
n.tilings = 8
max.size = 4096
iht = IHT$new(max.size)

library(keras)
model = keras_model_sequential()
model %>%
  layer_dense(units = 3L, activation = 'linear', input_shape = c(max.size))
model$compile(loss = 'mse', optimizer = optimizer_rmsprop(lr = 0.0025))

position.max = 0.6
position.min = -1.2
velocity.max = 0.07
velocity.min = -0.07
position.scale = n.tilings / (position.max - position.min)
velocity.scale = n.tilings / (velocity.max - velocity.min)

preprocess = function(state) {
  state = matrix(c(position.scale * state[1], velocity.scale * state[2]), ncol = 2)
  active.tiles = tiles(iht, 8, state)
  nHot(active.tiles, max.size)
}

# ----
val = ActionValueNetwork$new(model, preprocess)
# # test that predict works
# preprocess(env$state)
# val$predictQ(preprocess(matrix(1:2, ncol = 2L)))

# ----
# epsilon greedy policy with exp. replay
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)
memory = ExperienceReplay$new(size = 1000L, batch.size = 100L)

alg = QLearning$new()
agent = Agent$new(alg, val, policy, memory)

# learn every 100 steps
agent$learn.logical = FALSE
for (i in 1:1000) {
  interact(env, agent, n.steps = 100L, visualize = FALSE)
  print("L")
  agent$learn()
}
env$gym.env$close()
