#' ---
#' title: "Reinforcement Learning API"
#' author: Markus Dumke
#' output: github_document
#' ---

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(comment = "#>", collapse = FALSE, message = TRUE)

#'
# Switch to branch "markus"
devtools::load_all()
library(keras)

#' Environment
env = GridworldEnvironment$new(shape = c(4, 4), goal.states = c(0), initial.state = 15)

#' State Preprocessor
preprocessState = function(state) {
  reinforcelearn::nHot(state + 1, env$n.states)
}

(s = env$reset())
(s = preprocessState(s))

#' ActionValueNetwork
model.keras = keras_model_sequential()
model.keras %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
  use_bias = FALSE, kernel_initializer = "zeros")
keras::compile(model.keras, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))

action.vals = ActionValueNetwork$new(model.keras, preprocessState)
action.vals$model
(Q = action.vals$predictQ(s))
# action.vals$train(state, target)

#' Policy
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)
policy$epsilon
(probs = policy$getActionProbs(Q))
(action = policy$sampleAction(probs))

#' Learner
learner = QLearning$new()

#' Agent
agent = Agent$new(learner, action.vals, policy)

#' Interaction
interaction(env, agent, n.steps = 200)

#' Get Action Value Function
agent$action.value$model %>% get_weights()
