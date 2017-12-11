#' ---
#' title: "Agent"
#' author: Markus Dumke
#' output: github_document
#' ---

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(comment = "#>", collapse = FALSE, message = FALSE)

#'
# Switch to branch "markus"
devtools::load_all()
library(keras)
set.seed(1)

env = GridworldEnvironment$new(shape = c(4, 4), goal.states = 0, initial.state = 15)

#' ----
#'

#' ### Agent Components
#'
#'  - Representation of value function (optional)
#'  - Policy (e.g. epsilon-greedy)
#'  - Algorithm (how to learn, e.g. Q-Learning)
#'

#' ----
#'

#' ### State Preprocessor (user-defined function)
#'
#' - transform the state observation so that the agent can learn on this
preprocess = function(state) {
  reinforcelearn::nHot(state + 1, env$n.states)
}

(s = env$reset())
(phi = preprocess(s))

#' ----
#'


#' ### ActionValueNetwork
#'
#' - value function can be represented as a table or neural network
#' - R6 class to represent value function
#' - user defines keras or mxnet network
#'
#' - `predictQ(state)`
#'     - predict action values given state
#'
#' - `train(state, target)`
#'     - train model on state-target pair
#'     - calling `keras::fit()`
#'
model.keras = keras_model_sequential()
model.keras %>% layer_dense(units = 4, activation = "linear", input_shape = c(16),
  use_bias = FALSE, kernel_initializer = "zeros")
keras::compile(model.keras, loss = "mae", optimizer = keras::optimizer_sgd(lr = 0.4))

action.network = ActionValueNetwork$new(model.keras, preprocess)
action.network$model
(Q = action.network$predictQ(phi))
# action.network$train(state, target)

#' ----
#'

#' ### ActionValueTable
action.table = ActionValueTable$new(env$n.states, env$n.actions, step.size = 1)
action.table$Q
(Q = action.table$predictQ(s))
action.table$train(state = 15, target = matrix(-1, 4))
action.table$Q

#' ----
#'

#' ### Policy
#'
#' - R6 class to represent the policy
#' - `getActionProbs(Q)`
#'     - return probabilities for each action given value function
#'
#' - `sampleAction(probs)`
#'     - sample action from policy probabilities
#'
policy = RandomPolicy$new()
(probs = policy$getActionProbs(Q))

policy = EpsilonGreedyPolicy$new(epsilon = 0.1)
policy$epsilon
(probs = policy$getActionProbs(Q))
(action = policy$sampleAction(probs))

#' ----
#'

#' ### Algorithm
#'
#' - e.g. Q-Learning, Sarsa, ...
#' - `getTarget(reward, discount, action.values)`
#'     - get target used for training (TD target)
#'
algorithm = QLearning$new()
algorithm$getTarget(reward = 1.5, discount = 1, action.values = c(0, 2, 1))

#' ----
#'

#' ### Agent
#'
#' - `act(state)`
#'     - returns action sampled from policy
#' - `learn(state, action , new.state, reward)`
#'     - update value function / policy parameters
#'
agent = Agent$new(algorithm, action.table, policy)
agent$act(state = 1)
# agent$learn()

#' ----
#'

#' ### Interaction
#'
#' - function for interaction
#' - calls `agent$act()`, `environment$step()` and `agent$learn()` in a loop
#' - learning is optional
#' - allows visualization and single step mode
#' - modifies agent and environment
#' - returns number of steps, returns per episode
#' - logging to file possible
#'
interact(env, agent, n.steps = 100L, max.steps.per.episode = 15L, n.episodes = 3L)

#' Get learned action value function
(Q = agent$action.value$Q)
#' Get state value function
matrix(round(apply(Q, 1, max), 2), ncol = 4)

#' Run learned policy
env$reset()
env$visualize()
# act greedily
agent$policy$epsilon = 0 # caveat: may run forever for other seed
for (i in 1:6) {
  ## Comment in next line to step through environment
  # invisible(readline(prompt = "Press [enter] to take the next action"))
  interact(env, agent, 1, learn = FALSE, visualize = TRUE)
}
