#' ---
#' title: "Reinforcement Learning User API"
#' author: Markus Dumke
#' output: github_document
#' ---

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(comment = "#>", collapse = FALSE, message = TRUE)

#'
devtools::load_all()

#' Environment
env = WindyGridworld$new()
#env = Gridworld$new(shape = c(2L, 2L), goal.states = 0L)

#' State preprocessing

#' ActionValueTable
action.vals = ActionValueTable$new(n.states = env$n.states,
  n.actions = env$n.actions, step.size = 0.1)

#' Policy
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)

#' Algorithm
algorithm = QLearning$new()

#' Experience Replay
replay = ExperienceReplay$new(size = 1000L, batch.size = 32L)

#' Agent
agent = Agent$new(algorithm, action.vals, policy, replay)
# agent = Agent$new(algorithm, action.vals, policy) # no replay

#' Interaction
# agent$learn.logical = FALSE
options(warn = 2) # there is something wrong here
set.seed(123)
interact(env, agent, n.episodes = 100L)
# agent$action.value$Q
# matrix(getStateValues(agent$action.value$Q), ncol = 7)

# fixme: bug: action values go to negative infinity!!!

#agent$history

# lapply(agent$history, "[[", "state")
# purrr::transpose(agent$history)

# interact(env, agent, n.episodes = 10L)
#
# # to reset the counters we have to create a new copy of the environment
# env = WindyGridworld$new()
# interact(env, agent, n.episodes = 10L, max.steps.per.episode = 20L)
#

# # example train every 20 steps
# for (i in 1:500) {
#   interact(env, agent, n.steps = 20L, learn = FALSE)
#   agent$train()
# }


