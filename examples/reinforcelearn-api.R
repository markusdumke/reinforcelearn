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

#' State preprocessing

#' ActionValueTable
action.vals = ActionValueTable$new(n.states = env$n.states, n.actions = env$n.actions)

#' Policy
policy = EpsilonGreedyPolicy$new(epsilon = 0.1)

#' Algorithm
algorithm = QLearning$new()

#' Experience Replay
replay = ExperienceReplay$new(size = 10L)

#' Agent
agent = Agent$new(algorithm, action.vals, policy, replay)

#' Interaction
interact(env, agent, n.steps = 2000L)
interact(env, agent, n.episodes = 10L)

# to reset the counters we have to create a new copy of the environment
env = WindyGridworld$new()
interact(env, agent, n.episodes = 10L, max.steps.per.episode = 20L)
