#' Mountain Car environment
#' 
#' Classic mountain car control problem from the reinforcement learning literature. 
#' Goal is to drive a car up a steep hill. It can only be achieved by rolling 
#' backwards up the hill, builing up momentum and then going forwards.
#' The state space is two-dimensional (position and velocity), the action space 
#' is discrete (throttle forward +1, throttle backwards -1, zero throttle 0).
#' 
#' To use this you must have installed OpenAI Gym und gym-http-api. 
#' To start a session run python gym_http_server.py in your command line.
#' 
#' @section Methods: \describe{
#' \item{\code{mountainCar$new()}}{Creates a new \code{mountainCar} environment.} 
#' \item{\code{mountainCar$step(state, action, render = TRUE)}}{
#' Takes a step in the environment given a state and an action,
#'   returns the next state and reward. If render = TRUE the environment will be rendered.} 
#' \item{\code{mountainCar$setEpisodeOverFalse()}}{Resets the
#'   \code{episode.over} flag of the mountainCar environment. 
#'   Useful when starting a new episode.}
#' }
#'   
#' @docType class
#' @references Mountain Car example from Sutton & Barto, chapter 10
#' @importFrom R6 R6Class
#' @import gym
#' @export
#' 
mountainCar = R6::R6Class("mountainCar",
  public = list(
    # states = NULL,
    client = NULL,
    instance_id = NULL,
    position = NULL,
    velocity = NULL,
    velocity.bounds = NULL,
    position.bounds = NULL,
    state.space = NULL,
    state.nvar = NULL,
    n.actions = NULL,
    actions = NULL,
    action.space = NULL,
    next.state = NULL,
    initial.state = NULL,
    reward = NULL,
    episode.over = FALSE,
    n.steps = 0,
    state.space.bounds = NULL,
    
    initialize = function() {
      remote_base = "http://127.0.0.1:5000"
      client = create_GymClient(remote_base)
      self$client = client
      
      env_id = "MountainCar-v0"
      instance_id = env_create(client, env_id)
      self$instance_id = instance_id
      
      outdir = "/tmp/random-agent-results"
      env_monitor_start(client, instance_id, outdir, force = TRUE, resume = FALSE)
      
      action_space_info = env_action_space_info(client, instance_id)
      self$n.actions = action_space_info$n
      self$action.space = action_space_info$name
      self$actions = seq(0, self$n.actions - 1)
      observation_space_info = env_observation_space_info(client, instance_id)
      self$state.space = observation_space_info$name
      self$state.nvar = observation_space_info$shape
      self$state.space.bounds = list(position.bounds = c(observation_space_info$low[[1]], observation_space_info$high[[1]]), 
        velocity.bounds = c(observation_space_info$low[[2]], observation_space_info$high[[2]]))
      self$position.bounds = c(observation_space_info$low[[1]], observation_space_info$high[[1]])
      self$velocity.bounds = c(observation_space_info$low[[2]], observation_space_info$high[[2]])
      
    },
    
    step = function(action, render = TRUE) {
      self$n.steps = self$n.steps + 1
      
      # take action -> sample next state and reward
      res = env_step(self$client, self$instance_id, action, render)
      self$velocity = res$observation[[2]]
      self$position = res$observation[[1]]
      self$next.state = res$observation
      self$reward = res$reward
      self$episode.over = res$done
      
      invisible(self)
    },
    
    setEpisodeOverFalse = function() {
      self$initial.state = env_reset(self$client, self$instance_id)
      self$episode.over = FALSE
      invisible(self)
    }
  )
)

# # Tiles: Grid across the 2-dimensional state space: each observation
# #   will be encoded as a one-hot vector.
# make_feature_vector = function(observation, observation_space_info, n.grid = 10) {
#   position = observation[[1]]
#   velocity = observation[[2]]
# 
#   seq_position = seq(observation_space_info[[1]][1], observation_space_info[[1]][2],
#     length.out = n.grid + 1)
#   seq_velocity = seq(observation_space_info[[2]][1], observation_space_info[[2]][2],
#     length.out = n.grid + 1)
#   grid = matrix(0, nrow = n.grid, ncol = n.grid)
# 
#   grid[findInterval(position, seq_position), findInterval(velocity, seq_velocity)] = 1
#   c(grid)
# }
# 
# # Estimate Q: Given a state action pair, the function returns a scalar Q value.
# # If only a state is provided, it returns a vector of Q values for the different actions.
# # state is a feature vector
# estimate_Q = function(state, action = NULL, weights) {
#   if (is.null(action)) {
#     Q = state %*% weights
#   } else {
#     Q = weights[, action] %*% state
#   }
#   Q
# }
# 
# sample_epsilon_greedy_action <- function(Q, epsilon) {
# 
#   greedy_action = which.max(Q)
#   random_actions = seq(1, length(Q))
#   # non_greedy_actions = actions[actions != greedy_action]
#   action = sample(c(greedy_action, random_actions), size = 1,
#     prob = c(1 - epsilon, rep(epsilon / length(random_actions), length(random_actions))))
# 
#   action
# }
# 
# m = mountainCar$new()
# n.grid = 10
# alpha = 0.1
# episode_count = 20
# max_steps = 200
# reward = 0
# done = FALSE
# weights = matrix(runif(n.grid^2 * m$n.actions), nrow = n.grid^2, ncol = m$n.actions)
# # action = sample(seq_len(action_space_info$n) - 1, size = 1) # initial action, # python numeration starts with 0!
# discount.factor = 1
# Q = rep(0, 3)
# epsilon = 0.1
# 
# sarsa_approx(m)
# 
# for (i in 1:episode_count) {
#   m$setEpisodeOverFalse()
#   state = m$initial.state
#   action = sample_epsilon_greedy_action(Q, epsilon) - 1
#   for (j in 1:max_steps) {
#     m$step(action, render = TRUE)
# 
#     next.state = m$next.state
#     reward = m$reward
#     features.state = make_feature_vector(state, m$state.space.bounds, n.grid)
#     features.next.state = make_feature_vector(next.state, list(m$position.bounds, m$velocity.bounds), n.grid)
#     Q = estimate_Q(features.next.state, weights = weights)
#     next.action = sample_epsilon_greedy_action(Q, epsilon = epsilon) - 1
# 
#     # update weights using SARSA(0)
#     td.target =  reward + discount.factor * estimate_Q(features.next.state, next.action + 1, weights)
#     td.error = td.target - estimate_Q(features.state, action + 1, weights)
#     grad = td.error * features.state
#     weights[, action + 1] = weights[, action + 1] + alpha * grad
# 
#     action = next.action
#     state = next.state
#     if (m$episode.over) break
#   }
# }

