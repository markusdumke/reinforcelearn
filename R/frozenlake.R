#' Frozen Lake Environment
#' 
#' The agent controls the movement of a character in a grid world. Some tiles of
#' the grid are walkable, and others lead to the agent falling into the water.
#' Additionally, the movement direction of the agent is uncertain and only
#' partially depends on the chosen direction. The agent is rewarded for finding
#' a walkable path to a goal tile. See [https://gym.openai.com/envs/FrozenLake-v0]
#' for details.
#' 
#' To use this you must have installed OpenAI Gym und gym-http-api. To start a
#' session run python gym_http_server.py in your command line.
#' 
#' @section Methods: \describe{ \item{\code{FrozenLake$new()}}{Creates a new
#'   \code{FrozenLake} environment.} \item{\code{FrozenLake$step(state,
#'   action, render = TRUE)}}{ Takes a step in the environment given a state and
#'   an action, returns the next state and reward. If render = TRUE the
#'   environment will be rendered.} 
#'   \item{\code{FrozenLake$setEpisodeOverFalse()}}{Resets the 
#'   \code{episode.over} flag of the FrozenLake environment. Useful when
#'   starting a new episode.} }
#'   
#' @docType class
#' @importFrom R6 R6Class
#' @import gym
#' @export
#' 
FrozenLake = R6::R6Class("FrozenLake",
  public = list(
    states = NULL,
    n.states = NULL,
    client = NULL,
    instance_id = NULL,
    state.space = NULL,
    n.actions = NULL,
    actions = NULL,
    action.space = NULL,
    next.state = NULL,
    initial.state = NULL,
    reward = NULL,
    episode.over = FALSE,
    n.steps = 0,
    
    initialize = function() {
      remote_base = "http://127.0.0.1:5000"
      client = create_GymClient(remote_base)
      self$client = client
      
      env_id = "FrozenLake-v0"
      instance_id = env_create(client, env_id)
      self$instance_id = instance_id
      
      outdir = "/tmp/random-agent-results"
      env_monitor_start(client, instance_id, outdir, force = TRUE, resume = FALSE)
      
      action_space_info = env_action_space_info(client, instance_id)
      self$n.actions = action_space_info$n
      self$action.space = action_space_info$name
      self$actions = seq_len(self$n.actions) - 1
      observation_space_info = env_observation_space_info(client, instance_id)
      self$state.space = observation_space_info$name
      self$n.states = observation_space_info$n
      self$states = seq_len(self$n.states) - 1
    },
    
    step = function(state, action, render = TRUE) {
      self$n.steps = self$n.steps + 1
      
      # take action -> sample next state and reward
      res = env_step(self$client, self$instance_id, action, render)

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
