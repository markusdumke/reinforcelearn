#' Make Reinforcement Learning Environment
#' 
#' State and action space can be either "Discrete" or "Box".
#'
#' @param gym.envir.name scalar character, e.g. "FrozenLake-v0", 
#' see [OpenAI Gym](https://gym.openai.com/envs) for possible environments.
#'
#' @seealso [OpenAI Gym](https://gym.openai.com/docs)
#' @return Reinforcement Learning Environment, an R6 class.
#' @section Methods: \describe{
#' \item{\code{envir$initialize()}}{Creates a new environment.} 
#' \item{\code{envir$step(action, render = TRUE)}}{
#' Takes a step in the environment given an action,
#'   returns the next state, reward and if episode is finished. 
#'   If render = TRUE the environment will be rendered.} 
#' \item{\code{envir$reset()}}{Resets the
#'   \code{episode.over} flag of the environment and returns an initial state.
#'    Useful when starting a new episode.}
#' }
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure you have gym-http-api and python installed.
#' # Then start a server from command line by running: python gym_http_server.py
#' 
#' FrozenLake = makeEnvironment("FrozenLake-v0")
#' FrozenLake$reset()
#' FrozenLake$step(action = 0)
#' 
#' # Now we can start a new FrozenLake environment by running:
#' FrozenLake$initialize()
#' }
makeEnvironment <- function(gym.envir.name, max.steps.episode = 200, ...) {
  envir = R6::R6Class("envir",
    public = list(
      name = NULL,
      client = NULL,
      instance_id = NULL,
      state.space = NULL,
      state.space.bounds = NULL,
      state.shape = NULL,
      states = NULL,
      n.states = NULL,
      n.actions = NULL,
      actions = NULL,
      action.space = NULL,
      action.space.bounds = NULL,
      action.shape = NULL,
      next.state = NULL,
      initial.state = NULL,
      reward = NULL,
      episode.over = FALSE,
      n.steps = 0,
      max.steps.episode = NULL,
      
      transition.array = NULL, # better private?
      reward.matrix = NULL, # better private?
      
      initialize = function(gym.envir.name = NULL, max.steps.episode = 200, ...) {
        if (is.null(gym.envir.name)) {
          gym.envir.name = self$name
        }
        
        self$name = gym.envir.name
        remote_base = "http://127.0.0.1:5000"
        client = create_GymClient(remote_base)
        self$client = client
        
        env_id = gym.envir.name
        instance_id = env_create(client, env_id)
        self$instance_id = instance_id
        
        outdir = "/tmp/random-agent-results"
        env_monitor_start(client, instance_id, outdir, force = TRUE, resume = FALSE)
        
        action_space_info = env_action_space_info(client, instance_id)
        self$action.space = action_space_info$name
        
        if (action_space_info$name == "Discrete") {
          self$n.actions = action_space_info$n
          self$actions = seq(0, self$n.actions - 1)
        }
        
        if (action_space_info$name == "Box") {
          self$action.shape = action_space_info$shape[[1]]
          self$action.space.bounds = list()
          for (i in seq_len(self$action.shape)) {
            self$action.space.bounds = append(self$action.space.bounds, 
              list(c(action_space_info$low[[i]], action_space_info$high[[i]])))
          }
        }
        
        state_space_info = env_observation_space_info(client, instance_id)
        self$state.space = state_space_info$name
        
        if (state_space_info$name == "Discrete") {
          self$n.states = state_space_info$n
          self$states = seq(0, self$n.states - 1)
        }
        
        if (state_space_info$name == "Box") {
          self$state.shape = state_space_info$shape[[1]]
          self$state.space.bounds = list()
          for (i in seq_len(self$state.shape)) {
            self$state.space.bounds = append(self$state.space.bounds, 
              list(c(state_space_info$low[[i]], state_space_info$high[[i]])))
          }
        }
        
        self$max.steps.episode = max.steps.episode
        
      },
      
      step = function(action, render = TRUE) {
        self$n.steps = self$n.steps + 1
        
        res = env_step(self$client, self$instance_id, action, render)
        self$next.state = res$observation
        self$reward = res$reward
        self$episode.over = res$done
        
        invisible(self)
      },
      
      reset = function() {
        self$initial.state = env_reset(self$client, self$instance_id)
        self$episode.over = FALSE
        invisible(self)
      }
    )
  )
  
  envir$new(gym.envir.name, max.steps.episode = max.steps.episode, ...)
}
