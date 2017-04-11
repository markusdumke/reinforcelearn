#' Make Reinforcement Learning Environment
#' 
#' This function creates an environment for reinforcement learning. 
#' You could either use an existing environment from OpenAI Gym or specify the 
#' transition array and reward matrix for a Markov Decision Process.
#' 
#' State and action space can be either "Discrete" or "Box". For the discrete 
#' case states and actions are numerated starting from 0.
#'
#' @param gym.envir.name scalar character, e.g. "FrozenLake-v0", 
#' see [OpenAI Gym](https://gym.openai.com/envs) for possible environments.
#' @param transition.array numerical matrix (n.states x n.states x n.actions) 
#'   for each action giving the probabilities for transitions from one state to 
#'   all other states
#' @param reward.matrix numerical matrix the rewards for transitions from one 
#'   state to another
#' @param terminal.states integer vector: terminal.states of MDP. Note that
#'   states are numerated starting with 0.
#' @param initial.state scalar integer or integer vector: the starting state. If
#'   a vector is given a starting state will be randomly sampled from this 
#'   vector when reset is called. Note that states are numerated starting with 
#'   0. If NULL all states are possible initial states.
#' @importFrom R6 R6Class
#' @importFrom MDPtoolbox mdp_check 
#' @seealso [OpenAI Gym](https://gym.openai.com/docs)
#' @return Reinforcement Learning Environment, an R6 class.
#' @section Methods: \describe{
#' \item{\code{envir$initialize()}}{Creates a new environment.} 
#' \item{\code{envir$step(action, render = TRUE)}}{
#' Takes a step in the environment given an action,
#'   returns the next state, reward and if the episode is finished (logical). 
#'   If a transition array and reward matrix are given, the next step will be 
#'   sampled from the MDP, else the step [gym::env_step] function will be called.
#'   If render = TRUE the environment will be rendered.} 
#' \item{\code{envir$reset()}}{Resets the
#'   \code{episode.over} flag of the environment and returns an initial state.
#'    Useful when starting a new episode.}
#' }
#' @export
#' @import gym
#' @examples
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python and Gym installed.
#' FrozenLake = makeEnvironment("FrozenLake-v0")
#' FrozenLake$reset()
#' FrozenLake$step(action = 0)
#' 
#' # Now we can start a new FrozenLake environment by running:
#' FrozenLake$initialize()
#' 
#' # Create the MountainCar environment which has a continuous state space.
#' MountainCar = makeEnvironment("MountainCar-v0")
#' }
#' 
#' # Create an environment from a transition array and reward matrix (here a simple gridworld).
#' grid = gridworld$new()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, terminal.states = grid$terminal.states)
#'   
#' # Create the WindyGridworld environment.
#' grid = WindyGridworld$new()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, 
#'   terminal.states = grid$terminal.states, 
#'   initial.state = 30)
makeEnvironment <- function(gym.envir.name = NULL,  
  transition.array = NULL, reward.matrix = NULL, 
  terminal.states = NULL, initial.state = NULL) {
  envir = R6::R6Class("envir",
    public = list(
      gym = NULL,
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
      state = NULL,
      reward = NULL,
      episode.over = FALSE,
      n.steps = 0,
      transition.array = NULL, 
      reward.matrix = NULL,
      terminal.states = NULL,
      initial.state = NULL,
      
      initialize = function(gym.envir.name = NULL,
        transition.array = NULL, reward.matrix = NULL, 
        terminal.states = NULL, initial.state = NULL) {
        if (is.null(gym.envir.name)) {
          gym.envir.name = self$name
        }
        
        if (is.null(transition.array)) {
          package.path = system.file(package = "reinforcelearn")
          path2pythonfile = paste0(package.path, "/gym_http_server.py")
          command <- "python"
          system2(command, args = path2pythonfile, stdout = NULL, wait = FALSE)
          self$gym = TRUE
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
          
        } else {
          mdp_check(transition.array, reward.matrix)
          self$gym = FALSE
          self$state.space = "Discrete"
          self$action.space = "Discrete"
          self$actions = seq_len(ncol(reward.matrix)) - 1
          self$n.states = nrow(reward.matrix)
          self$n.actions = ncol(reward.matrix)
          self$states = seq_len(self$n.states) - 1
          self$transition.array = transition.array
          self$reward.matrix = reward.matrix
          self$terminal.states = terminal.states # state numeration starts with 0
          if (is.null(initial.state)) {
            self$initial.state = self$states[self$states != self$terminal.states]
          } else {
            self$initial.state = initial.state
          }
        }
      },
      
      step = function(action, render = TRUE) {
        self$n.steps = self$n.steps + 1
        if (self$gym == TRUE) {
          res = env_step(self$client, self$instance_id, action, render)
          self$state = res$observation
          self$reward = res$reward
          self$episode.over = res$done
        } else {
          
          self$reward = self$reward.matrix[self$state + 1, action + 1]
          self$state = sample(self$states, size = 1, 
            prob = self$transition.array[self$state + 1, , action + 1])
          
          if (self$state %in% self$terminal.states) {
            self$episode.over = TRUE
          }
        }
        
        invisible(self)
      },
      
      reset = function() {
        if (self$gym == TRUE) {
          self$state = env_reset(self$client, self$instance_id)
        } else {
          self$state = ifelse(length(self$initial.state) > 1, 
            sample(self$initial.state, size = 1), self$initial.state)
        }
        self$episode.over = FALSE
        invisible(self)
      }
    )
  )
  
  envir$new(gym.envir.name, transition.array, 
    reward.matrix, terminal.states, initial.state)
}
