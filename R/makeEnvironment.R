#' Make Reinforcement Learning Environment
#' 
#' This function creates an environment for reinforcement learning. 
#' You could either use an existing environment from OpenAI Gym or specify the 
#' transition array and reward matrix for a Markov Decision Process.
#' 
#' State and action space can be either "Discrete" or "Box". For the discrete 
#' case states and actions are numerated starting from 0.
#'
#' @param gym.envir.name scalar character, e.g. "CartPole-v0", 
#' see [OpenAI Gym](https://gym.openai.com/envs) for possible environments.
#' @param transition.array numerical matrix (n.states x n.states x n.actions) 
#'   for each action giving the probabilities for transitions from one state to 
#'   all other states
#' @param reward.matrix numerical matrix the rewards for transitions from one 
#'   state to another
#' @param initial.state scalar integer or integer vector: the starting state. If
#'   a vector is given a starting state will be randomly sampled from this 
#'   vector when reset is called. Note that states are numerated starting with 
#'   0. If NULL all states are possible initial states.
#' @param render logical scalar: whether to render the environment
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
#' \item{\code{envir$close()}}{Close the python window for a gym environment.}   
#' }
#' @export
#' @import gym
#' @examples
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python and Gym installed.
#' CartPole = makeEnvironment("CartPole-v0")
#' CartPole$reset()
#' CartPole$step(action = 0)
#' CartPole$close()
#' 
#' # Create the MountainCar environment which has a continuous state space.
#' MountainCar = makeEnvironment("MountainCar-v0")
#' }
#' 
#' # Create an environment from a transition array and reward matrix (here a simple gridworld).
#' grid = makeGridworld()
#' Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#'   
#' # Create the WindyGridworld environment.
#' grid = makeWindyGridworld()
#' WindyGridworld1 = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix, initial.state = 30L)
#'   
makeEnvironment <- function(gym.envir.name = NULL,  
  transition.array = NULL, reward.matrix = NULL, initial.state = NULL, 
  render = TRUE) {
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
      n.steps = 0L,
      transition.array = NULL, 
      reward.matrix = NULL,
      terminal.states = NULL,
      initial.state = NULL,
      render = NULL,
      
      initialize = function(gym.envir.name = NULL,
        transition.array = NULL, reward.matrix = NULL, 
        initial.state = NULL, render = TRUE) {
        
        self$render = render
        if (is.null(gym.envir.name)) {
          gym.envir.name = self$name
        }
        
        if (is.null(transition.array)) {
          assert_character(gym.envir.name)
          assert_logical(render)
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
          instance_id = env_create(client, env_id) # error
          self$instance_id = instance_id
          
          outdir = "/tmp/random-agent-results"
          env_monitor_start(client, instance_id, outdir, force = TRUE, resume = FALSE)
          
          action_space_info = env_action_space_info(client, instance_id)
          self$action.space = action_space_info$name
          
          if (action_space_info$name == "Discrete") {
            self$n.actions = action_space_info$n
            self$actions = seq(0L, self$n.actions - 1L)
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
            self$states = seq(0L, self$n.states - 1L)
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
          assert_matrix(reward.matrix, any.missing = FALSE)
          assert_array(transition.array, any.missing = FALSE, d = 3)
          MDPtoolbox::mdp_check(transition.array, reward.matrix)
          self$gym = FALSE
          self$state.space = "Discrete"
          self$action.space = "Discrete"
          self$actions = seq_len(ncol(reward.matrix)) - 1L
          self$n.states = nrow(reward.matrix)
          self$n.actions = ncol(reward.matrix)
          self$states = seq_len(self$n.states) - 1L
          self$transition.array = transition.array
          self$reward.matrix = reward.matrix
          # get terminal states from transition array
          terminal.states = apply(transition.array, 3, function(x) diag(x))
          self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1L
          
          # state numeration starts with 0
          if (is.null(initial.state)) {
            self$initial.state = self$states[self$states != self$terminal.states]
          } else {
            assert_integer(initial.state)
            self$initial.state = initial.state
          }
        }
      },
      
      step = function(action, render = self$render) {
        self$n.steps = self$n.steps + 1L
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
          self$state = ifelse(length(self$initial.state) > 1L, 
            sample(self$initial.state, size = 1), self$initial.state)
        }
        self$episode.over = FALSE
        invisible(self)
      },
      
      close = function() {
        if (self$gym == TRUE) {
          gym::env_close(self$client, self$instance_id)
        }
        invisible(self)
      }
    )
  )
  
  envir$new(gym.envir.name, transition.array, 
    reward.matrix, initial.state, render)
}
