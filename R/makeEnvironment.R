#' Make Reinforcement Learning Environment
#' 
#' This function creates an environment for reinforcement learning. 
#' You could either use an existing environment from OpenAI Gym or 
#' specify the transition array and reward matrix of a 
#' Markov Decision Process.
#' 
#' State and action space can be either "Discrete" or "Box". In the 
#' discrete case states and actions are numerated starting from 0.
#'
#' @param gym.envir.name [\code{character(1)}] \cr
#'   Name of Gym environment, e.g. "CartPole-v0", have a look at 
#'   \url{https://gym.openai.com/envs}.
#' @param transitions [\code{array (n.states x n.states x n.actions)}] \cr
#'   Transition array: For each action specifying the probabilities for 
#'   transitions between states. Only used for MDPs.
#' @param rewards [\code{matrix (n.states x n.actions)} or \code{array (n.states x n.states x n.actions)}] \cr 
#'   Reward array: This can be a matrix (n.states x n.actions) or a 3-dimensional array 
#'   (n.states x n.states x n.actions). The reward will be sampled from the specified array 
#'   depending on state, action and possibly also the next state. 
#'   Only used for MDPs.
#' @param initial.state [\code{integer}] \cr
#'   The starting state if \code{reset} is \code{NULL} else this argument is unused.
#'   If a vector is given a starting state will be
#'   randomly sampled from this vector when \code{reset} is called. 
#'   Note that states are numerated starting with 
#'   0. If \code{initial.state = NULL} all states are possible initial states. Only used for MDPs.
#' @param reset [\code{function}] \cr 
#'   Function that returns an initial state observation, takes no arguments. Only used for MDPs.
#' @param sampleReward [\code{function}] \cr 
#'   Function that returns the next reward given the current state, action and next state. 
#'   Otherwise the reward will be sampled from the reward array of the MDP specified 
#'   by \code{rewards}. Only used for MDPs.
#' @param render [\code{logical(1)}] \cr 
#'   Whether to render the Gym environment. If \code{TRUE} a python window 
#'   with a graphical interface opens when steps are sampled in the 
#'   environment for a gym environment.
#' @importFrom R6 R6Class
#' @importFrom MDPtoolbox mdp_check 
#' @seealso \url{https://github.com/openai/gym-http-api}
#' @return [\code{R6 class}] \cr 
#'   Reinforcement Learning Environment
#' @section Methods: \describe{
#' \item{\code{envir$initialize()}}{Creates a new environment.} 
#' \item{\code{envir$step(action, render)}}{
#'   Takes a step in the environment given an action,
#'   returns the next state, reward and if the episode has finished. 
#'   If a transition array and reward matrix are given, the next step 
#'   will be sampled from the MDP, else the step 
#'   \code{\link[gym]{env_step}} function will be called.
#'   If \code{render = TRUE} a Gym environment will be rendered.} 
#' \item{\code{envir$reset()}}{Resets the
#'   \code{done} flag of the environment and returns an initial state.
#'    Useful when starting a new episode.}
#' \item{\code{envir$close()}}{Close the python window for a gym 
#'   environment.}   
#' }
#' @export
#' @examples
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python and Gym installed.
#' # Note: If makeEnvironment returns an error, this is a bug, please run the code again!
#' CartPole = makeEnvironment("CartPole-v0")
#' CartPole$reset()
#' CartPole$step(action = 0)
#' CartPole$close()
#' 
#' # Create the MountainCar environment which has a continuous state space.
#' MountainCar = makeEnvironment("MountainCar-v0")
#' 
#' MountainCar$state.space
#' MountainCar$state.space.bounds
#' }
#' 
#' # Create an environment from a transition array and reward matrix.
#' P = array(0, c(2,2,2))
#' P[, , 1] = matrix(c(0.5, 0.5, 0.8, 0.2), 2, 2, byrow = TRUE)
#' P[, , 2] = matrix(c(0, 1, 0.1, 0.9), 2, 2, byrow = TRUE)
#' R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)  
#' env = makeEnvironment(transitions = P, rewards = R)
#' 
#' # Specify a custom probability distribution for the starting state.
#' reset = function() {
#'   p = c(0.2, 0.8)
#'   sample(0:1, prob = p, size = 1)
#' }
#' env = makeEnvironment(transitions = P, rewards = R, reset = reset)
#' env$reset()
#' print(env$state)
#' 
#' # Specify a custom reward function.
#' sampleReward = function(state, action, n.state) {
#'   if (state == 2 & action == 1L) {
#'     rexp(1)
#'   } else {
#'     rnorm(1)
#'   }
#' }
#' env = makeEnvironment(transitions = P, rewards = R, sampleReward = sampleReward)
#' env$reset()
#' env$step(1)
#' print(env$reward)
#' 
#' # Gridworld Environment
#' grid = gridworld()
#'
makeEnvironment = function(gym.envir.name = NULL,  transitions = NULL, 
  rewards = NULL, initial.state = NULL, reset = NULL, sampleReward = NULL, render = TRUE) {
  
  checkmate::assertCharacter(gym.envir.name, max.len = 1, null.ok = TRUE)
  
  if (!is.null(gym.envir.name)) {
    package.path = system.file(package = "reinforcelearn")
    path2pythonfile = paste0(package.path, "/gym_http_server.py")
    command = "python"
    system2(command, args = path2pythonfile, stdout = NULL, wait = FALSE)
  }
  envir$new(gym.envir.name, transitions, 
    rewards, initial.state, reset, sampleReward, render)
}

envir = R6::R6Class("envir",
  public = list(
    action.shape = NULL,
    action.space = NULL,
    action.space.bounds = NULL,
    actions = NULL,
    done = FALSE,
    initial.state = NULL,
    n.actions = NULL,
    n.states = NULL,
    n.steps = NULL,
    previous.state = NULL,
    render = NULL,
    reward = NULL,
    rewards = NULL,
    state = NULL,
    state.shape = NULL,
    state.space = NULL,
    state.space.bounds = NULL,
    states = NULL,
    terminal.states = NULL,
    transitions = NULL, 
    
    reset = NULL,
    step = NULL,
    close = NULL,
    getReward = NULL,
    
    initialize = function(gym.envir.name, transitions, 
      rewards, initial.state, reset, sampleReward, render) {
      
      if (!is.null(gym.envir.name)) {
        self$initializeGym(gym.envir.name, render)
      } else {
        self$initializeMDP(transitions, rewards, initial.state, reset, sampleReward)
      }
    },
    
    initializeGym = function(gym.envir.name, render) {
      if (!requireNamespace("gym", quietly = TRUE)) {
        stop("Please install the gym package to use gym environments. 
          Also make sure you have the prerequisites installed: https://github.com/openai/gym-http-api",
          call. = FALSE)
      }
      checkmate::assertCharacter(gym.envir.name)
      checkmate::assertFlag(render)
      self$render = render
      remote.base = "http://127.0.0.1:5000"
      client = gym::create_GymClient(remote.base)
      private$client = client
      instance.id = gym::env_create(client, gym.envir.name)
      private$instance.id = instance.id
      
      outdir = "/tmp/random-agent-results"
      gym::env_monitor_start(client, instance.id, outdir, force = TRUE, resume = FALSE)
      action.space.info = gym::env_action_space_info(client, instance.id)
      self$action.space = action.space.info$name
      
      if (action.space.info$name == "Discrete") {
        self$n.actions = action.space.info$n
        self$actions = seq(0, self$n.actions - 1)
      }
      
      if (action.space.info$name == "Box") {
        self$action.shape = action.space.info$shape[[1]]
        self$action.space.bounds = list()
        for (i in seq_len(self$action.shape)) {
          self$action.space.bounds = append(self$action.space.bounds, 
            list(c(action.space.info$low[[i]], action.space.info$high[[i]])))
        }
      }
      
      state.space.info = gym::env_observation_space_info(client, instance.id)
      self$state.space = state.space.info$name
      
      if (state.space.info$name == "Discrete") {
        self$n.states = state.space.info$n
        self$states = seq(0, self$n.states - 1)
      }
      
      if (state.space.info$name == "Box") {
        self$state.shape = state.space.info$shape[[1]]
        self$state.space.bounds = list()
        for (i in seq_len(self$state.shape)) {
          self$state.space.bounds = append(self$state.space.bounds, 
            list(c(state.space.info$low[[i]], state.space.info$high[[i]])))
        }
      }
      
      self$step = function(action) {
        self$n.steps = self$n.steps + 1
        self$previous.state = self$state
        res = gym::env_step(private$client, private$instance.id, action, self$render)
        self$state = res$observation
        self$reward = res$reward
        self$done = res$done
        invisible(self)
      }
      
      self$reset = function() {
        self$n.steps = 0
        self$state = gym::env_reset(private$client, private$instance.id)
        self$previous.state = NULL
        self$done = FALSE
        invisible(self)
      }
      
      self$close = function() {
        gym::env_close(private$client, private$instance.id)
        invisible(self)
      }
    },
    
    initializeMDP = function(transitions, rewards, initial.state, reset, sampleReward) {
      checkmate::assertArray(transitions, any.missing = FALSE, d = 3)
      checkmate::assertArray(rewards, any.missing = FALSE, min.d = 2, max.d = 3, null.ok = TRUE)
      if (!is.null(rewards)) {
        MDPtoolbox::mdp_check(transitions, rewards)
      }
      checkmate::assertFunction(sampleReward, nargs = 3, null.ok = TRUE)
      checkmate::assertFunction(reset, nargs = 0, null.ok = TRUE)
      
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$n.actions = dim(transitions)[3]
      self$n.states = dim(transitions)[1]
      self$actions = seq_len(self$n.actions) - 1L
      self$states = seq_len(self$n.states) - 1L
      self$transitions = transitions
      self$rewards = rewards
      terminal.states = apply(transitions, 3, function(x) diag(x))
      self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1
      if (length(self$terminal.states) == 0) {
        warning("There are no terminal states in the MDP!")
        self$terminal.states = - 1
      }
      
      if (is.null(initial.state)) {
        self$initial.state = self$states[self$states != self$terminal.states]
      } else {
        checkmate::assertIntegerish(initial.state, upper = self$n.states - 1)
        self$initial.state = initial.state
      }
      
      if (is.null(sampleReward)) {
        if (length(dim(rewards)) == 2) {
          self$getReward = function(state, action, state.n) {
            self$rewards[state + 1, action + 1]
          }
        } else {
          self$getReward = function(state, action, state.n) {
            self$rewards[state + 1, state.n + 1, action + 1]
          }
        }
        
      } else {
        self$getReward = sampleReward
      }
      
      self$step = function(action) {
        self$n.steps = self$n.steps + 1L
        self$previous.state = self$state
        self$state = sample(self$states, size = 1, 
          prob = self$transitions[self$state + 1, , action + 1])
        self$reward = self$getReward(self$previous.state, action, self$state)
        if (self$state %in% self$terminal.states) {
          self$done = TRUE
        }
        invisible(self)
      }
      
      if (is.null(reset)) {
        self$reset = function() {
          self$n.steps = 0
          self$state = ifelse(length(self$initial.state) > 1, 
            sample(self$initial.state, size = 1), self$initial.state)
          self$previous.state = NULL
          self$done = FALSE
          invisible(self)
        }
      } else {
        checkmate::assertFunction(reset, nargs = 0)
        self$reset = function() {
          self$n.steps = 0L
          self$state = reset()
          self$previous.state = NULL
          self$done = FALSE
          invisible(self)
        }
      }
      
      self$close = function() {
        invisible(self)
      }
    }
  ),
  private = list(
    client = NULL,
    instance.id = NULL
  )
)

globalVariables("self")
globalVariables("private")
