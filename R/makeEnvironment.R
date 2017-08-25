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
#' @param transition.array [\code{array (n.states x n.states x n.actions)}] \cr
#'   Transition array: For each action specifying the probabilities for 
#'   transitions between states.
#' @param reward.matrix [\code{matrix (n.states x n.actions)}] \cr 
#'   Reward matrix: The reward for taking action a in state s.
#' @param initial.state [\code{integer}] \cr
#'   The starting state. If a vector is given a starting state will be
#'   randomly sampled from this vector when \code{reset} is called. 
#'   Note that states are numerated starting with 
#'   0. If \code{NULL} all states are possible initial states.
#' @param render [\code{logical(1)}] \cr 
#'   Whether to render the environment. If \code{TRUE} a python window 
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
#' # Create an environment from a transition array and reward matrix (here a simple gridworld).
#' grid = makeEnvironment(transition.array = gridworld$transitions,
#'   reward.matrix = gridworld$rewards)
#'   
#' # Create the WindyGridworld environment from transition array and reward matrix.
#' grid = makeEnvironment(transition.array = windyGridworld$transitions,
#'   reward.matrix = windyGridworld$rewards,
#'   initial.state = 30L)
#'   
makeEnvironment = function(gym.envir.name = NULL,  
  transition.array = NULL, reward.matrix = NULL, initial.state = NULL, 
  render = TRUE) {
  envir = R6::R6Class("envir",
    public = list(
      action.shape = NULL,
      action.space = NULL,
      action.space.bounds = NULL,
      actions = NULL,
      done = FALSE,
      gym = NULL,
      initial.state = NULL,
      n.actions = NULL,
      n.states = NULL,
      n.steps = NULL,
      previous.state = NULL,
      render = NULL,
      reward = NULL,
      reward.matrix = NULL,
      state = NULL,
      state.shape = NULL,
      state.space = NULL,
      state.space.bounds = NULL,
      states = NULL,
      terminal.states = NULL,
      transition.array = NULL, 
      
      initialize = function(gym.envir.name, transition.array, 
        reward.matrix, initial.state, render) {
        if (!is.null(gym.envir.name)) {
          if (!requireNamespace("gym", quietly = TRUE)) {
            stop("Please install the gym package to use gym environments. Also make sure you have the prerequisites installed: https://github.com/openai/gym-http-api",
              call. = FALSE)
          }
          checkmate::assertCharacter(gym.envir.name)
          checkmate::assertFlag(render)
          self$gym = TRUE
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
            self$actions = seq(0L, self$n.actions - 1L)
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
            self$states = seq(0L, self$n.states - 1L)
          }
          
          if (state.space.info$name == "Box") {
            self$state.shape = state.space.info$shape[[1]]
            self$state.space.bounds = list()
            for (i in seq_len(self$state.shape)) {
              self$state.space.bounds = append(self$state.space.bounds, 
                list(c(state.space.info$low[[i]], state.space.info$high[[i]])))
            }
          }
        } else {
          checkmate::assertMatrix(reward.matrix, any.missing = FALSE)
          checkmate::assertArray(transition.array, any.missing = FALSE, d = 3)
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
          terminal.states = apply(transition.array, 3, function(x) diag(x))
          self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1L
          
          if (is.null(initial.state)) {
            self$initial.state = self$states[self$states != self$terminal.states]
          } else {
            checkmate::assertIntegerish(initial.state)
            self$initial.state = initial.state
          }
        }
      },
      
      step = function(action, render = self$render) {
        self$n.steps = self$n.steps + 1L
        self$previous.state = self$state
        if (self$gym == TRUE) {
          res = gym::env_step(private$client, private$instance.id, action, render)
          self$state = res$observation
          self$reward = res$reward
          self$done = res$done
        } else {
          self$reward = self$reward.matrix[self$state + 1, action + 1]
          self$state = sample(self$states, size = 1, 
            prob = self$transition.array[self$state + 1, , action + 1])
          if (self$state %in% self$terminal.states) {
            self$done = TRUE
          }
        }
        invisible(self)
      },
      
      reset = function() {
        self$n.steps = 0
        if (self$gym == TRUE) {
          self$state = gym::env_reset(private$client, private$instance.id)
        } else {
          self$state = ifelse(length(self$initial.state) > 1L, 
            sample(self$initial.state, size = 1), self$initial.state)
        }
        self$done = FALSE
        invisible(self)
      },
      
      close = function() {
        if (self$gym == TRUE) {
          gym::env_close(private$client, private$instance.id)
        }
        invisible(self)
      }
    ),
    private = list(
      client = NULL,
      instance.id = NULL
    )
  )
  if (!is.null(gym.envir.name)) {
    package.path = system.file(package = "reinforcelearn")
    path2pythonfile = paste0(package.path, "/gym_http_server.py")
    command = "python"
    system2(command, args = path2pythonfile, stdout = NULL, wait = FALSE)
  }
  envir$new(gym.envir.name, transition.array, 
    reward.matrix, initial.state, render)
}

globalVariables("self")
globalVariables("private")
