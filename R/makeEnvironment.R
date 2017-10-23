#' Make Reinforcement Learning Environment
#'
#' This function creates an environment for reinforcement learning.
#' You could either use an environment from OpenAI Gym or
#' specify the transition and reward array of a
#' Markov Decision Process.
#'
#' This function returns an R6 class with a \code{reset} and \code{step} method.
#' Everytime when you start an episode call the \code{reset} method first.
#' Use then the \code{step} method to interact with the environment.
#' Note that the methods do not return anything, but change the attributes of the R6 class,
#' most importantly the \code{state}, \code{reward} and \code{done} attribute.
#'
#' Note that all states and actions are numerated starting with 0!
#'
#' For OpenAI gym environments have a look at \url{https://gym.openai.com/envs}
#' and at the description of what requirements to install
#' at \url{https://github.com/openai/gym-http-api}.
#'
#' For a detailed explanation have a look at the vignette "How to create an environment?".
#'
#' @param gym [\code{character(1)}] \cr
#'   Name of gym environment, e.g. \code{"CartPole-v0"}
#' @param transitions [\code{array (n.states x n.states x n.actions)}] \cr
#'   Transition array of Markov Decision Process: For each action specifying the probabilities for
#'   transitions between states.
#' @param rewards [\code{matrix (n.states x n.actions)} or \code{array (n.states x n.states x n.actions)}] \cr
#'   Reward array: This can be a matrix (n.states x n.actions) or a 3-dimensional array
#'   (n.states x n.states x n.actions). The reward will be sampled from the specified array
#'   depending on state, action and possibly also the next state.
#' @param initial.state [\code{integer}] \cr
#'   The starting state if \code{reset} is \code{NULL} else this argument is unused.
#'   If a vector is given a starting state will be
#'   randomly sampled from this vector when \code{reset} is called.
#'   Note that states are numerated starting with
#'   0. If \code{initial.state = NULL} all states are possible initial states.
#' @param reset [\code{function}] \cr
#'   Function that returns an initial state observation, takes no arguments.
#' @param sampleReward [\code{function}] \cr
#'   Function that returns the next reward given the current state, action and next state.
#'   Otherwise the reward will be sampled from the reward array of the MDP specified
#'   by \code{rewards}.
#' @param render [\code{logical(1)}] \cr
#'   Whether to render the OpenAI Gym environment. If \code{TRUE} a python window
#'   with a graphical interface opens whenever the step method is called.
#' @seealso Create gridworlds with \code{\link{makeGridworld}}.
#' For the mountain car environment have a look at \code{\link{mountainCar}}.
#' @return [\code{R6 class}] \cr
#'   Reinforcement Learning Environment.
#' @section Methods: \describe{
#' \item{\code{envir$step(action)}}{
#'   Takes a step in the environment. Given an action the method
#'   returns the next state, reward and if the episode has finished.
#'   If a transition array and reward matrix are given, the next step
#'   will be sampled from the MDP, else \code{\link[gym]{env_step}} will be called.
#'  }
#' \item{\code{envir$reset()}}{
#'   Resets the \code{done} flag of the environment and returns an initial state.
#'   Useful when starting a new episode.
#' }
#' \item{\code{envir$close()}}{
#'   Close the python window for an OpenAI Gym environment.}
#' }
#' @export
#' @examples
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python and Gym installed.
#' # Start server.
#' package.path = system.file(package = "reinforcelearn")
#' path2pythonfile = paste0(package.path, "/gym_http_server.py")
#' system2("python", args = path2pythonfile, stdout = NULL,
#'   wait = FALSE, invisible = FALSE)
#'
#' env = makeEnvironment("CartPole-v0", render = FALSE)
#' env$reset()
#' env$step(action = 0)
#' env$close()
#' print(env)
#'
#' # Create the MountainCar environment which has a continuous state space.
#' env = makeEnvironment("MountainCar-v0")
#' env$state.space
#' env$state.space.bounds
#'
#' # Take random actions for 200 steps.
#' env$reset()
#' for (i in 1:200) {
#'   action = sample(env$actions, 1)
#'   env$step(action)
#' }
#' }
#'
#' # Create an environment from a transition array and reward matrix.
#' P = array(0, c(2,2,2))
#' P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
#' P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
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
makeEnvironment = function(gym = NULL, transitions = NULL, rewards = NULL,
  initial.state = NULL, reset = NULL, sampleReward = NULL, render = TRUE) {

  envir$new(gym, transitions, rewards, initial.state, reset, sampleReward, render)
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

    initialize = function(gym, transitions,
      rewards, initial.state, reset, sampleReward, render) {

      if (!is.null(gym)) {
        self$initializeGym(gym, render)
      } else {
        self$initializeMDP(transitions, rewards, initial.state, reset, sampleReward)
      }
    },

    initializeGym = function(gym, render) {
      if (!requireNamespace("gym", quietly = TRUE)) {
        stop("Please install the gym package to use environments from OpenAI Gym.
        Also make sure you have the requirements installed: https://github.com/openai/gym-http-api",
          call. = FALSE)
      }
      client = gym::create_GymClient("http://127.0.0.1:5000")
      instance.id = gym::env_create(client, gym)
      # checkmate::assertList(gym, len = 2)
      checkmate::assertFlag(render)
      self$render = render
      outdir = "/tmp/random-agent-results"
      private$client = client
      private$instance.id = instance.id
      gym::env_monitor_start(private$client, private$instance.id,
        outdir, force = TRUE, resume = FALSE)

      action.space.info = gym::env_action_space_info(private$client, private$instance.id)
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

      state.space.info = gym::env_observation_space_info(private$client, private$instance.id)
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
        self$initial.state = setdiff(self$states, self$terminal.states)
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
          self$previous.state = NULL
          self$done = FALSE
          self$state = reset()
          if (self$state %in% self$terminal.states) {
            warning("The starting state is a terminal state!")
            self$done = TRUE
          }
          invisible(self)
        }
      }

      self$close = function() {
        invisible(self)
      }
    },

    print = function() {
      printEnvir(self)
    }
  ),
  private = list(
    client = NULL,
    instance.id = NULL
  )
)

# Copied from R6 class
# Trim a string to n characters; if it's longer than n, add " ..." to the end
trim = function(str, n = 60) {
  if (nchar(str) > n) paste(substr(str, 1, n - 4), "...")
  else str
}

# Custom printing function for environment
# x is a list
printEnvir = function(x) {
  cat(paste("Number of steps:", x$n.steps, "\n"))
  cat(paste("State:", trim(paste(as.character(x$state), collapse = " ")), "\n"))
  cat(paste("Reward:", x$reward, "\n"))
  cat(paste("Done:", x$done, "\n"))
  invisible(x)
}

globalVariables("self")
globalVariables("private")
