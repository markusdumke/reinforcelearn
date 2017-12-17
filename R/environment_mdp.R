#' MDP Environment
#'
#' Markov Decision Process environment.
#'
#' @section Usage:
#' `makeEnvironment("MDP", transitions, rewards, initial.state, visualize)`
#'
#' @param transitions \[`array (n.states x n.states x n.actions)`] \cr
#'   State transition array.
#' @param rewards \[`matrix (n.states x n.actions)`] \cr
#'   Reward array.
#' @param initial.state \[`integer`] \cr
#'   Optional starting state.
#'   If a vector is given a starting state will be
#'   randomly sampled from this vector whenever `reset` is called.
#'   Note that states are numerated starting with
#'   0. If `initial.state = NULL` all non-terminal states are
#'   possible starting states.
#'
#' @md
#'
#' @name MdpEnvironment
#' @inheritSection Environment Methods
#' @export
#'
#' @examples
#' # Create a Markov Decision Process.
#' P = array(0, c(2, 2, 2))
#' P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
#' P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
#' R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
#' env = makeEnvironment("mdp", transitions = P, rewards = R)
#' env$reset()
#' env$step(1L)
NULL

MdpEnvironment = R6::R6Class("MdpEnvironment",
  inherit = Environment,

  public = list(
    action.space = NULL,
    actions = NULL,
    action.names = NULL,
    initial.state = NULL,
    n.actions = NULL,
    n.states = NULL,
    rewards = NULL,
    state.space = NULL,
    states = NULL,
    terminal.states = NULL,
    transitions = NULL,

    initialize = function(transitions, rewards, initial.state, action.names = NULL, ...) {
      checkmate::assertArray(transitions, any.missing = FALSE, d = 3L)
      checkmate::assertArray(rewards, any.missing = FALSE, d = 2L)
      #checkmate::assertCharacter(), assertNamed ...
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$n.actions = dim(transitions)[3]
      self$n.states = dim(transitions)[1]
      self$actions = seq_len(self$n.actions) - 1L
      self$action.names = action.names
      self$states = seq_len(self$n.states) - 1L
      self$transitions = transitions
      self$rewards = rewards
      terminal.states = apply(transitions, 3L, function(x) diag(x))
      self$terminal.states = which(apply(terminal.states, 1L, function(x) all(x == 1L))) - 1L
      if (length(self$terminal.states) == 0) {
        warning("There are no terminal states in the MDP!")
        self$terminal.states = -1L
      }
      if (missing(initial.state)) {
        self$initial.state = setdiff(self$states, self$terminal.states)
      } else {
        checkmate::assertIntegerish(initial.state, upper = self$n.states - 1L)
        self$initial.state = initial.state
      }

      step_ = function(env, action) {
        if (is.character(action)) {
          action = self$action.names[action]
        }
        reward = self$rewards[self$state + 1L, action + 1L] # use old state here!
        state = sample(self$states, size = 1L,
          prob = self$transitions[self$state + 1L, , action + 1L])
        if (state %in% self$terminal.states) {
          done = TRUE
        } else {
          done = FALSE
        }
        list(state, reward, done)
      }

      reset_ = function(env) {
        state = ifelse(length(self$initial.state) > 1L,
          sample(self$initial.state, size = 1L), self$initial.state)
        state
      }
      # call initialize of superclass with mdp step and reset function
      super$initialize(step_, reset_, ...)
    }
  )
)
