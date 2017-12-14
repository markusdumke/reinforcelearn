#' MDP Environment
#'
#' Markov Decision Process environment.
#'
#' @section Usage:
#' \code{makeEnvironment("MDP", transitions, rewards, initial.state, visualize)}
#'
#' @param transitions [\code{array (n.states x n.states x n.actions)}] \cr
#'   State transition array.
#' @param rewards [\code{matrix (n.states x n.actions)}] \cr
#'   Reward array.
#' @param initial.state [\code{integer}] \cr
#'   Optional starting state.
#'   If a vector is given a starting state will be
#'   randomly sampled from this vector whenever \code{reset} is called.
#'   Note that states are numerated starting with
#'   0. If \code{initial.state = NULL} all non-terminal states are
#'   possible starting states.
#'
#' @inheritParams Environment
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
#' env = makeEnvironment("MDP", transitions = P, rewards = R)
#' env$reset()
#' env$step(1L)
NULL

MdpEnvironment = R6::R6Class("MdpEnvironment",
  inherit = Environment,

  public = list(
    action.space = NULL,
    actions = NULL,
    initial.state = NULL,
    n.actions = NULL,
    n.states = NULL,
    rewards = NULL,
    state.space = NULL,
    states = NULL,
    terminal.states = NULL,
    transitions = NULL,

    initialize = function(transitions, rewards, initial.state, visualize) {
      checkmate::assertArray(transitions, any.missing = FALSE, d = 3)
      checkmate::assertArray(rewards, any.missing = FALSE, min.d = 2, max.d = 2, null.ok = FALSE)
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$n.actions = dim(transitions)[3]
      self$n.states = dim(transitions)[1]
      self$actions = seq_len(self$n.actions) - 1L
      self$states = seq_len(self$n.states) - 1L
      self$transitions = transitions
      self$rewards = rewards
      terminal.states = apply(transitions, 3, function(x) diag(x))
      self$terminal.states = which(apply(terminal.states, 1, function(x) all(x == 1))) - 1L
      if (length(self$terminal.states) == 0) {
        warning("There are no terminal states in the MDP!")
        self$terminal.states = -1
      }
      if (missing(initial.state)) {
        self$initial.state = setdiff(self$states, self$terminal.states)
      } else {
        checkmate::assertIntegerish(initial.state, upper = self$n.states - 1)
        self$initial.state = initial.state
      }

      step_ = function(env, action) {
        reward = self$rewards[self$state + 1, action + 1] # use old state here!
        state = sample(self$states, size = 1,
          prob = self$transitions[self$state + 1, , action + 1])
        if (state %in% self$terminal.states) {
          done = TRUE
        } else {
          done = FALSE
        }
        list(state, reward, done)
      }

      reset_ = function() {
        state = ifelse(length(self$initial.state) > 1,
          sample(self$initial.state, size = 1), self$initial.state)
        state
      }
      # call initialize of superclass with mdp step and reset function
      super$initialize(step_, reset_, visualize)
    }
  )
)
