# File containing example environments

#' Make Gridworld Environment
#' 
#' Simple gridworld environment for reinforcement learning.
#'
#' @param shape [\code{integer(2)}] \cr 
#'   Shape of the gridworld
#' @param terminal.states [\code{integer}] \cr 
#'   Terminal states in the 
#'   gridworld, states are numerated starting with 0 with increasing 
#'   number from left to right
#' @return [\code{R6 class}] \cr
#'   Returns the gridworld environment.
#' @details The states are enumerated as follows (example 4x4 grid):
#' \tabular{rrrr}{
#'  0 \tab 1 \tab 2 \tab 3 \cr
#'  4 \tab 5 \tab 6 \tab 7 \cr
#'  8 \tab 9 \tab 10 \tab 11 \cr
#'  12 \tab 13 \tab 14 \tab 15 \cr
#' }
#' So a board position could look like this (T: terminal state, x: current state):
#' \tabular{rrrr}{
#'  T \tab o \tab o \tab o \cr
#'  o \tab o \tab o \tab o \cr
#'  o \tab x \tab o \tab o \cr
#'  o \tab o \tab o \tab T \cr
#' }
#' Possible actions include going left, right, down or up. If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' grid = makeGridworld()
#' grid = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#' @seealso \code{\link{makeEnvironment}}
#' 
makeGridworld = function(shape = c(4L, 4L), terminal.states = c(0L, 15L)) {
  gridworld = R6::R6Class("gridworld",
    public = list(
      state.space = NULL,
      states = NULL,
      n.states = NULL,
      action.space = NULL,
      actions = NULL,
      n.actions = NULL,
      shape = NULL,
      terminal.states = NULL,
      non.terminal.states = NULL,
      initial.state = NULL,
      transition.array = NULL, 
      reward.matrix = NULL,
      
      initialize = function(shape, terminal.states) {
        if (any(terminal.states > prod(shape))) {
          stop("Terminal states must be inside the grid!")
        }
        checkmate::checkCount(shape, positive = TRUE)
        checkmate::checkCount(terminal.states)
        self$state.space = "Discrete"
        self$action.space = "Discrete"
        self$shape = shape
        self$terminal.states = terminal.states
        self$actions = 0:3
        self$n.states = prod(shape)
        self$n.actions = length(self$actions)
        self$states = seq_len(self$n.states) - 1
        self$non.terminal.states = self$states[self$states != self$terminal.states]
        
        private$computeBorderStates()
        private$makeTransitionArray()
        private$makeRewardMatrix()
      }#,
      
      # step = function(state, action) {
      #   # increment counter
      #   self$n.steps = self$n.steps + 1
      #   
      #   # take action -> sample next state and reward
      #   self$next.state = sample(self$states, size = 1, 
      #     prob = self$transition.array[state, , action])
      #   self$reward = self$reward.matrix[state, action]
      #   
      #   # episode over if terminalState is reached
      #   if (self$next.state %in% self$terminal.states) {
      #     self$done = TRUE
      #   }
      #   invisible(self)
      # },
      # 
      #reset = function() {
      #   self$done = FALSE
      # }
    ),
    private = list(
      border.states.left = NULL,
      border.states.right = NULL,
      border.states.up = NULL,
      border.states.down = NULL,
      computeBorderStates = function() {
        private$border.states.left = seq(0, self$n.states - self$shape[2], self$shape[2])
        private$border.states.right = seq(self$shape[2] - 1, self$n.states - 1, self$shape[2])
        private$border.states.up = seq(0, self$shape[2] - 1)
        private$border.states.down = seq(self$n.states - self$shape[2], self$n.states - 1)
        invisible(self)
      },
      
      makeTransitionArray = function() {
        # make probability transition array for each action (3-dimensional array)
        self$transition.array = array(matrix(0, nrow = self$n.states, 
          ncol = self$n.states),
          dim = c(self$n.states, self$n.states, self$n.actions))
        # fill in probabilities: when action is taking you off the grid,
        # the new state will be the same as the old state
        for(state in self$non.terminal.states) {
          for(action in self$actions) {
            if(action == 0) { # left
              new.state = ifelse(state %in% private$border.states.left, state, state - 1)
            }
            if (action == 1) { # right
              new.state = ifelse(state %in% private$border.states.right, state, state + 1)
            }
            if (action == 2) { # up
              new.state = ifelse(state %in% private$border.states.up, state, state - self$shape[2])
            }
            if (action == 3) { # down
              new.state = ifelse(state %in% private$border.states.down, state, state + self$shape[2])
            }
            self$transition.array[state + 1, new.state + 1, action + 1] = 1
          }
        }
        for (state in self$terminal.states) {
          new.state = state
          self$transition.array[state + 1, new.state + 1, ] = 1
        }
        invisible(self)
      },
      
      makeRewardMatrix = function() {
        # reward matrix: matrix depending on actions and states
        # reward of - 1 for each step
        self$reward.matrix = matrix(- 1, nrow = self$n.states, 
          ncol = self$n.actions)
        
        # set rewards of terminal states to 0
        self$reward.matrix[self$terminal.states + 1, ] = 0
        invisible(self)
      }
    )
  )
  gridworld$new(shape, terminal.states)
}

#' Gridworld
#'
#' Simple gridworld environment for reinforcement learning.
#' Grid of shape 4x4 with two terminal states in the upper left and
#' bottom right of the grid. Episodes start in any of the nonterminal
#' states. The possible actions include going left, right, up and down.
#' @format [\code{list(2)}] \cr 
#'   List with the transition array and reward matrix.
#' @name gridworld
NULL
#' @export
