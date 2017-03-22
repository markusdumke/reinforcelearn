#' Gridworld environment
#' 
#' Simple gridworld for reinforcement learning. With the step method given a 
#' state and an action in a gridworld, the next state and reward are returned.
#' 
#' @section Methods: \describe{ \item{\code{gridworld$new(shape, 
#'   terminal.states, initial.state)}}{Creates a new \code{gridworld} with a
#'   specific \code{shape}, which is a length-two integer, e.g. \code{c(4, 4)}. 
#'   \code{terminal.states} is an integer vector of the terminal states in the 
#'   gridworld. Default is \code{c(1, 16)}. initial.state sppecifies the 
#'   initial.states, an integer vector.} }
#'   
#' @details The states are enumerated as follows (example 4x4 grid):
#' \tabular{rrrr}{
#'  1 \tab 2 \tab 3 \tab 4 \cr
#'  5 \tab 6 \tab 7 \tab 8 \cr
#'  9 \tab 10 \tab 11 \tab 12 \cr
#'  13 \tab 14 \tab 15 \tab 16 \cr
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
#' @docType class
#' @references Gridworld example from Sutton & Barto, chapter 4
#' @usage #grid = gridworld$new()
#' @export
#' @importFrom R6 R6Class
#' @examples
#' grid = gridworld$new(shape = c(4, 4), terminal.states = c(0, 15), 
#'   initial.state = 1:14)
#' @seealso [makeEnvironment]
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
    
    initialize = function(shape = c(4, 4), terminal.states = c(0, 15), 
      initial.state = 1:14) {
      if (any(terminal.states > prod(shape))) {
        stop("Terminal states must be inside the grid!")
      }
      check_count(terminal.states, positive = TRUE)
      check_count(initial.state, positive = TRUE)
      
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$initial.state = initial.state
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
    #     self$episode.over = TRUE
    #   }
    #   invisible(self)
    # },
    # 
    #reset = function() {
    #   self$episode.over = FALSE
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
        dim = c(self$n.states, self$n.states, self$n.actions),
        dimnames = list(NULL, NULL, self$actions))
      # fill in probabilities: when action is taking you off the grid,
      # the new state will be the same as the old state
      for(state in self$states) {
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
      invisible(self)
    },
    
    makeRewardMatrix = function() {
      # reward matrix: matrix depending on actions and states
      # reward of - 1 for each step
      self$reward.matrix = matrix(- 1, nrow = self$n.states, 
        ncol = self$n.actions, dimnames = list(NULL, self$actions))
      
      # set rewards of terminal states to 0
      self$reward.matrix[self$terminal.states + 1, ] = 0
      invisible(self)
    }
  )
)
