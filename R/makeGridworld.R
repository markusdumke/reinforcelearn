#' Make Gridworld Environment
#' 
#' \code{makeGridworld} is used to create gridworld environments, 
#' which can be used as reinforcement learning problems.
#'
#' @param shape [\code{integer(2)}] \cr 
#'   Shape of the gridworld (number of rows x number of columns).
#' @param goal.states [\code{integer}] \cr 
#'   Goal states in the gridworld.
#' @param cliff.states [\code{integer}] \cr 
#'   Cliff states in the gridworld.
#' @param reward.step [\code{integer(1)}] \cr 
#'   Reward for taking a step.
#' @param cliff.transition.states [\code{integer}] \cr 
#'   States to which the environment transitions if stepping into the cliff. 
#'   If it is a vector, all states will have equal probability.
#' @param reward.cliff [\code{integer(1)}] \cr 
#'   Reward for taking a step in the cliff state.
#' @param wind [\code{integer}] \cr 
#'   Strength of the upward wind in each cell.
#' @return [\code{R6 class}] \cr
#'   Returns the gridworld environment, which is an R6 class. Most importantly it contains as 
#'   attributes the state transition array and the reward matrix, which can then be passed on to 
#'   \code{makeEnvironment} to create a full reinforcement learning environment.
#' @details 
#' In a gridworld the episodic task is to get from a start state to a goal state. 
#' The grid cells are the states.
#' 
#' Possible actions include going left (action 0), right (action 1), up (action 2) or down (action 3). 
#' If an action would take you off the grid, you remain in the previous state. For each step you
#' get a reward of \code{reward.step}, until you reach a goal state, then the episode is done.
#'  
#' When stepping into a cliff state you get a reward of \code{reward.cliff}, 
#' usually a high negative reward and transition to a state \code{cliff.transition.states}. 
#' 
#' In each column a deterministic wind specified via \code{wind} pushes you up a specific number of 
#' grid cells (for the next action).
#' 
#' The states are enumerated row-wise  and numeration starts with 0. 
#' Here is an example 4x4 grid:
#' \tabular{rrrr}{
#'  0 \tab 1 \tab 2 \tab 3 \cr
#'  4 \tab 5 \tab 6 \tab 7 \cr
#'  8 \tab 9 \tab 10 \tab 11 \cr
#'  12 \tab 13 \tab 14 \tab 15 \cr
#' }
#' So a board position could look like this (G: goal state, x: current state, C: cliff state):
#' \tabular{rrrr}{
#'  G \tab o \tab o \tab o \cr
#'  o \tab o \tab o \tab o \cr
#'  o \tab x \tab o \tab o \cr
#'  o \tab o \tab o \tab C \cr
#' }
#' 
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Gridworld Environment (Sutton & Barto Example 4.1)
#' grid = makeGridworld()
#' grid = makeEnvironment(transition.array = grid$transition.array, 
#'   reward.matrix = grid$reward.matrix)
#'   
#' # Windy Gridworld (Sutton & Barto Example 6.5) 
#' windygrid = makeGridworld(shape = c(7, 10), goal.states = 37, 
#'   reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0))
#' windygrid = makeEnvironment(transition.array = windygrid$transition.array, 
#'   reward.matrix = windygrid$reward.matrix, initial.state = 30)
#'   
#' # Cliff Walking (Sutton & Barto Example 6.6)   
#' cliff = makeGridworld(shape = c(4, 12), goal.states = 47, cliff.states = 37:46, 
#'   reward.step = - 1, reward.cliff = - 100, cliff.transition.states = 36)
#' cliff = makeEnvironment(transition.array = cliff$transition.array, 
#'   reward.matrix = cliff$reward.matrix, initial.state = 36) 
#'   
#' @seealso \code{\link{makeEnvironment}}
#' 
makeGridworld = function(shape = c(4, 4), goal.states = c(0, 15), cliff.states = NULL,
  reward.step = - 1, reward.cliff = - 100, wind = rep(0, shape[2]), cliff.transition.states = NULL) {
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
      transition.array = NULL, 
      reward.matrix = NULL,
      cliff.states = NULL,
      goal.states = NULL,
      wind = NULL,
      cliff.transition.states = NULL,
      
      initialize = function(shape, goal.states, cliff.states, reward.step, 
        reward.cliff, wind, cliff.transition.states) {
        if (any(goal.states > prod(shape) | cliff.states > prod(shape) | cliff.transition.states > prod(shape))) {
          stop("All states must be inside the grid! States are numerated starting with 0, check Details!")
        }
        checkmate::checkCount(shape, positive = TRUE)
        checkmate::checkCount(goal.states)
        checkmate::checkCount(cliff.states)
        checkmate::checkCount(wind)
        checkmate::checkCount(cliff.transition.states)
        stopifnot(length(wind) == shape[2])
        checkmate::checkNumber(reward.step)
        checkmate::checkNumber(reward.cliff)
        self$state.space = "Discrete"
        self$action.space = "Discrete"
        self$shape = shape
        self$wind = wind
        self$terminal.states = goal.states
        self$cliff.states = cliff.states
        self$goal.states = goal.states
        self$cliff.transition.states = cliff.transition.states
        self$actions = 0:3
        self$n.states = prod(shape)
        self$n.actions = length(self$actions)
        self$states = seq_len(self$n.states) - 1
        self$non.terminal.states = setdiff(self$states, self$terminal.states)
        
        private$computeBorderStates()
        private$makeTransitionArray()
        private$makeRewardMatrix(reward.step, reward.cliff)
      }
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
        self$transition.array = array(matrix(0, nrow = self$n.states, 
          ncol = self$n.states),
          dim = c(self$n.states, self$n.states, self$n.actions))
        for(state in setdiff(self$non.terminal.states, self$cliff.states)) {
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
            column = state
            while (column > (self$shape[2] - 1)) {
              column = column - self$shape[2]
            }
            new.state = new.state - self$wind[column + 1] * self$shape[2]
            while (new.state < 0) {
              new.state = new.state + self$shape[2]
            }
            self$transition.array[state + 1, new.state + 1, action + 1] = 1
          }
        }
        for (state in self$goal.states) {
          new.state = state
          self$transition.array[state + 1, new.state + 1, ] = 1
        }
        for (state in self$cliff.states) {
          self$transition.array[state + 1, self$cliff.transition.states + 1, ] = 
            1 / length(self$cliff.transition.states)
        }
        invisible(self)
      },
      
      makeRewardMatrix = function(reward.step, reward.cliff) {
        self$reward.matrix = matrix(reward.step, nrow = self$n.states, ncol = self$n.actions)
        self$reward.matrix[self$cliff.states + 1, ] = reward.cliff
        self$reward.matrix[self$goal.states + 1, ] = 0
        invisible(self)
      }
    )
  )
  gridworld$new(shape, goal.states, cliff.states, reward.step, 
        reward.cliff, wind, cliff.transition.states)
}

# writeLines(". . . .\n. . x .\n. . . .\n. . . .") # visualize gridworld
