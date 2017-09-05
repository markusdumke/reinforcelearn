#' Make Gridworld
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
#' @param diagonal.moves [\code{logical(1)}] \cr
#'   Should diagonal moves be allowed?
#' @param wind [\code{integer}] \cr 
#'   Strength of the upward wind in each cell.
#' @param stochasticity [\code{numeric(1)}] \cr
#'   Probability of random transition to any of the neighboring states when taking any action.
#' @return [\code{list(2)}] \cr
#'   Returns a list with the state transition array [\code{array(3)}] and reward matrix 
#'   [\code{matrix}] of the gridworld.
#'   These can then be passed on to \code{makeEnvironment} to create a full reinforcement 
#'   learning environment.
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
#' @seealso gridworld
#' @seealso windy.gridworld
#' @seealso cliff
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Gridworld Environment (Sutton & Barto Example 4.1)
#' gridworld = makeGridworld()
#' env = makeEnvironment(transitions = gridworld$transitions, 
#'   rewards = gridworld$rewards)
#'   
#' # Windy Gridworld (Sutton & Barto Example 6.5) 
#' windy.gridworld = makeGridworld(shape = c(7, 10), goal.states = 37, 
#'   reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0))
#' env = makeEnvironment(transitions = windy.gridworld$transitions, 
#'   rewards = windy.gridworld$rewards, initial.state = 30)
#'   
#' # Cliff Walking (Sutton & Barto Example 6.6)   
#' cliff = makeGridworld(shape = c(4, 12), goal.states = 47, cliff.states = 37:46, 
#'   reward.step = - 1, reward.cliff = - 100, cliff.transition.states = 36)
#' env = makeEnvironment(transitions = cliff$transitions, 
#'   rewards = cliff$rewards, initial.state = 36) 
#'   
#' @seealso \code{\link{makeEnvironment}}
#' 
makeGridworld = function(shape = c(4, 4), goal.states = c(0, 15), cliff.states = NULL,
  reward.step = - 1, reward.cliff = - 100, diagonal.moves = FALSE, wind = rep(0, shape[2]), 
  cliff.transition.states = NULL, stochasticity = 0) {
  
  g = gridworld$new(shape, goal.states, cliff.states, reward.step, 
    reward.cliff, diagonal.moves, wind, cliff.transition.states, stochasticity)
  list(transitions = g$transitions, rewards = g$rewards)
}

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
    transitions = NULL, 
    rewards = NULL,
    cliff.states = NULL,
    goal.states = NULL,
    wind = NULL,
    cliff.transition.states = NULL,
    stochasticity = NULL,
    diagonal.moves = NULL,
    
    initialize = function(shape, goal.states, cliff.states, reward.step, 
      reward.cliff, diagonal.moves, wind, cliff.transition.states, stochasticity) {
      if (any(goal.states > prod(shape) | cliff.states > prod(shape) | cliff.transition.states > prod(shape))) {
        stop("All states must be inside the grid! States are numerated starting with 0, check Details!")
      }
      checkmate::assertIntegerish(shape, len = 2)
      checkmate::assertIntegerish(goal.states)
      checkmate::assertIntegerish(cliff.states, null.ok = TRUE)
      checkmate::assertIntegerish(wind, len = shape[2])
      checkmate::assertIntegerish(cliff.transition.states, null.ok = TRUE)
      stopifnot(length(wind) == shape[2])
      checkmate::assertNumber(reward.step)
      checkmate::assertNumber(reward.cliff)
      checkmate::assertFlag(diagonal.moves)
      checkmate::assertNumber(stochasticity, lower = 0, upper = 1)
      self$state.space = "Discrete"
      self$action.space = "Discrete"
      self$shape = shape
      self$wind = wind
      self$terminal.states = goal.states
      self$cliff.states = cliff.states
      self$goal.states = goal.states
      self$cliff.transition.states = cliff.transition.states
      if (diagonal.moves) {
        self$actions = 0:7
      } else {
        self$actions = 0:3
      }
      self$n.states = prod(shape)
      self$n.actions = length(self$actions)
      self$states = seq_len(self$n.states) - 1
      self$non.terminal.states = setdiff(self$states, self$terminal.states)
      self$stochasticity = stochasticity
      self$diagonal.moves = diagonal.moves
      
      self$computeBorderStates()
      self$makeTransitionArray()
      self$makeRewardMatrix(reward.step, reward.cliff)
    },
    
    border.states.left = NULL,
    border.states.right = NULL,
    border.states.up = NULL,
    border.states.down = NULL,
    
    computeBorderStates = function() {
      self$border.states.left = seq(0, self$n.states - self$shape[2], self$shape[2])
      self$border.states.right = seq(self$shape[2] - 1, self$n.states - 1, self$shape[2])
      self$border.states.up = seq(0, self$shape[2] - 1)
      self$border.states.down = seq(self$n.states - self$shape[2], self$n.states - 1)
      invisible(self)
    },
    
    goLeft = function(state) {
      state - 1
    },
    
    goRight = function(state) {
      state + 1
    },
    
    goUp = function(state) {
      state - self$shape[2]
    },
    
    goDown = function(state) {
      state + self$shape[2]
    },
    
    isBorderState = function(state, border.states) {
      state %in% border.states
    },
    
    makeTransitionArray = function() {
      self$transitions = array(matrix(0, nrow = self$n.states, ncol = self$n.states),
        dim = c(self$n.states, self$n.states, self$n.actions))
      
      for(state in setdiff(self$non.terminal.states, self$cliff.states)) {
        
        left.state = ifelse(self$isBorderState(state, self$border.states.left), 
          state, self$goLeft(state))
        right.state = ifelse(self$isBorderState(state, self$border.states.right), 
          state, self$goRight(state))
        up.state = ifelse(self$isBorderState(state, self$border.states.up), state, 
          self$goUp(state))
        down.state = ifelse(self$isBorderState(state, self$border.states.down), state, 
          self$goDown(state))
        left.up.state = ifelse(self$isBorderState(state, c(self$border.states.left, 
          self$border.states.up)), state, self$goLeft(self$goUp(state)))
        left.down.state = ifelse(self$isBorderState(state, c(self$border.states.left, 
          self$border.states.down)), state, self$goLeft(self$goDown(state)))
        right.up.state = ifelse(self$isBorderState(state, c(self$border.states.right, 
          self$border.states.up)), state, self$goRight(self$goUp(state)))
        right.down.state = ifelse(self$isBorderState(state, c(self$border.states.right, 
          self$border.states.down)), state, self$goRight(self$goDown(state)))
        
        neighbor.states = c(left.state, right.state, up.state, down.state, 
          left.up.state, left.down.state, right.up.state, right.down.state)
        neighbor.states.unique = unique(neighbor.states)
        stochasticity.neighbors = self$stochasticity / 8 * table(neighbor.states)
        self$transitions[state + 1, neighbor.states.unique + 1, ] = stochasticity.neighbors
        
        if (state %in% self$cliff.states) {
          self$transitions[state + 1, self$cliff.transition.states + 1, ] = 
            1 / length(self$cliff.transition.states)
        } else {
          
          if (self$diagonal.moves) {
            new.state = neighbor.states
          } else {
            new.state = c(left.state, right.state, up.state, down.state)
          }
          
          column = state
          new.states = self$applyWind(column, new.state)
          new.states = sapply(new.states, self$getIntoBounds)
          for (i in seq_len(self$n.actions)) {
            self$transitions[state + 1, new.states[i] + 1, i] = self$transitions[state + 1, 
              new.states[i] + 1, i] + 1 - self$stochasticity
          }
        }
      }
      for (state in self$goal.states) {
        new.state = state
        self$transitions[state + 1, new.state + 1, ] = 1
      }
      invisible(self)
    },
    
    applyWind = function(column, new.state) {
      while (column > (self$shape[2] - 1)) {
        column = column - self$shape[2]
      }
      new.state = new.state - self$wind[column + 1] * self$shape[2]
      new.state
    },
    
    getIntoBounds = function(new.state) {
      while (new.state < 0) {
        new.state = new.state + self$shape[2]
      }
      new.state
    },
    
    makeRewardMatrix = function(reward.step, reward.cliff) {
      self$rewards = matrix(reward.step, nrow = self$n.states, ncol = self$n.actions)
      self$rewards[self$cliff.states + 1, ] = reward.cliff
      self$rewards[self$goal.states + 1, ] = 0
      invisible(self)
    }
  )
)

# writeLines(". . . .\n. . x .\n. . . .\n. . . .") # visualize gridworld
