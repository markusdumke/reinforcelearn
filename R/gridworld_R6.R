#' Gridworld environment as R6 class
#'
#' Simple gridworld for reinforcement learning. With the step method given a state and an action in a gridworld,
#' the next state and reward are returned.
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
#' Possible actions include going left, right, down or up. If an action would take you off
#' the grid, you remain in the previous state. For each step you get a reward of -1, until you reach
#' into a terminal state.
#'
#' @references Gridworld example from Sutton & Barto, chapter 4
#' @param shape length-two integer vector: the shape of the grid, e.g. (4, 4)
#' @param terminal.states integer vector of terminal states
#' @return A ref class with method `step()`
#' @export
#' @importFrom R6 R6Class
#' @examples
#' set.seed(27)
#' grid = gridworld_R6$new(shape = c(4, 4), terminal.states = c(1, 16))
#'
#' # initial state = 3
#' states = 3
#' rewards = numeric(0)
#' sampled.actions = character(0)
#' episode.over = FALSE
#' i = 1
#'
#' while(grid$episode.over == FALSE) {
#'   sampled.actions = append(sampled.actions, sample(grid$actions, size = 1))
#'   grid$step(states[i], sampled.actions[i])
#'   states = append(states, grid$next.state)
#'   rewards = append(rewards, grid$reward)
#'   episode.over = grid$episode.over
#'   i = i + 1
#' }
#'
#' print(rewards)
#' print(states)
gridworld_R6 = R6::R6Class("gridworld_R6",
  public = list(
    shape = NULL,
    terminal.states = NULL,
    actions = NULL,
    n.states = NULL,
    n.actions = NULL,
    transition.array = NULL,
    reward.matrix = NULL,
    episode.over = NULL,
    next.state = NULL,
    reward = NULL,
    states = NULL,
    
    initialize = function(shape = c(4, 4), terminal.states = c(1, 16)) {
      self$shape = shape
      self$terminal.states = terminal.states
      self$actions = c("left", "right", "up", "down")
      self$n.states = prod(shape)
      self$n.actions = length(self$actions)
      self$states = seq_len(self$n.states)
      self$episode.over = FALSE
      private$computeBorderStates()
      private$makeTransitionArray()
      private$makeRewardMatrix()
    },
    
    step = function(state, action) {
      self$episode.over = FALSE
      
      # take action -> sample next state and reward
      self$next.state = sample(self$states, size = 1, prob = self$transition.array[state, , action])
      
      self$reward = self$reward.matrix[state, action]
      
      # episode over if terminalState is reached
      if(self$next.state %in% self$terminal.states) {
        self$episode.over = TRUE
      }
      invisible(self)
    }
  ),
  private = list(
    border.states.left = NULL,
    border.states.right = NULL,
    border.states.up = NULL,
    border.states.down = NULL,
    computeBorderStates = function() {
      private$border.states.left = seq(1, self$n.states, self$shape[2])
      private$border.states.right = seq(self$shape[2], self$n.states, self$shape[2])
      private$border.states.up = seq(1, self$shape[2])
      private$border.states.down = seq(self$n.states - self$shape[2] + 1, self$n.states)
      invisible(self)
    },
    
    makeTransitionArray = function() {
      # make probability transition array for each action (3-dimensional array)
      self$transition.array = array(matrix(0, nrow = self$n.states, ncol = self$n.states),
        dim = c(self$n.states, self$n.states, self$n.actions),
        dimnames = list(NULL, NULL, self$actions))
      # fill in probabilities: when action is taking you off the grid,
      # the new state will be the same as the old state
      for(state in seq_len(self$n.states)) {
        for(action in self$actions) {
          
          if(action == "left") {
            new.state = ifelse(state %in% private$border.states.left, state, state - 1)
          }
          
          if(action == "right") {
            new.state = ifelse(state %in% private$border.states.right, state, state + 1)
          }
          
          if(action == "up") {
            new.state = ifelse(state %in% private$border.states.up, state, state - self$shape[2])
          }
          
          if(action == "down") {
            new.state = ifelse(state %in% private$border.states.down, state, state + self$shape[2])
          }
          self$transition.array[state, new.state, action] = 1
        }
      }
      invisible(self)
    },
    
    makeRewardMatrix = function() {
      # reward matrix: matrix depending on actions and states
      # reward of - 1 for each step
      self$reward.matrix = matrix(- 1, nrow = self$n.states, ncol = self$n.actions, dimnames = list(NULL, self$actions))
      
      # set rewards of terminal states to 0
      self$reward.matrix[self$terminal.states, ] = 0
      invisible(self)
    }
  )
)
