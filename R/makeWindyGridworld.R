#' Make Windy Gridworld environment
#' 
#' Windy Gridworld problem for reinforcement learning. Actions include
#' going left, right, up and down. In each column the wind pushes you up a
#' specific number of steps (for thenext action). If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#'
#' @param shape length-two integer, shape of the grid
#' @param terminal.states integer vector of the terminal states in the 
#' gridworld, states are numerated starting with 0 with increasing 
#' number from left to right
#' @param wind integer vector specifying the strength of the upward 
#' wind in each cell.
#' 
#' @return R6 class
#' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' grid = makeWindyGridworld()
#' @seealso [makeEnvironment]
#' 
makeWindyGridworld <- function(shape = c(7L, 10L), terminal.states = 37L, 
  wind = c(0L, 0L, 0L, 1L, 1L, 1L, 2L, 2L, 1L, 0L)) {
  WindyGridworld = R6::R6Class("WindyGridworld",
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
      wind = NULL,
      
      initialize = function(shape, terminal.states, wind) {
        self$wind = wind
        self$state.space = "Discrete"
        self$action.space = "Discrete"
        self$shape = shape
        self$terminal.states = terminal.states
        self$actions = seq_len(4) - 1L
        self$n.states = prod(shape)
        self$n.actions = length(self$actions)
        self$states = seq_len(self$n.states) - 1L
        self$non.terminal.states = self$states[self$states != self$terminal.states]
        private$computeBorderStates()
        private$makeTransitionArray()
        private$makeRewardMatrix()
      }
    ),
    
    private = list(
      border.states.left = NULL,
      border.states.right = NULL,
      border.states.up = NULL,
      border.states.down = NULL,
      computeBorderStates = function() {
        private$border.states.left = seq(0L, self$n.states - self$shape[2], self$shape[2])
        private$border.states.right = seq(self$shape[2] - 1L, self$n.states - 1, self$shape[2])
        private$border.states.up = seq(0L, self$shape[2] - 1L)
        private$border.states.down = seq(self$n.states - self$shape[2], self$n.states - 1L)
        invisible(self)
      },
      
      makeTransitionArray = function() {
        self$transition.array = array(matrix(0, nrow = self$n.states, ncol = self$n.states),
          dim = c(self$n.states, self$n.states, self$n.actions))
        
        for(state in self$non.terminal.states) {
          for(action in self$actions) {
            if(action == 0L) { # left
              new.state = ifelse(state %in% private$border.states.left, state, state - 1L)
            }
            if (action == 1L) { # right
              new.state = ifelse(state %in% private$border.states.right, state, state + 1L)
            }
            if (action == 2L) { # up
              new.state = ifelse(state %in% private$border.states.up, state, state - self$shape[2])
            }
            if (action == 3L) { # down
              new.state = ifelse(state %in% private$border.states.down, state, state + self$shape[2])
            }
            
            column = state
            while (column > (self$shape[2] - 1L)) {
              column = column - self$shape[2]
            }
            new.state = new.state - self$wind[column + 1L] * self$shape[2]
            while (new.state <= 0L) {
              new.state = new.state + self$shape[2]
            }
            self$transition.array[state + 1L, new.state + 1L, action + 1L] = 1
          }
        }
        for (state in self$terminal.states) {
          new.state = state
          self$transition.array[state + 1, new.state + 1, ] = 1
        }
        
        invisible(self)
      },
      
      makeRewardMatrix = function() {
        self$reward.matrix = matrix(- 1, nrow = self$n.states, 
          ncol = self$n.actions)
        self$reward.matrix[self$terminal.states + 1, ] = 0
        invisible(self)
      }
    )
  )
  WindyGridworld$new(shape, terminal.states, wind)
}

