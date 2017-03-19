#' Windy Gridworld environment
#' 
#' Simple WindyGridworld problem for reinforcement learning. Actions include
#' going left, right, up and down. In each column the wind pushes you up a
#' specific number of steps.
#' 
#' @section Methods: \describe{ \item{\code{WindyGridworld$new(shape,
#'   initial.state, terminal.states, wind)}} {Creates a new
#'   \code{WindyGridworld} with a specific \code{shape}, which is a length-two
#'   integer, e.g. \code{c(7, 10)}. \code{terminal.states} is an integer vector
#'   of the terminal states in the gridworld. Default is \code{37}.
#'   Initial.state is an integer vector, e.g. 30. \code{wind} is an integer
#'   vector specifying the strength of the upward wind.}}
#'   
#' Possible actions include going left, right, down or up. If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#' @docType class
#' @references Windy gridworld example from Sutton & Barto, chapter 6
#' @usage WindyGridworld$new()
#' @export
#' @importFrom R6 R6Class
#' @seealso [makeEnvironment]
#' @examples
#' WindyGridworld1 = WindyGridworld$new()
#' 
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
    initial.state = NULL,
    transition.array = NULL, 
    reward.matrix = NULL,
    wind = NULL,
    
    initialize = function(shape = c(7, 10), initial.state = 30, 
      terminal.states = 37, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)) {
      
      self$wind = wind
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
    
    # make probability transition array for each action (3-dimensional array)
    makeTransitionArray = function() {
      
      self$transition.array = array(matrix(0, nrow = self$n.states, ncol = self$n.states),
        dim = c(self$n.states, self$n.states, self$n.actions),
        dimnames = list(NULL, NULL, self$actions))
      
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
          
          # push next.state upwards because of wind
          column = new.state
          while (column > (self$shape[2] - 1)) {
            column = column - self$shape[2]
          }
          new.state = new.state - self$wind[column + 1] * self$shape[2]
          while (new.state <= 0) {
            new.state = new.state + self$shape[2]
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
