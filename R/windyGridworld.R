#' Windy Gridworld environment
#' 
#' Simple windy gridworld for reinforcement learning. Actions include going 
#' left, right, up and down. In each column the wind pushes you up a specific 
#' number of steps. 
#' 
#' @section Methods: \describe{ \item{\code{windyGridworld$new(shape,
#'   terminal.states)}}{Creates a new \code{windyGridworld} with a specific
#'   \code{shape}, which is a length-two integer, e.g. \code{c(4, 4)}. 
#'   \code{terminal.states} is an integer vector of the terminal states in the
#'   gridworld. Default is \code{c(1, 16)}} \item{\code{windyGridworld$step(state,
#'   action)}}{Takes a step in the gridworld given a state and an action,
#'   returns the next state and reward.} 
#'   \item{\code{windyGridworld$setEpisodeOverFalse()}}{Resets the
#'   \code{episode.over} flag of the gridworld class. Useful when starting a new
#'   episode.} }
#'   
#' Possible actions include going left, right, down or up. If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#' @docType class
#' @references Windy gridworld example from Sutton & Barto, chapter 6
#' @usage #grid = windyGridworld$new()
#' A step method returns the next.state and reward given a state and action.
#' @export
#' @importFrom R6 R6Class
#' @examples
#' set.seed(27)
#' grid = windyGridworld$new()
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
windyGridworld = R6::R6Class("windy_gridworld",
  public = list(
    shape = NULL,
    n.states = NULL,
    states = NULL,
    terminal.states = NULL,
    non.terminal.states = NULL,
    n.actions = NULL,
    actions = NULL,
    transition.array = NULL, 
    reward.matrix = NULL, 
    next.state = NULL,
    reward = NULL,
    episode.over = FALSE,
    n.steps = 0,
    wind = 0,
    
    initialize = function(shape = c(7, 10), start.state = 22, 
      terminal.states = 29, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)) {
      
      # implement input checking
      self$wind = wind
      self$shape = shape
      self$terminal.states = terminal.states
      self$actions = c("left", "right", "up", "down") # seq(1, 4)
      self$n.states = prod(shape)
      self$n.actions = length(self$actions)
      self$states = seq_len(self$n.states)
      self$non.terminal.states = self$states[self$states != self$terminal.states]
  
      private$computeBorderStates()
      private$makeTransitionArray()
      private$makeRewardMatrix()
    },
    
    step = function(state, action) {
      # increment counter
      self$n.steps = self$n.steps + 1
      
      # take action -> sample next state and reward
      self$next.state = sample(self$states, size = 1, 
        prob = self$transition.array[state, , action])
      self$reward = self$reward.matrix[state, action]
      
      # episode over if terminalState is reached
      if (self$next.state %in% self$terminal.states) {
        self$episode.over = TRUE
      }
      invisible(self)
    },
    
    setEpisodeOverFalse = function() {
      self$episode.over = FALSE
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
    
    # make probability transition array for each action (3-dimensional array)
    makeTransitionArray = function() {
      
      self$transition.array = array(matrix(0, nrow = self$n.states, ncol = self$n.states),
        dim = c(self$n.states, self$n.states, self$n.actions),
        dimnames = list(NULL, NULL, self$actions))

      for(state in seq_len(self$n.states)) {
        for(action in self$actions) {
          if(action == "left") {
            new.state = ifelse(state %in% private$border.states.left, state, state - 1)
          }
          if (action == "right") {
            new.state = ifelse(state %in% private$border.states.right, state, state + 1)
          }
          if (action == "up") {
            new.state = ifelse(state %in% private$border.states.up, state, state - self$shape[2])
          }
          if (action == "down") {
            new.state = ifelse(state %in% private$border.states.down, state, state + self$shape[2])
          }
          
          # push next.state upwards because of wind
          column = new.state
          while (column > self$shape[2]) {
            column = column - self$shape[2]
          }
          new.state = new.state - self$wind[column] * self$shape[2]
          while (new.state <= 0) {
            new.state = new.state + self$shape[2]
          }
          self$transition.array[state, new.state, action] = 1
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
      self$reward.matrix[self$terminal.states, ] = 0
      invisible(self)
    }
  )
)
