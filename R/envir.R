#' Creates environment
#' 
#' This function creates an reinforcement learning environment from a given 
#' transition.array and reward.matrix. The envir object is an R6 class with a 
#' step method.
#' 
#' @param transition.array numerical matrix (n.states x n.states x n.actions) 
#' for each action giving the probabilities for transitions from one state to 
#' all other states
#' @param reward.matrix numerical matrix the rewards for transitions from one 
#' state to another
#' @param terminal.states integer vector which states are terminal
#' @return R6 class envir
#' @export
#' @importFrom R6 R6Class
#'
#' @section Methods: \describe{ \item{\code{envir$step(state,
#'   action)}}{Takes a step in the gridworld given a state and an action,
#'   returns the next state and reward.} 
#'   \item{\code{envir$setEpisodeOverFalse()}}{Resets the
#'   \code{episode.over} flag of the envir class. Useful when starting a new
#'   episode.} }
#' @usage # env = envir(...)
#' # env$step(state, action)
#'   
#' @examples
#' grid = gridworld$new()
#' env = envir(grid$transition.array, grid$reward.matrix, grid$terminal.states)
envir <- function(transition.array, reward.matrix, terminal.states) {
  
  envir = R6::R6Class("envir",
    public = list(
      shape = NULL,
      n.states = NULL,
      states = NULL,
      terminal.states = NULL,
      non.terminal.states = NULL,
      initial.state = NULL, # has no functionality yet
      n.actions = NULL,
      actions = NULL,
      transition.array = NULL, # better private?
      reward.matrix = NULL, # better private?
      next.state = NULL,
      reward = NULL,
      episode.over = FALSE,
      n.steps = 0,
      
      initialize = function(transition.array, reward.matrix, terminal.states) {
        # self$shape = c(nrow(reward.matrix), nrow(reward.matrix))
        self$terminal.states = terminal.states
        self$actions = seq_len(ncol(reward.matrix))
        self$n.states = nrow(reward.matrix)
        self$n.actions = ncol(reward.matrix)
        self$states = seq_len(self$n.states)
        self$non.terminal.states = self$states[self$states != self$terminal.states]
        self$transition.array = transition.array
        self$reward.matrix = reward.matrix
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
    )
  )
  
  envir$new(transition.array, reward.matrix, terminal.states)
}
