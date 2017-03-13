# Windy Gridworld

windyGridworld = R6::R6Class("windy_gridworld",
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
    wind = 0,
    
    initialize = function(shape = c(7, 10), start.state = 22, 
      terminal.states = 29, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0)) {
      
      # implement input checking

      self$terminal.states = terminal.states
      self$actions = c("left", "right", "up", "down")
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
    
    makeTransitionArray = function() {
      # make probability transition array for each action (3-dimensional array)
      self$transition.array = array(matrix(0, nrow = self$n.states, 
        ncol = self$n.states),
        dim = c(self$n.states, self$n.states, self$n.actions),
        dimnames = list(NULL, NULL, self$actions))
      # fill in probabilities: when action is taking you off the grid,
      # the new state will be the same as the old state
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
          # in which column is new state
          # apply wind -> - self$shape[2]
          while (new.state > self$shape[2]) {
            column = new.state - self$shape[2]
          }
          new.state = new.state - wind[column] * self$shape[2]
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
