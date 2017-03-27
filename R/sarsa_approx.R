#' SARSA with linear function approximation
#' 
#' SARSA(0) implemented. Function approximation using linear tiles.
#'
#' @inheritParams sarsa
#' @inheritParams predictMC
#' @param n.features integer scalar: number of features
#' @param render logical scalar: should the environment be rendered
#' @param make_feature_vector function which produces a feature vector for a given state observation.
#' @param ... arguments passed to make_feature_vector
#'
#' @return Numeric matrix of weights. Number of weights is number of features x number of actions
#' @export
#'
#' @examples
#' \dontrun{
#' # Make sure you have gym-http-api and python installed.
#' # Set path to your gym-http-api folder
#' options(gym.api.path = "C:/Users/M/Downloads/WinPython-64bit-3.6.0.1Qt5/scripts/gym-http-api")
#' MountainCar = makeEnvironment("MountainCar-v0")
#' 
#' weights = sarsa_approx(mcar, make_feature_vector, n.features = 10, 
#'   state.space.bounds = mcar$state.space.bounds, n.grid = 10, n.episodes = 10, 
#'   render = TRUE)
#' }
#' 
sarsa_approx <- function(envir, make_feature_vector, n.features, lambda = 0, 
  n.episodes = 10, alpha = 0.1, epsilon = 0.1, 
  discount.factor = 1, render = TRUE, seed = NULL, ...) { # initial.weights argument?
  
  # input checking
  if (!is.null(seed)) set.seed(seed)
  
  weights = matrix(runif(n.features^2 * envir$n.actions), nrow = n.features^2, ncol = envir$n.actions)
  Q = rep(0, envir$n.actions)
  
  for (i in seq_len(n.episodes)) {
    
    envir$setEpisodeOverFalse()
    state = envir$initial.state
    action = sample_epsilon_greedy_action(Q, epsilon) - 1
    
    for (j in 1:envir$max.steps.episode) {
      
      envir$step(action, render = render)
      
      next.state = envir$next.state
      reward = envir$reward
      features.state = make_feature_vector(state, ...)
      features.next.state = make_feature_vector(next.state, ...)
      Q = estimate_Q(features.next.state, weights = weights)
      next.action = sample_epsilon_greedy_action(Q, epsilon = epsilon) - 1
      
      # update weights using SARSA(0)
      td.target =  reward + discount.factor * estimate_Q(features.next.state, next.action + 1, weights)
      td.error = td.target - estimate_Q(features.state, action + 1, weights)
      grad = td.error * features.state
      weights[, action + 1] = weights[, action + 1] + alpha * grad
      
      action = next.action
      state = next.state
      
      if (envir$episode.over) {
        print(paste("Episode finished after", j, "time steps."))
        break
      } 
    }
  }
  weights
}


#' Make feature vector using tiles
#' 
#' Tiles: Grid across the 2-dimensional state space: each observation
#' will be encoded as a one-hot vector.
#'
#' @param state length-two list: the state observation
#' @param state.space.bounds length-two list: the state space bounds
#' @param n.grid integer scalar: sqrt of number of grid cells
#'
#' @return one-hot feature vector
#' @export
make_feature_vector = function(state, state.space.bounds, n.grid) {
  x = state[[1]]
  y = state[[2]]

  seq_x = seq(state.space.bounds[[1]][1], state.space.bounds[[1]][2],
    length.out = n.grid + 1)
  seq_y = seq(state.space.bounds[[2]][1], state.space.bounds[[2]][2],
    length.out = n.grid + 1)
  grid = matrix(0, nrow = n.grid, ncol = n.grid)

  grid[findInterval(x, seq_x), findInterval(y, seq_y)] = 1
  c(grid)
}

# Estimate Q: Given a state action pair, the function returns a scalar Q value.
# If only a state is provided, it returns a vector of Q values for the different actions.
# state is a feature vector
estimate_Q = function(state, action = NULL, weights) {
  if (is.null(action)) {
    Q = state %*% weights
  } else {
    Q = weights[, action] %*% state
  }
  Q
}
