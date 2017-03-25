#' Q-Learning with linear function approximation
#'
#' @inheritParams qlearning
#' @inheritParams predictMC
#' @param n.features integer scalar: number of features
#' @param make_feature_vector function which returns a feature vector for a given state observation.
#' @param ... arguments passed to make_feature_vector
#'
#' @return Numeric matrix of weights. Number of weights is number of features x number of actions
#' @export
#' @seealso [qlearning]
#'
#' @examples
#' \dontrun{
#' # Make sure you have gym-http-api and python installed.
#' # Then start a server from command line by running: python gym_http_server.py
#' options(gym.api.path = "C:/Users/M/Downloads/WinPython-64bit-3.6.0.1Qt5/scripts/gym-http-api")
#' MountainCar = makeEnvironment("MountainCar-v0")
#' 
#' res = qlearning_approx(MountainCar, make_feature_vector, n.features = 10, 
#'   state.space.bounds = MountainCar$state.space.bounds, n.grid = 10, n.episodes = 10, 
#'   epsilon = 0)
#' }
#' 
qlearning_approx <- function(envir, make_feature_vector, n.features, 
  n.episodes = 10, alpha = 0.1, epsilon = 0.1, initial.weights = NULL,
  discount.factor = 1, render = TRUE, seed = NULL, ...) { # initial.weights argument?
  
  # input checking
  if (!is.null(seed)) set.seed(seed)
  
  episode.finished.after = rep(0, n.episodes)
  if (is.null(initial.weights)) {
    weights = matrix(runif(n.features ^ 2 * envir$n.actions), 
      nrow = n.features ^ 2, ncol = envir$n.actions)
  } else {
    weights = initial.weights
  }
  # Q = rep(0, envir$n.actions)
  
  for (i in seq_len(n.episodes)) {
    print(paste("Episode:", i))
    envir$reset()
    state = envir$state
    j = 0
    
    while (envir$episode.over == FALSE) {
      j = j + 1
      features.state = make_feature_vector(state, ...)
      Q.state = estimate_Q(features.state, weights = weights)
      action = sample_epsilon_greedy_action(Q.state, epsilon)
      envir$step(action, render = render)
      
      next.state = envir$state
      reward = envir$reward
      
      features.next.state = make_feature_vector(next.state, ...)
      Q.next.state = estimate_Q(features.next.state, weights = weights)
      
      td.target =  reward + discount.factor * max(Q.next.state)
      td.error = td.target - Q.state[action + 1]
      grad = td.error * features.state
      weights[, action + 1] = weights[, action + 1] + alpha * grad
      
      state = next.state
      
      if (envir$episode.over) {
        # every episode make a cost-to-go plot
        episode.finished.after[i] = j
        print(paste("Episode", i, "finished after", j, "time steps."))
        break
      } 
    }
  }
  list(weights = weights, episode.finished.after = episode.finished.after)
}
