#' #' Gradient Monte Carlo Policy Evaluation using Function Approximation
#' #'
#' #' Evaluate a given policy using value function approximation. Using Monte Carlo
#' #' learning to estimate state value function v from sample returns. The value
#' #' function is approximated by a linear combination of features, which are
#' #' trained using Stochastic Gradient Descent (SGD).
#' #'
#' #' @inheritParams evaluatePolicy
#' #' @inheritParams predictMC
#' #' @param data data.frame containing values of features for each state
#' #' (states x features)
#' #' @param value.function a differentiable value function
#' #'
#' #' @return value function weights
#' #' @export
#' #'
#' gradientMCPolicyEvaluation <- function(envir, policy, data, n.episodes = 10,
#'   function.approximation = "linear", discount.factor = 1, alpha = 0.1) {
#' 
#'   # input checking
#' 
#'   # initialize weights of value function
#'   w = 0
#' 
#'   for (i in seq_len(n.episodes)) {
#' 
#'     initial.state = sample(envir$non.terminal.states)
#'     episode = sampleEpisode(policy, envir, initial.state)
#'     len.episode = length(episode$states)
#' 
#'     # for each state in the episode update parameter weights
#'     # need to observe feature vector for each state
#'     for (j in seq_len(len.episode - 1)) {
#'         sequ = seq(j + 1, len.episode)
#'         rewards = episode$rewards[sequ]
#'         G = estimateReturn(rewards, discount.factor)
#'         error = G - value.function(episode$states[j], w)
#'         grad_v =
#'         w = w + alpha * error * grad_v
#'     }
#' 
#' 
#'   }
#' 
#'   v
#' }
#' 
#' 
#' # State value function v. Given a state and weights, compute the value
#' # ( = expected return) of this state as a linear combination of features.
#' valueFunction = function(x, weights) {
#'   x %*% weights
#' }
#' 
