#' Policy Evaluation (Dynamic Programming)
#'
#' Evaluate a given policy in an environment using dynamic programming 
#' using  the Bellmann expectation equation as an update rule
#' \deqn{v(s) <- \sum \pi(a|s) (R + \gamma \sum Pss' v(s')])}
#' 
#' @details The algorithm runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than the given precision in all states or if the 
#' specified number of iterations is exhausted.
#' 
#' @inheritParams documentParams
#'
#' @return [\code{numeric}]\cr Returns the state value function v.
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Define uniform random policy, take each action with equal probability
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Evaluate given policy for gridworld example
#' v = evaluatePolicy(grid, random.policy, iter = 100)
#' v = evaluatePolicy(grid, random.policy, precision = 0.001)
#' print(round(matrix(v, ncol = 4, byrow = TRUE)))
#' 
evaluatePolicy = function(envir, policy, v = NULL, discount.factor = 1, 
  precision = 0.0001, iter = NULL) {
  
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertNumeric(v, len = envir$n.states)
  }
  if (envir$n.actions != ncol(policy)) {
    stop("The number of columns of the policy must be equal to the number of actions.")
  }
  if (envir$n.states != nrow(policy)) {
    stop("The number of rows of the policy must be equal to the number of states.")
  }
  if (any(rowSums(policy) != 1)) {
    stop("The probabilities of each row of the policy must sum to 1.")
  }
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(iter, null.ok = TRUE)
  
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  P = envir$transition.array
  improvement = TRUE
  j = 0
  Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  while (improvement == TRUE) {
    if (!is.null(iter)) {
      j = j + 1
      if (j > iter) break
    }
    for (i in seq_len(envir$n.actions)) {
      Q[non.terminal.states + 1, i] = policy[non.terminal.states + 1, i] * 
        (envir$reward.matrix[non.terminal.states + 1, i] + 
            discount.factor * P[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
            v[non.terminal.states + 1])
    }
    v.new = rowSums(Q)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  v
}
# vectorized, but slower version
# evaluatePolicy = function(envir, policy, v = NULL, discount.factor = 1, precision = 0.0001) {
#   
#   stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
#   if (is.null(v)) {
#     v = rep(0, envir$n.states)
#   } else {
#     checkmate::assertVector(v, len = envir$n.states)
#   }
#   v.new = v
#   P = matrix(envir$transition.array, nrow = envir$n.states, ncol = envir$n.actions * envir$n.states)
#   R = envir$reward.matrix
#   improvement = TRUE
#   
#   while (improvement == TRUE) {
#     v2 = Matrix::bdiag(v, v, v, v)
#     d = matrix(policy * (R + (P %*% v2)), nrow = envir$n.states, ncol = envir$n.actions)
#     v.new = rowSums(d)
#     improvement = any(abs(v - v.new) > precision)
#     v = v.new
#   }
#   v
# }

#---------------------------------------------------------------------

#' Policy Iteration (Dynamic Programming)
#' 
#' Find optimal policy by dynamic programming. Iterate between evaluating a 
#' given policy and improving the policy by a greedy update. 
#' Converges to the optimal policy.
#' 
#' @details The algorithm runs until the policy does not change 
#' in two subsequent steps or if the 
#' specified number of iterations is exhausted.
#'
#' @inheritParams documentParams
#'
#' @return [\code{list(2)}] \cr
#' Returns the optimal state value function [\code{numeric}] 
#' and the optimal policy [\code{matrix}] (number of states x number of actions)
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @seealso \code{\link{iterateValue}}
#' @examples
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#' res = iteratePolicy(grid)
#' 
iteratePolicy = function(envir, initial.policy = NULL, 
  discount.factor = 1, precision = 0.0001, iter = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(iter, null.ok = TRUE)
  
  if (is.null(initial.policy)) {
    initial.policy = matrix(1 / envir$n.actions, nrow = envir$n.states, 
      ncol = envir$n.actions)
  } else {
    if (envir$n.actions != ncol(initial.policy)) {
      stop("The number of columns of the policy must be equal to the number of actions.")
    }
    if (envir$n.states != nrow(initial.policy)) {
      stop("The number of rows of the policy must be equal to the number of states.")
    }
    if (any(rowSums(initial.policy) != 1)) {
      stop("The probabilities of each row of the policy must sum to 1.")
    }
  }
  
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  policy = initial.policy
  Q = policy
  v = rep(0, envir$n.states)
  j = 0
  
  while (TRUE) {
    if (!is.null(iter)) {
      j = j + 1
      if (j > iter) break
    }
    v = evaluatePolicy(envir, policy, v, discount.factor, precision)
    policy.old = policy
    for (i in seq_len(envir$n.actions)) {
      Q[non.terminal.states + 1, i] = envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * envir$transition.array[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
        v[non.terminal.states + 1]
    }
    policy = improvePolicy(Q)
    if (identical(policy.old, policy)) break
  }
  
  list(v = v, policy = policy)
}

#---------------------------------------------------------------------

#' Value Iteration (Dynamic Programming)
#'
#' Find optimal policy by dynamic programming. Iterate between evaluating a
#' given policy (only one step), then improving the policy by a greedy update.
#' Converges to the optimal policy.
#' 
#' @details The algorithm runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than the given precision in all states or if the 
#' specified number of iterations is exhausted.
#'
#' @inheritParams documentParams
#'
#' @return [\code{list(2)}] \cr
#'   Returns the optimal state value function [\code{numeric}] 
#'   and the optimal policy [\code{matrix}] (number of states x number of actions)
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @seealso \code{\link{iteratePolicy}}
#' @examples
#' grid = makeEnvironment(transition.array = gridworld$transitions, 
#'   reward.matrix = gridworld$rewards)
#' res = iterateValue(grid)
#' 
iterateValue = function(envir, v = NULL, discount.factor = 1, 
  precision = 0.0001, iter = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    checkmate::assertNumeric(v, len = envir$n.states)
  }
  checkmate::assertNumber(discount.factor, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(iter, null.ok = TRUE)
  non.terminal.states = setdiff(seq(0, envir$n.states - 1), envir$terminal.states)
  P = envir$transition.array
  improvement = TRUE
  j = 0
  
  Q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  while (improvement == TRUE) {
    if (!is.null(iter)) {
      j = j + 1
      if (j > iter) break
    }
    for (i in seq_len(envir$n.actions)) {
      Q[non.terminal.states + 1, i] = envir$reward.matrix[non.terminal.states + 1, i] + 
        discount.factor * P[non.terminal.states + 1, non.terminal.states + 1, i] %*% 
        v[non.terminal.states + 1]
    }
    v.new = apply(Q, 1, max)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  policy = returnPolicy(Q, epsilon = 0)
  list(v = v, policy = policy)
}


