#' Dynamic Programming
#'
#' These functions solve a Markov Decision Process using a model of the environment.
#' \code{evaluatePolicy} evaluates a given policy, 
#' \code{iteratePolicy} and \code{iterateValue} can be used to find the optimal policy.
#' 
#' \code{evaluatePolicy} runs until the improvement in the state value 
#' function in two subsequent steps is smaller than the given precision in all states or if the 
#' specified number of iterations is exhausted.
#' 
#' Both \code{iteratePolicy} and \code{iterateValue} alternate between evaluating a policy and 
#' improving the current policy by acting greedily with respect to the current policy's value function.
#' The difference between these two algorithms is that 
#' \code{iteratePolicy} evaluates the policy until some stop criterion is met, 
#' while \code{iterateValue} evaluates each policy only one step and
#' then immediately improves upon the current policy.
#' 
#' When the \code{policy} argument is \code{NULL} the initial policy will be a uniform random policy.
#' 
#' \code{iteratePolicy} stops if the policy does not change in two subsequent iterations or if the 
#' specified number of iterations is exhausted. For the policy evaluation step in policy iteration 
#' the same stop criteria mentioned above are applied.
#' 
#' \code{iterateValue} runs until the improvement in the value 
#' function in two subsequent steps
#' is smaller than the given precision in all states or if the 
#' specified number of iterations is exhausted.
#' 
#' @rdname dp
#' @inheritParams qSigma
#' @param policy [\code{matrix(n.states x n.actions)}] \cr 
#'   A policy specified as a probability matrix (states x actions).
#' @param v [\code{numeric(n.states)}] \cr 
#'   Initial state value function. Terminal states must have a value of 0!
#' @param q [\code{matrix(n.states x n.actions)}] \cr 
#'   Initial action value function. Terminal states must have a value of 0!
#' @param precision [\code{numeric(1)}] \cr 
#'   Algorithm stops when improvement is
#'   smaller than precision.
#' @param n.iter [\code{integer(1)}] \cr 
#'   Number of iterations. If supplied the \code{precision} argument will be ignored.
#'
#' @return [\code{list(3)}] \cr
#' Returns the state value function [\code{numeric}], the 
#' action value function [\code{matrix}]
#' and the policy [\code{matrix}].
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Set up gridworld problem
#' grid = makeEnvironment(transitions = gridworld$transitions, 
#'   rewards = gridworld$rewards)
#'   
#' # Define uniform random policy, take each action with equal probability
#' random.policy = matrix(1 / grid$n.actions, nrow = grid$n.states, 
#'   ncol = grid$n.actions)
#' 
#' # Evaluate given policy
#' res = evaluatePolicy(grid, random.policy, precision = 0.001)
#' print(round(matrix(res$v, ncol = 4, byrow = TRUE)))
#' 
evaluatePolicy = function(envir, policy, v = NULL, q = NULL, 
  discount = 1, precision = 0.0001, n.iter = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertMatrix(policy, nrows = envir$n.states, 
    ncols = envir$n.actions, any.missing = FALSE)
  if (any(rowSums(policy) != 1)) {
    stop("The probabilities of each row of the policy must sum to 1.")
  }
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(n.iter, null.ok = TRUE)
  checkmate::assertNumeric(v, len = envir$n.states, null.ok = TRUE)
  checkmate::assertMatrix(q, nrows = envir$n.states, 
    ncols = envir$n.actions, null.ok = TRUE)
  
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    if (any(v[envir$terminal.states + 1] != 0)) {
      stop("State values of terminal states must be 0!")
    }
  }
  if (is.null(q)) {
    q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  } else {
    if (any(q[envir$terminal.states + 1, ] != 0)) {
      stop("Action values of terminal states must be 0!")
    }
  }
  P = envir$transitions
  R = envir$rewards
  improvement = TRUE
  j = 0
  
  res = evaluatePolicy2(envir, policy, v, q, discount, precision, n.iter, P, R, improvement, j)
  list(v = res$v, q = res$q, policy = policy)
}

getActionValue = function(P, R, discount, v) {
  R + discount * apply(P, 3, function(x, v) x %*% v, v = v)
}

# getStateValue = function(policy, q) {
#   rowSums(policy * q)
# }

evaluatePolicy2 = function(envir, policy, v, q, discount, 
  precision, n.iter, P, R, improvement, j) {
  while (improvement == TRUE) {
    if (!is.null(n.iter)) {
      j = j + 1
      if (j > n.iter) break
    }
    q = getActionValue(P, R, discount, v)
    v.new = rowSums(policy * q)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  list(v = v, q = q)
}

#---------------------------------------------------------------------
#' @inheritParams evaluatePolicy
#' @inheritParams qSigma
#' @rdname dp
#' @param n.iter.eval [integer(1)] \cr
#'   Number of iterations per evaluation step.
#' @param precision.eval [numeric(1)] \cr
#'   Policy evaluation stops when improvement is smaller than precision.
#' @export
#' @examples
#' # Find optimal policy using Policy Iteration
#' res = iteratePolicy(grid)
#' print(round(matrix(res$v, ncol = 4, byrow = TRUE)))
#' 
iteratePolicy = function(envir, policy = NULL, discount = 1, 
  n.iter = NULL, precision.eval = 0.0001, n.iter.eval = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertMatrix(policy, nrows = envir$n.states, 
    ncols = envir$n.actions, any.missing = FALSE, null.ok = TRUE)
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(precision.eval, lower = 0)
  checkmate::assertInt(n.iter, null.ok = TRUE)
  checkmate::assertInt(n.iter.eval, null.ok = TRUE)
  
  if (is.null(policy)) {
    policy = matrix(1 / envir$n.actions, nrow = envir$n.states, ncol = envir$n.actions)
  }
  if (any(rowSums(policy) != 1)) {
    stop("The probabilities of each row of the policy must sum to 1.")
  }
  
  q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  v = rep(0, envir$n.states)
  P = envir$transitions
  R = envir$rewards
  improvement = TRUE
  j = 0
  
  while (TRUE) {
    if (!is.null(n.iter)) {
      j = j + 1
      if (j > n.iter) break
    }
    res = evaluatePolicy2(envir, policy, v, q, discount, precision.eval, 
      n.iter.eval, P, R, improvement, j = 0)
    v = res$v
    q = res$q
    policy.old = policy
    policy = improvePolicy(q)
    if (identical(policy.old, policy)) break
  }
  
  list(v = v, q = q, policy = policy)
}

#---------------------------------------------------------------------
#' @inheritParams evaluatePolicy
#' @inheritParams iteratePolicy
#' @rdname dp
#' @export
#' @examples
#' # Find optimal policy using Value Iteration
#' res = iterateValue(grid)
#' print(res$policy)
#' 
iterateValue = function(envir, v = NULL, q = NULL, discount = 1, 
  precision = 0.0001, n.iter = NULL) {
  
  checkmate::assertClass(envir, "R6")
  stopifnot(envir$state.space == "Discrete" & envir$action.space == "Discrete")
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(precision, lower = 0)
  checkmate::assertInt(n.iter, null.ok = TRUE)
  checkmate::assertNumeric(v, len = envir$n.states, null.ok = TRUE)
  checkmate::assertMatrix(q, nrows = envir$n.states, 
    ncols = envir$n.actions, null.ok = TRUE)
  
  if (is.null(v)) {
    v = rep(0, envir$n.states)
  } else {
    if (any(v[envir$terminal.states + 1] != 0)) {
      stop("State values of terminal states must be 0!")
    }
  }
  if (is.null(q)) {
    q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  } else {
    if (any(q[envir$terminal.states + 1, ] != 0)) {
      stop("Action values of terminal states must be 0!")
    }
  }
  
  P = envir$transitions
  R = envir$rewards
  improvement = TRUE
  j = 0
  
  q = matrix(0, nrow = envir$n.states, ncol = envir$n.actions)
  while (improvement == TRUE) {
    if (!is.null(n.iter)) {
      j = j + 1
      if (j > n.iter) break
    }
    q = getActionValue(P, R, discount, v)
    v.new = apply(q, 1, max)
    improvement = any(abs(v - v.new) > precision)
    v = v.new
  }
  policy = improvePolicy(q)
  list(v = v, q = q, policy = policy)
}

improvePolicy = function(Q) {
  greedy.actions = apply(Q, 1, argmax)
  policy = matrix(0, nrow = nrow(Q), ncol = ncol(Q))
  policy[matrix(c(seq_len(nrow(Q)), greedy.actions), ncol = 2)] = 1
  policy
}

argmax = function(x) {
  nnet::which.is.max(x)
}
