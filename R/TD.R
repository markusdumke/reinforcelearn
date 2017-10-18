#' TD(lambda)
#'
#' Estimate the state value function for a given policy using temporal difference learning.
#'
#' Can be used for episodic or continuing environments, please specify either a maximum number of
#' steps or a maximum number of episodes.
#'
#' @inheritParams qSigma
#' @param policy [\code{matrix} | \code{function}] \cr
#'   A policy specified as a probability matrix (states x actions) or
#'   a function returning an action given a preprocessed state observation
#'   (with linear function approximation).
#' @param n.steps [\code{integer(1)}] \cr
#'   Number of evaluations (steps in the environment).
#'
#' @return [\code{list}] \cr
#' Returns the state value function V and some statistics about learning behavior, e.g. the number
#' of steps and return per episode.
#'
#' @export
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction. Chapter 6
#' @examples
#' # Random Walk Task (Sutton & Barto Example 6.2)
#' P = array(dim = c(7, 7, 2))
#' P[, , 1] = matrix(c(rep(c(1, rep(0, 6)), 2), c(0, 1, rep(0, 5)),
#'   c(0, 0, 1, rep(0, 4)), c(rep(0, 3), 1, rep(0, 3)), c(rep(0, 4), 1, rep(0, 2)),
#'   c(rep(0, 6), 1)), ncol = 7, byrow = TRUE)
#' P[, , 2] = matrix(c(c(1, rep(0, 6)), c(0, 0, 1, rep(0, 4)),
#'   c(rep(0, 3), 1, rep(0, 3)), c(rep(0, 4), 1, rep(0, 2)),
#'   c(rep(0, 5), 1, 0), c(rep(0, 6), 1), c(rep(0, 6), 1)), ncol = 7, byrow = TRUE)
#' R = matrix(c(rep(0, 12), 1, 0), ncol = 2)
#' env = makeEnvironment(transitions = P, rewards = R, initial.state = 3)
#'
#' # Uniform random policy
#' random.policy = matrix(1 / env$n.actions, nrow = env$n.states,
#'   ncol = env$n.actions)
#'
#' # Estimate state value function with TD(0)
#' res = td(env, random.policy, n.episodes = 100)
#'
td = function(envir, policy, fun.approx = "table", preprocessState = identity,
  initial.value = NULL, n.episodes = NULL, n.steps = NULL, discount = 1,
  lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  updateLambda = identity2, updateLearningRate = identity2) {

  checkmate::assertClass(envir, "R6")
  checkmate::assertChoice(fun.approx, c("table", "linear"))
  checkmate::assertFunction(preprocessState)
  stopifnot(envir$action.space == "Discrete")
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0)
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
  checkmate::assertInt(n.steps, lower = 1, null.ok = TRUE)
  checkmate::assertInt(n.episodes, lower = 1, null.ok = TRUE)
  checkmate::assertVector(initial.value, null.ok = TRUE)
  checkmate::assertFunction(updateLambda)
  checkmate::assertFunction(updateLearningRate)
  if (is.null(n.steps) & is.null(n.episodes)) {
    stop("Please specify either a maximal number of steps or episodes!")
  }
  if (fun.approx == "table") {
    checkmate::assertMatrix(policy, ncols = envir$n.actions, nrows = envir$n.states)
    if (any(rowSums(policy) != 1)) {
      stop("The probabilities of each row of the policy must sum to 1.")
    }
  } else {
    checkmate::assertFunction(policy)
  }

  if (is.null(n.episodes)) {
    n.episodes = Inf
  }

  if (is.null(n.steps)) {
    n.steps = Inf
  }

  envir$reset()
  if (fun.approx == "linear") {
    n.weights = length(preprocessState(envir$state))
  } else {
    n.weights = envir$n.states
  }
  if (is.null(initial.value)) {
    V = rep(0, n.weights)
  } else {
    stopifnot(length(initial.value) == n.weights)
    V = initial.value
  }

  eligibility = rep(0, n.weights)
  envir$reset()
  state = preprocessState(envir$state)
  episode = 0
  step = 0
  steps = vector()
  returns = vector()
  return.ep = 0

  if (fun.approx == "table") {
    getAction = function(state) {
      sampleActionFromPolicy(policy[state + 1, ])
    }
  } else {
    getAction = policy
  }

  while (TRUE) {
    if (episode >= n.episodes | step >= n.steps) {
      break
    }
    action = getAction(state)
    envir$step(action)
    return.ep = return.ep + discount^step * envir$reward
    step = step + 1
    s.n = preprocessState(envir$state)

    if (fun.approx == "table") {
      eligibility[state + 1] = eligibility[state + 1] * (1 - eligibility.type) + 1
      td.target = envir$reward + discount * V[s.n + 1]
      td.error = td.target - V[state + 1]
    } else {
      eligibility = eligibility * (1 - eligibility.type) + state
      td.target = envir$reward + discount * V %*% s.n
      td.error = td.target - V %*% state
    }
    V = V + learning.rate * td.error * eligibility
    eligibility = discount * lambda * eligibility

    state = s.n

    if (envir$done == TRUE) {
      episode = episode + 1
      message(paste("Episode", episode, "finished after", envir$n.steps, "steps."))
      steps = append(steps, envir$n.steps)
      returns = append(returns, return.ep)
      return.ep = 0
      envir$reset()
      state = preprocessState(envir$state)
      eligibility = rep(0, n.weights)
      learning.rate = updateLearningRate(learning.rate, i)
      lambda = updateLambda(lambda, i)
    }
  }

  if (length(returns) == 0) {
    returns = return.ep
  }
  if (length(steps) == 0) {
    steps = step
  }
  list(V = V, steps = steps, returns = returns)
}
