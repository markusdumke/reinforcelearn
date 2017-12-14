#' Q(sigma) / Q-Learning / Sarsa / Expected Sarsa
#'
#' Value-based reinforcement learning control algorithms.
#'
#' Q(sigma), Q-Learning, Expected Sarsa and Sarsa build a family of temporal-difference learning
#' algorithms, which can be used to find the optimal action value function using the principle
#' of generalized policy iteration.
#' Q(sigma) subsumes Q-Learning, Sarsa and Expected Sarsa as special cases.
#' For more details on the algorithm read De Asis et al. (2017).
#' Expected Sarsa can be used on-policy if \code{target.policy == "egreedy"} else it is identical
#' to Q-Learning.
#'
#' When \code{fun.approx == "table"} the action value function will be represented as a matrix,
#' but you can also use a linear combination of features for function approximation.
#'
#' The raw state observation returned from the environment must be preprocessed using
#' the \code{preprocessState} argument. This function takes the state observation as input and
#' returns a preprocessed state which can be directly used by the function approximator.
#' To use a tabular value function \code{preprocessState} must return an integer value between
#' [0, number of states - 1]. For linear function approximation the output of
#' \code{preprocessState} must be a matrix with one row.
#'
#' Experience replay can be used by specifying a prefilled replay memory using the
#' \code{replay.memory} argument or by specifying the length of the replay memory,
#' which is then filled with experience using a random uniform policy.
#' Sampling can also be prioritized by the error as proposed by Schaul et al. (2016).
#'
#' Note that eligibility traces cannot be used with experience replay.
#'
#' The hyperparameters \code{epsilon}, \code{sigma}, \code{alpha}, \code{lambda} and
#' \code{learning.rate} can be changed over time. Therefore pass on functions that return a new
#' value of the hyperparameter. These updates will be applied after each episode. First argument of
#' the function must be the parameter itself, the second argument the current episode number.
#'
#' @param envir [\code{R6 class}] \cr
#'   The reinforcement learning environment created by \code{\link{makeEnvironment}}.
#' @param fun.approx [\code{character(1)}] \cr
#'   How to represent the value function? Currently \code{"table"} and \code{"linear"} are supported.
#' @param sigma [\code{numeric(1) in [0, 1]}] \cr
#'   Parameter of the Q(sigma) algorithm. It controls if the temporal-difference target
#'   is equal to the sarsa target (for \code{sigma = 1}) or the expected sarsa target
#'   (for \code{sigma = 0}). For intermediate values of \code{sigma} a weighted mean
#'   between the two targets is used.
#' @param lambda [\code{numeric(1) in [0, 1]}] \cr
#'   Eligibility trace decay parameter.
#' @param n.episodes [\code{integer(1)}] \cr
#'   Number of episodes.
#' @param learning.rate [\code{numeric(1)}] \cr
#'   Learning rate used for gradient descent.
#' @param epsilon [\code{numeric(1) in [0, 1]}] \cr
#'   Ratio of random exploration in epsilon-greedy action selection.
#' @param updateEpsilon [\code{function}] \cr
#'   A function that updates \code{epsilon}. It takes two arguments,
#'   \code{epsilon} and the current number of episodes which are finished,
#'   and returns the new \code{epsilon} value.
#' @param updateSigma [\code{function}] \cr
#'   A function that updates \code{sigma}. It takes two arguments,
#'   \code{sigma} and the current number of episodes which are finished,
#'   and returns the new \code{sigma} value.
#' @param updateLambda [\code{function}] \cr
#'   A function that updates \code{lambda}. It takes two arguments,
#'   \code{lambda} and the current number
#'   of episodes which are finished, and returns the new \code{lambda} value.
#' @param updateLearningRate [\code{function}] \cr
#'   A function that updates the learning rate. It takes two arguments, \code{learning.rate}
#'   and the current number of episodes which are finished, and returns the new
#'   \code{learning.rate} value.
#' @param updateAlpha [\code{function}] \cr
#'   A function that updates \code{alpha}. It takes two arguments,
#'   \code{alpha} and the current number
#'   of episodes which are finished, and returns the new \code{alpha} value.
#' @param initial.value [\code{numeric}] \cr
#'   Initial value function matrix or weight matrix.
#'   If \code{NULL} weights will be initialized to 0.
#'   Only used for tabular or linear function approximation.
#' @param n.states [\code{integer(1)}] \cr
#'   Number of states for tabular value function.
#'   Only needed if the state space is continuous, but
#'   will be transformed by \code{preprocessState}, so that a single integer value is returned.
#'   Else the number of states is accessed from the \code{envir$n.states} argument.
#' @param discount [\code{numeric(1) in [0, 1]}] \cr
#'   Discount factor.
#' @param target.policy [\code{character(1)}] \cr
#'   Should the expected sarsa target be computed on-policy
#'   using the epsilon-greedy behavior policy (\code{target.policy = "egreedy"})
#'   or off-policy using a greedy target policy (\code{target.policy = "greedy"})?
#' @param double.learning [\code{logical(1)}] \cr
#'   Should double learning be used?
#' @param replay.memory [\code{list}] \cr
#'   Initial replay memory, which can be passed on.
#'   Each list element must be a list containing \code{state}, \code{action},
#'   \code{reward} and \code{next.state}.
#' @param replay.memory.size [\code{integer(1)}] \cr
#'   Size of the replay memory. Only used if \code{replay.memory} is \code{NULL}. Then
#'   the replay memory will be initially filled with experiences following a uniform random policy.
#' @param batch.size [\code{integer(1)}] \cr
#'   Batch size, how many experiences are sampled from the replay memory at each step?
#'   Must be smaller than the size of the replay memory!
#' @param alpha [\code{numeric(1) in [0, 1]}] \cr
#'   If \code{alpha = 0} sampling from the replay memory will be uniform,
#'   otherwise observations with a high error will be proportionally prioritized.
#' @param theta [\code{numeric(1) in (0, 1]}] \cr
#'   \code{theta} is a small positive constant that prevents the edge-case of transitions
#'   in the replay memory not being revisited once their error is zero.
#' @param model [\code{any}] \cr
#'   Currently unused parameter.
#' @param preprocessState [\code{function}] \cr
#'   A function that takes the state observation returned from the environment as an input and
#'   preprocesses this in a way the algorithm can work with it.
#' @param eligibility.type [\code{numeric(1)}] \cr
#'   Type of eligibility trace, use \code{eligibility.type = 1} for replacing traces,
#'   \code{eligibility.type = 0} for accumulating traces or
#'   intermediate values for a mixture between both.
#' @rdname qSigma
#' @return [\code{list(4)}] \cr
#'   Returns the action value function or model parameters [\code{matrix}] and the
#'   return and number of steps per episode [\code{numeric}].
#'   For Double Learning both value functions will be returned.
#' @references De Asis et al. (2017): Multi-step Reinforcement Learning: A Unifying Algorithm
#' @references Hasselt et al. (2010): Double Q-Learning
#' @references Mnih et al. (2013): Playing Atari with Deep Reinforcement Learning
#' @references Schaul et al. (2016): Prioritized Experience Replay
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @importFrom stats predict
#' @export
#' @examples
#' # Solve Windy Gridworld
#' env = makeEnvironment("WindyGridworld")
#'
#' res = qlearning(env, n.episodes = 20)
#' print(res$steps)
#'
#' # State value function
#' print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))
#'
#' # Policy
#' policy = max.col(res$Q1) - 1L
#' print(matrix(policy, ncol = 10, byrow = TRUE))
#'
#' # Decay epsilon over time. Each 10 episodes epsilon will be halfed.
#' decayEpsilon = function(epsilon, i) {
#'   if (i %% 10 == 0) {
#'     epsilon = epsilon * 0.5
#'   }
#'   epsilon
#' }
#'
#' res = expectedSarsa(env, epsilon = 0.5, updateEpsilon = decayEpsilon, n.episodes = 20)
#'
#' # Solve the Mountain Car problem using linear function approximation
#' m = makeEnvironment("MountainCar")
#'
#' # Define preprocessing function (we use grid tiling)
#' n.tilings = 8
#' max.size = 4096
#' iht = IHT(max.size)
#'
#' position.max = m$state.space.bounds[[1]][2]
#' position.min = m$state.space.bounds[[1]][1]
#' velocity.max = m$state.space.bounds[[2]][2]
#' velocity.min = m$state.space.bounds[[2]][1]
#' position.scale = n.tilings / (position.max - position.min)
#' velocity.scale = n.tilings / (velocity.max - velocity.min)
#'
#' # Scale state first, then get active tiles and return n hot vector
#' gridTiling = function(state) {
#'   state = c(position.scale * state[1], velocity.scale * state[2])
#'   active.tiles = tiles(iht, 8, state)
#'   nHot(active.tiles, max.size, out = "vector")
#' }
#'
#' set.seed(123)
#' res = sarsa(m, fun.approx = "linear", preprocessState = gridTiling, n.episodes = 20)
#' print(res$returns)
#'
qSigma = function(envir, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, sigma = 1,
  target.policy = "egreedy", lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  # argument checking
  checkArgs(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma,
    target.policy, lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)

  # here the learning algorithm will be initialized depending on the function arguments
  agent = qSigmaAgent$new(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma,
    target.policy, lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)

  for (i in seq_len(n.episodes)) {
    agent$runEpisode(envir, i)
  }
  # close graphical representation of environment if existing
  if (!is.null(envir$close)) {
    envir$close()
  }

  list(Q1 = agent$Q1, Q2 = agent$Q2, steps = agent$episode.steps, returns = agent$returns)
}

#=================================================================
# Check function arguments of qSigma
checkArgs = function(envir, fun.approx, preprocessState,
  model, initial.value, n.states, n.episodes, sigma,
  target.policy, lambda, eligibility.type, learning.rate,
  epsilon, discount, double.learning,
  replay.memory, replay.memory.size, batch.size, alpha, theta,
  updateEpsilon, updateSigma, updateLambda, updateAlpha,
  updateLearningRate) {

  checkmate::assertClass(envir, "R6")
  stopifnot(envir$action.space == "Discrete")
  checkmate::assertChoice(fun.approx, c("table", "linear"))
  checkmate::assertNumber(discount, lower = 0, upper = 1)
  checkmate::assertNumber(learning.rate, lower = 0, upper = 1)
  checkmate::assertNumber(epsilon, lower = 0, upper = 1)
  checkmate::assertNumber(alpha, lower = 0, upper = 1)
  checkmate::assertNumber(theta, lower = 0, upper = 1)
  if (fun.approx == "table") {
    checkmate::assertMatrix(initial.value, null.ok = TRUE, any.missing = FALSE,
      nrows = envir$n.states, ncols = envir$n.actions)
  }
  if (fun.approx == "linear") {
    envir$reset()
    n.weights = length(preprocessState(envir$state))
    checkmate::assertMatrix(initial.value, null.ok = TRUE, any.missing = FALSE,
      nrows = n.weights, ncols = envir$n.actions)
  }
  checkmate::assertMatrix(initial.value, null.ok = TRUE, any.missing = FALSE,
    nrows = envir$n.states, ncols = envir$n.actions)
  checkmate::assertNumber(lambda, lower = 0, upper = 1)
  checkmate::assertNumber(sigma, lower = 0, upper = 1)
  checkmate::assertNumber(eligibility.type, lower = 0, upper = 1)
  checkmate::assertInt(n.states, null.ok = TRUE)
  checkmate::assertInt(n.episodes, lower = 1)
  checkmate::assertInt(replay.memory.size, lower = 1)
  checkmate::assertInt(batch.size, lower = 1)
  checkmate::assertList(replay.memory, types = "list", null.ok = TRUE, any.missing = FALSE)
  if (!is.null(replay.memory)) {
    for (i in seq_along(replay.memory)) {
      checkmate::assertList(replay.memory[[i]], len = 4)
       # names = c("state", "action", "reward", "next.state"))
      checkmate::assertInt(replay.memory[[i]][["action"]])
      checkmate::assertNumber(replay.memory[[i]][["reward"]])
    }

    replay.memory.size = length(replay.memory)
  }
  if (replay.memory.size < batch.size) {
    stop("Batch size must be smaller than replay memory size!")
  }
  checkmate::assertChoice(target.policy, c("greedy", "egreedy"))
  checkmate::assertFlag(double.learning)
  checkmate::assertFunction(preprocessState)
  checkmate::assertFunction(updateEpsilon)
  checkmate::assertFunction(updateSigma)
  checkmate::assertFunction(updateLambda)
  checkmate::assertFunction(updateAlpha)
  checkmate::assertFunction(updateLearningRate)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
qlearning = function(envir, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100,
  lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 0,
    target.policy = "greedy", lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
sarsa = function(envir, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100,
  lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 1,
    target.policy = "egreedy", lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)
}

#' @export
#' @inheritParams qSigma
#' @rdname qSigma
expectedSarsa = function(envir, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, target.policy = "egreedy",
  lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 0, target.policy,
    lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)
}
