#' Q(sigma) / Q-Learning / Sarsa / Expected Sarsa
#'
#' Value-based reinforcement learning control algorithms.
#'
#' You must specify the reinforcement learning environment using the \code{envir} argument.
#' It takes an \code{R6 class} created by \code{\link{makeEnvironment}} as input.
#' See documentation there.
#'
#' The algorithms can be used to find the optimal action value function using the principle
#' of generalized policy iteration. They all learn online using temporal-difference learning.
#' Q(sigma) subsumes the well known Q-Learning, Sarsa and Expected Sarsa algorithms as special cases.
#' The default call \code{qsigma()} is exactly equivalent to Sarsa(0). A weighted mean between sarsa
#' and expected sarsa updates can be used by varying the parameter \code{sigma}.
#' When \code{target.policy == "egreedy"} the policy used to compute the expected sarsa is the epsilon-greedy
#' policy used for action selection, when \code{target.policy == "greedy"} a greedy target policy will be
#' used as in Q-Learning. See De Asis et al. (2017) for more details.
#'
#' The functions \code{qlearning} and \code{sarsa} are there for convenience.
#' They call the \code{qSigma} function with a special set of parameters.
#'
#' When \code{fun.approx == "table"} the action value function will be represented using a table,
#' a linear combination of features and a neural network can also be used. For a neural network you
#' need to pass on a keras model via the \code{model} argument. This way it is possible to
#' construct a Deep Q-Network (DQN).
#'
#' The raw state observation returned from the environment can be preprocessed using
#' the \code{preprocessState} function. This function takes the state observation as input and
#' returns a preprocessed state which can be directly used by the function approximator.
#' This is especially important when using function approximation.
#'
#' Experience replay can be used by specifying a prefilled replay memory through the
#' \code{replay.memory} argument or by specifying the length of the replay memory,
#' which will then be filled with random experience. Using the defaults experiences are sampled
#' uniformly, a proportional prioritization proposed by Schaul et al. (2016) can also be used by
#' varying the parameters \code{alpha} and \code{theta}.
#'
#' Using eligibility traces the one-step algorithms can be extended to multi-step algorithms. The
#' parameter \code{lambda} controls the tradeoff between one-step and multi-step methods. Three
#' different kinds of eligibility traces are implemented, accumulating, replacing and dutch traces.
#' See Sutton and Barto (Book draft 2017, Chapter 12) for more details.
#' This only works if no experience replay is used, i.e. \code{replay.memory.size == 1} and for a
#' tabular value function.
#'
#' Double Learning can be used with all of the functions.
#' Then two Q value functions are used, Q1 for action selection and
#' Q2 for action evaluation. After a number of steps the weights of Q2 are replaced by the weights
#' of Q1. See Hasselt et al. (2015) for details.
#'
#' The hyperparameters \code{epsilon}, \code{theta}, \code{sigma}, \code{alpha}, \code{lambda} and
#' \code{learning.rate} can be changed over time. Therefore pass on functions that return a new
#' value of the hyperparameter. These updates will be applied after each episode. First argument of
#' the function must be the parameter itself, the second argument is the current episode number.
#'
#' @param envir [\code{R6 class}] \cr
#'   The reinforcement learning environment created by \code{\link{makeEnvironment}}.
#' @param fun.approx [\code{character(1)}] \cr
#'   How to represent the value function? Currently \code{"table"}, \code{"linear"}
#'   and \code{"neural.network"} are supported.
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
#'   Else the number of states is accessed from the \code{envir} argument.
#' @param discount [\code{numeric(1) in [0, 1]}] \cr
#'   Discount factor.
#' @param target.policy [\code{character(1)}] \cr
#'   Should the temporal-difference target be computed on-policy
#'   using the epsilon-greedy behavior policy (\code{target.policy = "egreedy"})
#'   or off-policy using a greedy policy in the
#'   expected sarsa part of the update (\code{target.policy = "greedy"})?
#' @param double.learning [\code{logical(1)}] \cr
#'   Should double learning be used?
#' @param replay.memory [\code{list}] \cr
#'   Initial replay memory, which can be passed on.
#'   Each list element must be a list containing state, action,
#'   reward and next.state.
#' @param replay.memory.size [\code{integer(1)}] \cr
#'   Size of the replay memory. Only used if \code{replay.memory} is \code{NULL}. Then
#'   the replay memory will be initially filled with experiences following a uniform random policy.
#' @param batch.size [\code{integer(1)}] \cr
#'   Batch size, how many experiences are sampled from the replay memory at each step?
#'   Must be smaller than the size of the replay memory!
#' @param alpha [\code{numeric(1) in [0, 1]}] \cr
#'   If \code{alpha = 0} sampling from replay memory will be uniform, otherwise observations with
#'   high temporal-difference error will be proportionally prioritized.
#' @param theta [\code{numeric(1) in (0, 1]}] \cr
#'   \code{theta} is a small positive constant that prevents the edge-case of transitions
#'   in the replay memory not being revisited once their TD error is zero.
#' @param model [\code{keras model}] \cr
#'   A neural network model specified using the \code{keras} package.
#'   See Details for more information.
#' @param preprocessState [\code{function}] \cr
#'   A function that takes the state observation returned from the environment as an input and
#'   preprocesses this in a way the algorithm can work with it.
#' @param update.target.after [\code{integer(1)}] \cr
#'   When using double learning the target network / table will be updated after
#'   \code{update.target.after} steps.
#' @param eligibility.type [\code{numeric(1)}] \cr
#'   Type of eligibility trace, use \code{eligibility.type = 1} for replacing traces,
#'   \code{eligibility.type = 0} for accumulating traces or intermediate values for a mixture between both.
#' @rdname qSigma
#' @return [\code{list(4)}] \cr
#'   Returns the action value function or model parameters [\code{matrix}] and the
#'   number of steps per episode [\code{numeric}]
#'   and the sum of all rewards in the episode [\code{numeric}].
#' @references De Asis et al. (2017): Multi-step Reinforcement Learning: A Unifying Algorithm
#' @references Hasselt et al. (2015): Deep Reinforcement Learning with Double Q-Learning
#' @references Mnih et al. (2013): Playing Atari with Deep Reinforcement Learning
#' @references Schaul et al. (2016): Prioritized Experience Replay
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @importFrom stats predict
#' @export
#' @examples
#' # Solve Windy Gridworld
#' env = windyGridworld()
#'
#' res = qSigma(env, sigma = 0.5, n.episodes = 20)
#' print(res$steps)
#' res = qlearning(env, n.episodes = 20)
#' print(matrix(round(apply(res$Q1, 1, max), 1), ncol = 10, byrow = TRUE))
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
#' optimal.policy = max.col(res$Q1) - 1L
#' print(matrix(optimal.policy, ncol = 10, byrow = TRUE))
#'
#' # Solve the Mountain Car problem using linear function approximation
#' m = mountainCar()
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
#'   makeNHot(active.tiles, max.size, out = "vector")
#' }
#'
#' set.seed(123)
#' res = sarsa(m, fun.approx = "linear", preprocessState = gridTiling, n.episodes = 20)
#' print(res$returns)
#'
#' \dontrun{
#' env = gridworld()
#' 
#' # Use a neural network as function approximator
#' makeOneHot = function(state) {
#'   one.hot = matrix(rep(0, 16), nrow = 1)
#'   one.hot[1, state + 1] = 1
#'   one.hot
#' }
#'
#' # Define keras model
#' library(keras)
#' model = keras_model_sequential()
#' model %>% layer_dense(units = 4, activation = 'linear', input_shape = c(16))
#'
#' res = qSigma(env, fun.approx = "neural.network", model = model,
#'   preprocessState = makeOneHot, n.episodes = 20)
#' }
#'
qSigma = function(envir, fun.approx = "table", preprocessState = identity,
  model = NULL, initial.value = NULL, n.states = NULL, n.episodes = 100, sigma = 1,
  target.policy = "egreedy", lambda = 0, eligibility.type = 0, learning.rate = 0.1,
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  # argument checking
  checkArgs(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma,
    target.policy, lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning, update.target.after,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)

  # here the learning algorithm will be initialized depending on the function arguments
  agent = qSigmaAgent$new(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma,
    target.policy, lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning, update.target.after,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)

  for (i in seq_len(n.episodes)) {
    agent$runEpisode(envir, i)
  }

  list(Q1 = agent$Q1, Q2 = agent$Q2, steps = agent$episode.steps, returns = agent$returns)
}

#=================================================================
# Check function arguments of qSigma
checkArgs = function(envir, fun.approx, preprocessState,
  model, initial.value, n.states, n.episodes, sigma,
  target.policy, lambda, eligibility.type, learning.rate,
  epsilon, discount, double.learning, update.target.after,
  replay.memory, replay.memory.size, batch.size, alpha, theta,
  updateEpsilon, updateSigma, updateLambda, updateAlpha,
  updateLearningRate) {

  checkmate::assertClass(envir, "R6")
  stopifnot(envir$action.space == "Discrete")
  checkmate::assertChoice(fun.approx, c("table", "neural.network", "linear"))
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
  checkmate::assertInt(update.target.after, lower = 1)
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
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 0,
    target.policy = "greedy", lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning, update.target.after,
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
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 1,
    target.policy = "egreedy", lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning, update.target.after,
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
  epsilon = 0.1, discount = 1, double.learning = FALSE, update.target.after = 1,
  replay.memory = NULL, replay.memory.size = 1, batch.size = 1, alpha = 0, theta = 0.01,
  updateEpsilon = identity2, updateSigma = identity2, updateLambda = identity2,
  updateAlpha = identity2, updateLearningRate = identity2) {

  qSigma(envir, fun.approx, preprocessState,
    model, initial.value, n.states, n.episodes, sigma = 0, target.policy,
    lambda, eligibility.type, learning.rate,
    epsilon, discount, double.learning, update.target.after,
    replay.memory, replay.memory.size, batch.size, alpha, theta,
    updateEpsilon, updateSigma, updateLambda, updateAlpha,
    updateLearningRate)
}
