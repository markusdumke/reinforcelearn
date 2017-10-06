#' Make Gridworld
#' 
#' \code{makeGridworld} creates gridworld environments.
#'
#' @param shape [\code{integer(2)}] \cr 
#'   Shape of the gridworld (number of rows x number of columns).
#' @param goal.states [\code{integer}] \cr 
#'   Goal states in the gridworld.
#' @param cliff.states [\code{integer}] \cr 
#'   Cliff states in the gridworld.
#' @param reward.step [\code{integer(1)}] \cr 
#'   Reward for taking a step.
#' @param cliff.transition.states [\code{integer}] \cr 
#'   States to which the environment transitions if stepping into the cliff. 
#'   If it is a vector, all states will have equal probability. 
#'   Only used when \code{cliff.transition.done == FALSE}, 
#'   else specify the \code{initial.state} argument.
#' @param reward.cliff [\code{integer(1)}] \cr 
#'   Reward for taking a step in the cliff state.
#' @param diagonal.moves [\code{logical(1)}] \cr
#'   Should diagonal moves be allowed?
#' @param wind [\code{integer}] \cr 
#'   Strength of the upward wind in each cell.
#' @param cliff.transition.done [\code{logical(1)}] \cr
#'   Should the episode end after stepping into the cliff?
#' @param stochasticity [\code{numeric(1)}] \cr
#'   Probability of random transition to any of the neighboring states when taking any action.
#' @param ...
#'   Arguments passed on to \code{\link{makeEnvironment}}
#' @return [\code{R6 Class}] \cr
#'   \code{makeGridworld} computes the state transition array and reward matrix and passes these arguments 
#'   on to \code{\link{makeEnvironment}}. The output is an R6 class, the reinforcement learning environment.
#' @details 
#' A gridworld is an episodic navigation task, the goal is to get from start state to goal state. 
#' 
#' Possible actions include going left, right, up or down. If \code{diagonal.moves = TRUE} diagonal 
#' moves are also possible, leftup, leftdown, rightup and rightdown.
#'  
#' When stepping into a cliff state you get a reward of \code{reward.cliff}, 
#' usually a high negative reward and transition to a state specified by \code{cliff.transition.states}. 
#' 
#' In each column a deterministic wind specified via \code{wind} pushes you up a specific number of 
#' grid cells (for the next action).
#' 
#' A stochastic gridworld is a gridworld where with probability \code{stochasticity} the next state
#' is chosen at random from all neighbor states independent of the actual action.
#' 
#' If an action would take you off the grid, the new state is the nearest cell inside the grid. 
#' For each step you get a reward of \code{reward.step}, until you reach a goal state, 
#' then the episode is done.
#' 
#' States are enumerated row-wise and numeration starts with 0. 
#' Here is an example 4x4 grid:
#' \tabular{rrrr}{
#'  0 \tab 1 \tab 2 \tab 3 \cr
#'  4 \tab 5 \tab 6 \tab 7 \cr
#'  8 \tab 9 \tab 10 \tab 11 \cr
#'  12 \tab 13 \tab 14 \tab 15 \cr
#' }
#' So a board position could look like this (G: goal state, x: current state, C: cliff state):
#' \tabular{rrrr}{
#'  G \tab o \tab o \tab o \cr
#'  o \tab o \tab o \tab o \cr
#'  o \tab x \tab o \tab o \cr
#'  o \tab o \tab o \tab C \cr
#' }
#' 
#' A few gridworlds are already included in the package and can be loaded by typing 
#' \code{\link{gridworld}}, \code{\link{windyGridworld}} and \code{\link{cliff}}.
#' @importFrom stats aggregate
#' @seealso \code{\link{makeEnvironment}}
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @export
#' @examples
#' # Gridworld Environment (Sutton & Barto Example 4.1)
#' gridworld = makeGridworld(goal.states = c(0, 15))
#'   
#' # Windy Gridworld (Sutton & Barto Example 6.5) 
#' windy.gridworld = makeGridworld(shape = c(7, 10), goal.states = 37, 
#'   reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0), initial.state = 30)
#'   
#' # Cliff Walking (Sutton & Barto Example 6.6)   
#' cliff = makeGridworld(shape = c(4, 12), goal.states = 47, 
#'   cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100, 
#'   cliff.transition.states = 36, initial.state = 36)
#' 
makeGridworld = function(shape = c(4, 4), goal.states = NULL, cliff.states = NULL,
  reward.step = - 1, reward.cliff = - 100, diagonal.moves = FALSE, wind = rep(0, shape[2]), 
  cliff.transition.states = NULL, cliff.transition.done = FALSE, stochasticity = 0, ...) {
  
  checkmate::assertIntegerish(shape, len = 2)
  if (prod(shape) <= 1) {
    stop("A gridworld with only one state is not allowed!")
  }
  checkmate::assertIntegerish(goal.states)
  goal.states = goal.states + 1
  checkmate::assertIntegerish(cliff.states, null.ok = TRUE)
  if (!is.null(cliff.states)) {
    cliff.states = cliff.states + 1
  }
  checkmate::assertIntegerish(cliff.transition.states, null.ok = TRUE)
  if (!is.null(cliff.transition.states)) {
    cliff.transition.states = cliff.transition.states + 1
  }
  if (any(goal.states > prod(shape)) | any(cliff.states > prod(shape)) | 
      any(cliff.transition.states > prod(shape))) {
    stop("All states must be inside the grid! States are numerated row-wise starting with 0, check Details!")
  }
  checkmate::assertIntegerish(wind, len = shape[2])
  checkmate::assertNumber(reward.step)
  checkmate::assertNumber(reward.cliff)
  checkmate::assertFlag(diagonal.moves)
  checkmate::assertFlag(cliff.transition.done)
  checkmate::assertNumber(stochasticity, lower = 0, upper = 1)
  
  n.states = prod(shape)
  states = seq_len(n.states)
  n.col = shape[2]
  if (diagonal.moves) {
    n.actions = 8
  } else {
    n.actions = 4
  }
  
  rewards = makeRewardMatrix(reward.step, reward.cliff, n.states, n.actions,
    cliff.states, goal.states)
  
  transitions = array(matrix(0, nrow = n.states, ncol = n.states),
    dim = c(n.states, n.states, 8))
  
  border.states = list(left = seq(1, n.states - n.col + 1, n.col), 
    right = seq(n.col, n.states, n.col),
    up = seq(1, n.col),
    down = seq(n.states - n.col + 1, n.states))
  
  non.terminal.states = setdiff(states, c(goal.states, cliff.states))
  actions = list("left", "right", "up", "down", "leftup", "leftdown", "rightup", "rightdown")
  actions = lapply(actions, function(x) {class(x) = x; x})
  
  if (cliff.transition.done) {
    goal.states = c(goal.states, cliff.states)
    m.cliff = NULL
  } else {
    if (!is.null(cliff.states)) {
      if (!is.null(cliff.transition.states)) {
        cliff.pairs = as.matrix(expand.grid(cliff.states, cliff.transition.states))
        cliff.prob = 1 / length(cliff.transition.states)
        m.cliff = cbind(cliff.pairs, cliff.prob)
      } else {
        non.terminal.states = c(non.terminal.states, cliff.states)
        m.cliff = NULL
      }
    }
  }
  
  n.states = length(non.terminal.states)
  new.states = vapply(actions, go, states = non.terminal.states, border.states = border.states, 
    n.col = n.col, FUN.VALUE = numeric(n.states))
  
  if (!is.matrix(new.states)) {
    new.states = as.matrix(new.states, nrow = 1)
  }
  
  m.stoch = matrix(0, nrow = n.states * 8, ncol = 3)
  m.stoch[, 1] = rep(non.terminal.states, 8)
  m.stoch[, 2] = c(new.states)
  m.stoch[, 3] = stochasticity / 8
  
  m.goal = matrix(c(goal.states, goal.states, rep(1, length(goal.states))), ncol = 3)
  m = rbind(m.cliff, m.goal, m.stoch)
  m = m[rep(seq_len(nrow(m)), each = 8), ]
  m = cbind(m, action = rep(1:8, nrow(m) / 8))
  
  new.states = c(apply(new.states, 2, applyWind, states = non.terminal.states, n.col = n.col, wind = wind))
  new.states = getIntoBounds(new.states, n.col = n.col)
  
  m2 = matrix(c(rep(non.terminal.states, 8), new.states, rep(1 - stochasticity, length(new.states)), 
    rep(1:8, each = length(non.terminal.states))), ncol = 4)
  m = rbind(m, m2)
  colnames(m) = c("row", "col", "prob", "action")
  
  m = as.matrix(aggregate(prob ~ row + col + action, data = as.data.frame(m), FUN = "sum"))
  transitions[m[, c("row", "col", "action")]] = m[, "prob"]
  transitions = transitions[, , seq_len(n.actions)]
  
  makeEnvironment(transitions = transitions, rewards = rewards, ...)
}

makeRewardMatrix = function(reward.step, reward.cliff, n.states, n.actions,
  cliff.states, goal.states) {
  rewards = matrix(reward.step, nrow = n.states, ncol = n.actions)
  rewards[cliff.states, ] = reward.cliff
  rewards[goal.states, ] = 0
  rewards
}

go = function(x, ...) {
  UseMethod("go", x)
}

#' @export
go.left = function(x, states, border.states, ...) {
  ifelse(states %in% border.states[["left"]], states, states - 1)
}

#' @export
go.right = function(x, states, border.states, ...) {
  ifelse(states %in% border.states[["right"]], states, states + 1)
}

#' @export
go.up = function(x, states, border.states, n.col) {
  ifelse(states %in% border.states[["up"]], states, states - n.col)
}

#' @export
go.down = function(x, states, border.states, n.col) {
  ifelse(states %in% border.states[["down"]], states, states + n.col)
}

#' @export
go.leftup = function(x, states, border.states, n.col) {
  go.left(x, go.up(x, states, border.states, n.col), border.states)
}

#' @export
go.leftdown = function(x, states, border.states, n.col) {
  go.left(x, go.down(x, states, border.states, n.col), border.states)
}

#' @export
go.rightup = function(x, states, border.states, n.col) {
  go.right(x, go.up(x, states, border.states, n.col), border.states)
}

#' @export
go.rightdown = function(x, states, border.states, n.col) {
  go.right(x, go.down(x, states, border.states, n.col), border.states)
}

applyWind = function(states, new.states, wind, n.col) {
  column = states %% n.col
  column[column == 0] = n.col
  new.states - wind[column] * n.col
}

getIntoBounds = function(new.states, n.col) {
  while (any(new.states <= 0)) {
    new.states[new.states <= 0] = new.states[new.states <= 0] + n.col
  }
  new.states
}
