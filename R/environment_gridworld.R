#' Gridworld
#'
#' \code{Gridworld} creates gridworld environments.
#'
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
#' @section Usage:
#' \code{Gridworld$new(shape = NULL, goal.states = NULL, cliff.states = NULL,
#'   reward.step = -1, reward.cliff = -100, diagonal.moves = FALSE, wind = rep(0, shape[2]),
#'   cliff.transition.states = NULL, cliff.transition.done = FALSE, stochasticity = 0, ...)}
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
#'   Arguments passed on to \code{\link{MdpEnvironment}}.
#'
#' @name Gridworld
#' @inheritSection Environment Methods
#' @export
#'
#' @examples
#' # Gridworld Environment (Sutton & Barto Example 4.1)
#' env1 = Gridworld$new(shape = c(4, 4), goal.states = c(0, 15))
#'
#' # Windy Gridworld (Sutton & Barto Example 6.5)
#' env2 = Gridworld$new(shape = c(7, 10), goal.states = 37,
#'   reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0),
#'   initial.state = 30)
#'
#' # Cliff Walking (Sutton & Barto Example 6.6)
#' env3 = Gridworld$new(shape = c(4, 12), goal.states = 47,
#'   cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100,
#'   cliff.transition.states = 36, initial.state = 36)
NULL

#' @export
Gridworld = R6::R6Class("Gridworld",
  inherit = MdpEnvironment,

  public = list(

    initialize = function(shape = NULL, goal.states = NULL, cliff.states = NULL,
      reward.step = -1, reward.cliff = -100, diagonal.moves = FALSE, wind = rep(0, shape[2]),
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
      if (any(goal.states > prod(shape)) || any(cliff.states > prod(shape)) |
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

      m.cliff = NULL
      if (cliff.transition.done) {
        goal.states = c(goal.states, cliff.states)
      } else {
        if (!is.null(cliff.states)) {
          if (!is.null(cliff.transition.states)) {
            cliff.pairs = as.matrix(expand.grid(cliff.states, cliff.transition.states))
            cliff.prob = 1 / length(cliff.transition.states)
            m.cliff = cbind(cliff.pairs, cliff.prob)
          } else {
            non.terminal.states = c(non.terminal.states, cliff.states)
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

      visualize = function(env) {
        one.row = paste(rep("-", shape[2]), collapse = " ")
        grid.vis = paste("", one.row, collapse = "")
        for (i in seq_len(shape[1] - 1)) {
          grid.vis = paste(grid.vis, "\n", one.row)
        }

        n = env$state + 1
        # find position of nth -
        str.pos = gregexpr("-", grid.vis)[[1]][n]
        # replace nth - with o (current state in grid)
        grid.vis = sub(paste0("^(.{", str.pos - 1, "})(.)(.*$)", collapse = ""),
          "\\1o\\3", grid.vis)
        cat(grid.vis, "\n", "\n")
        ## some unsuccessful tries for the same thing
        # str_replace(grid.vis, "(.-[^-]*){7}", "\\o")
        # sub("^((?:[^-]*-){2}).*", "\\o", grid.vis)
        # pat <- paste0("^((?:.*?-){", n - 1, "}.*?)-")
        # pat <- paste0("(.?-.?){", n - 1, "}")
        # sub("(.?-.?)", " o ", grid.vis)#, perl = TRUE)
      }
      super$initialize(transitions = transitions, rewards = rewards, visualize = visualize, ...)
    }
  )
)

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

#' Cliff Walking
#'
#' Gridworld environment for reinforcement learning from Sutton & Barto (2017).
#' Grid of shape 4x12 with a goal state in the bottom right of the grid.
#' Episodes start in the lower left state. Possible actions include going left, right, up and down.
#' Some states in the lower part of the grid are a cliff,
#' so taking a step into this cliff will yield a high negative reward of - 100 and move the agent
#' back to the starting state.
#' Elsewise rewards are - 1, for the goal state 0.
#'
#' This is the gridworld (goal state denoted G, cliff states denoted C, start state denoted S):
#' \tabular{rrrrrrrrrrrr}{
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  S \tab C \tab C \tab C \tab C \tab C \tab C \tab C \tab C \tab C \tab C \tab G \cr
#' }
#'
#' @section Usage:
#' \code{CliffWalking$new()}
#'
#' @name CliffWalking
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 6.6
#' @export
#' @inheritSection Environment Methods
#' @examples
#' env = CliffWalking$new()
NULL

#' @export
CliffWalking = R6::R6Class("CliffWalking",
  inherit = Gridworld,
  public = list(
    initialize = function() {
      super$initialize(shape = c(4, 12), goal.states = 47,
        cliff.states = 37:46, reward.step = -1, reward.cliff = -100,
        cliff.transition.states = 36, initial.state = 36)
    }
  )
)

#' Windy Gridworld
#'
#' Windy Gridworld problem for reinforcement learning. Actions include
#' going left, right, up and down. In each column the wind pushes you up a
#' specific number of steps (for the next action). If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#'
#' This is the gridworld (goal state denoted G, start state denoted S).
#' The last row specifies the upward wind in each column.
#' \tabular{rrrrrrrrrr}{
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  S \tab . \tab . \tab . \tab . \tab . \tab . \tab G \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \tab . \cr
#'  0 \tab 0 \tab 0 \tab 1 \tab 1 \tab 1 \tab 2 \tab 2 \tab 1 \tab 0 \cr
#' }
#'
#' @section Usage:
#' \code{WindyGridworld$new()}
#'
#' @name WindyGridworld
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 6.5
#' @export
#' @inheritSection Environment Methods
#' @examples
#' env = WindyGridworld$new()
NULL

#' @export
WindyGridworld = R6::R6Class("WindyGridworld",
  inherit = Gridworld,
  public = list(
    initialize = function() {
      super$initialize(shape = c(7, 10), goal.states = 37,
        reward.step = -1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0),
        initial.state = 30)
    }
  )
)

# fixme: add character actions (e.g. "left")
