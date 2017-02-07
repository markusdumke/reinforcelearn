#' Gridworld environment
#'
#' @description Simple gridworld for reinforcement learning.
#'
#' @details Possible actions include going left, right, down or up. If an action would take you off
#' the grid, you remain in the previous state. For each step you get a reward of -1, until you reach
#' into a terminal state.
#'
#' @param shape length-two integer vector: the shape of the grid, e.g. (4, 4)
#' @param terminal.states integer vector of terminal states
#' @return list with a 3-dimensional array containing the probability transition matrices
#'  for each action and a reward matrix (states x actions)
#' @export
#' @details The states are enumerated as follows (example 4x4 grid):
#' \tabular{rrrr}{
#'  1 \tab 2 \tab 3 \tab 4 \cr
#'  5 \tab 6 \tab 7 \tab 8 \cr
#'  9 \tab 10 \tab 11 \tab 12 \cr
#'  13 \tab 14 \tab 15 \tab 16 \cr
#' }
#' So a board position could look like this (T: terminal state, x: current state):
#' \tabular{rrrr}{
#'  T \tab o \tab o \tab o \cr
#'  o \tab o \tab o \tab o \cr
#'  o \tab x \tab o \tab o \cr
#'  o \tab o \tab o \tab T \cr
#' }
#'
#' @examples
#' # Create 3x5 gridworld with one terminal state in the top right corner of the grid.
#' gridworld(shape = c(3, 5), terminal.states = 5)
#' @references Gridworld example from Sutton & Barto, chapter 4
#' @seealso \code{\link{gridworld_step}}
gridworld <- function(shape = c(4, 4), terminal.states = c(1, 16)) {

  n.states = prod(shape)

  actions = c("left", "right", "up", "down")
  n.actions = length(actions)

  # make probability transition matrix for each action (3-dimensional array)
  P = array(matrix(0, nrow = n.states, ncol = n.states),
            dim = c(n.states, n.states, n.actions),
            dimnames = list(NULL, NULL, actions))

  # fill in probabilities: when action is taking you off the grid,
  # the new state will be the same as the old state
  for(state in seq_len(n.states)) {
    for(action in actions) {

      if(action == "left") {
        border.states = seq(1, n.states, shape[2])
        new.state = ifelse(state %in% border.states, state, state - 1)
      }

      if(action == "right") {
        border.states = seq(shape[2], n.states, shape[2])
        new.state = ifelse(state %in% border.states, state, state + 1)
      }

      if(action == "up") {
        border.states = seq(1, shape[2])
        new.state = ifelse(state %in% border.states, state, state - 4)
      }

      if(action == "down") {
        border.states = seq(n.states - shape[2] + 1, n.states)
        new.state = ifelse(state %in% border.states, state, state + 4)
      }
      P[state, new.state, action] = 1
    }
  }

  # reward matrix: 3-dimensional array depending on actions and states
  # reward of - 1 for each step
  rewards = matrix(- 1, nrow = n.states, ncol = n.actions, dimnames = list(NULL, actions))

  # set rewards of terminal states to 0
  rewards[terminal.states, ] = 0

  return(list(transition.matrix = P, reward.matrix = rewards, terminal.states = terminal.states))
}
