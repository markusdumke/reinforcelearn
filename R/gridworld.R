#' @title Gridworld environment
#'
#' @description Simple gridworld for reinforcement learning. Given a state and an action,
#' the next state and reward are returned.
#'
#' @details Possible actions include going left, right, down or up. If an action would take you off
#' the grid, you remain in the previous state. For each step you get a reward of -1, until you reach
#' into a terminal state.
#'
#' @param state the current state, e.g. c(1, 3)
#' @param action the action, one of ("left", "right", "up", "down")
#' @param shape the shape of the grid, e.g. (4, 4)
#' @param terminalStates list of terminal states, each list element is a length two vector
#' @return list with next state, reward and a flag if episode is finished, i.e. the new state is
#' a terminal state
#'
#' @examples
#' actions = c("left", "right", "up", "down")
#' states = matrix(c(3, 3), nrow = 1, ncol = 2)
#' rewards = numeric(0)
#' sampledActions = character(0)
#' episodeOver = FALSE
#' i = 1
#'
#' while(episodeOver == FALSE) {
#'   sampledActions = append(sampledActions, sample(actions, size = 1))
#'   i = i + 1
#'   step = gridworld(states[i - 1, ], sampledActions[i - 1])
#'   rewards = append(rewards, step[["reward"]])
#'   states = rbind(states, step[["state"]])
#'   episodeOver = step[["episodeOver"]]
#' }
#'
#' print(sampledActions)
#' print(rewards)
#' print(states)
#'
#' @references Gridworld example from Sutton & Barto, chapter 4
gridworld <- function(state, action, shape = c(4, 4), terminalStates = list(c(1, 1), c(4, 4))) {

  terminalStates = matrix(unlist(terminalStates), ncol = 2, byrow = TRUE)
  # n_states = prod(shape)
  # n_actions = 4

  # possible actions:
  up = c(-1, 0)
  down = c(1, 0)
  right = c(0, 1)
  left = c(0, -1)

  # Flag if episode is over
  episodeOver = FALSE

  # make action
  if(action == "left"){
    nextState = state + left
  } else if(action == "right"){
    nextState = state + right
  } else if(action == "up"){
    nextState = state + up
  } else if(action == "down"){
    nextState = state + down
  }

  # check if new state is inside grid else set new state to old state
  states_inside_grid = as.matrix(expand.grid(seq(1, shape[1]), seq(1, shape[2])))
  if(!duplicated(rbind(states_inside_grid, matrix(nextState, ncol = 2)))[-seq_len(nrow(states_inside_grid))]) {
    nextState = state
  }

  # episode over if terminalState is reached
  if(duplicated(rbind(terminalStates, matrix(nextState, ncol = 2)))[-seq_len(nrow(terminalStates))]) {
    episodeOver = TRUE
  }

  reward = -1

  return(list(state = nextState, reward = reward, episodeOver = episodeOver))
}
