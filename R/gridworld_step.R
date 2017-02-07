#' Take a step in a gridworld
#'
#' @description Given a state and an action in a gridworld, the next state and reward are returned.
#'
#' @details Possible actions include going left, right, down or up. If an action would take you off
#' the grid, you remain in the previous state. For each step you get a reward of -1, until you reach
#' into a terminal state.
#'
#' @param state scalar integer: the current state
#' @param gridworld object returned by \code{\link{gridworld}}
#' @param action character: the action, one of ("left", "right", "up", "down")
#' @return list with next state, reward and a flag if episode is finished, i.e. the new state is
#' a terminal state
#' @export
#' @examples
#' set.seed(27)
#' grid = gridworld()
#' actions = c("left", "right", "up", "down")
#' # specify the initial state
#' states = 3
#' rewards = numeric(0)
#' sampled.actions = character(0)
#' episode.over = FALSE
#' i = 1
#'
#' while(episode.over == FALSE) {
#'   sampled.actions = append(sampled.actions, sample(actions, size = 1))
#'   step = gridworld_step(sampled.actions[i], states[i], grid)
#'   rewards = append(rewards, step[["reward"]])
#'   states = rbind(states, step[["state"]])
#'   episode.over = step[["episode.over"]]
#'   i = i + 1
#' }
#'
#' print(rewards)
#' print(states)
#'
#' @references Gridworld example from Sutton & Barto, chapter 4
#' @seealso \code{\link{gridworld}}
gridworld_step = function(action, state, gridworld) {
  # Flag if episode is over
  episode.over = FALSE

  # take action -> sample next state and reward
  n.states = nrow(gridworld$reward.matrix)
  states = seq(1, n.states)
  next.state = sample(states, size = 1, prob = gridworld$transition.matrix[state, , action])

  reward = gridworld$reward.matrix[state, action]

  # episode over if terminalState is reached
  if(next.state %in% gridworld$terminal.state) {
    episode.over = TRUE
  }

  return(list(state = next.state, reward = reward, episode.over = episode.over))
}
