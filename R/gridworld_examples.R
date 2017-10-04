#' Example Gridworld
#'
#' Simple gridworld environment for reinforcement learning.
#' Grid of shape 4x4 with two goal states in the upper left and
#' bottom right of the grid. Episodes start in any of the other
#' states. Possible actions include going left, right, up and down.
#' 
#' This is the gridworld (goal state denoted G):
#' \tabular{rrrr}{
#'  G \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab . \cr
#'  . \tab . \tab . \tab G \cr
#' }
#' 
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 4.1
#' @return [\code{R6 Class}] \cr
#' Reinforcement Learning environment.
#' @export
#' @examples 
#' env = gridworld()
#' 
gridworld = function() {
  makeGridworld(goal.states = c(0, 15))
}

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
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 6.5
#' @return [\code{R6 Class}] \cr
#' Reinforcement Learning environment.
#' @export
#' @examples 
#' env = windyGridworld()
#' 
windyGridworld = function() {
  makeGridworld(shape = c(7, 10), goal.states = 37, 
    reward.step = - 1, wind = c(0, 0, 0, 1, 1, 1, 2, 2, 1, 0), initial.state = 30)
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
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 6.6
#' @return [\code{R6 Class}] \cr
#' Reinforcement Learning environment.
#' @export
#' @examples 
#' env = cliff()
#' 
cliff = function() {
  makeGridworld(shape = c(4, 12), goal.states = 47, 
    cliff.states = 37:46, reward.step = - 1, reward.cliff = - 100, 
    cliff.transition.states = 36, initial.state = 36)
}
