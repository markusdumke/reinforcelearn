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
#' @format [\code{list(2)}] \cr 
#'   List with transition array and reward matrix.
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 6.6
#' @name cliff
NULL
#' @export
