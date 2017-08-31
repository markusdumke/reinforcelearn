#' Gridworld
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
#' @format [\code{list(2)}] \cr 
#'   List with transition array and reward matrix.
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction Example 4.1
#' @name gridworld
NULL
#' @export
