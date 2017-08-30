#' Windy Gridworld
#'
#' Windy Gridworld problem for reinforcement learning. Actions include
#' going left, right, up and down. In each column the wind pushes you up a
#' specific number of steps (for the next action). If an action would
#' take you off the grid, you remain in the previous state. For each step you
#' get a reward of -1, until you reach into a terminal state.
#' @format [\code{list(2)}] \cr
#'   List with the transition array and reward matrix.
#' @references Sutton and Barto (Book draft 2017): Reinforcement Learning: An Introduction
#' @name windy.gridworld
NULL
#' @export
