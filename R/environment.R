#' Make Reinforcement Learning Environment
#'
#' This function creates an environment for reinforcement learning.
#'
#' Use the \code{step} method to interact with the environment.
#'
#' Note that all states and actions are numerated starting with 0!
#'
#' For a detailed explanation and more examples
#' have a look at the vignette "How to create an environment?".
#'
#' @param type [\code{character(1)}] \cr
#'   Type of environment. There are several prebuild environments.
#'   See also here Section Environments.
#' @param ... [\code{any}] \cr Arguments passed on to the specific environment.
#'
#' @return [\code{R6 class}] \cr
#'   Reinforcement Learning Environment.
#'
#' @section Methods:
#' \describe{
#' \item{\code{$step(action)}}{
#' Take action in environment.
#' Returns a list with \code{state}, \code{reward}, \code{done}.
#'  }
#' \item{\code{$reset()}}{
#'   Resets the \code{done} flag of the environment and returns an initial state.
#'   Useful when starting a new episode.
#' }
#' \item{\code{$visualize()}}{
#'   Visualizes the environment (if there is a visualization function).
#' }
#' }
#'
#' @section Environments:
#' \itemize{
#'   \item{\code{\link{Environment}}}
#'   \item{\code{\link{GymEnvironment}}}
#'   \item{\code{\link{MdpEnvironment}}}
#'   \item{\code{\link{Gridworld}}}
#'   \item{\code{\link{MountainCar}}}
#' }
#'
#' @section Environments:
#'
#'
#' @export
#' @examples
#' step = function(self, action) {
#'   state = list(mean = action + rnorm(1), sd = runif(1))
#'   reward = rnorm(1, state[[1]], state[[2]])
#'   done = FALSE
#'   list(state, reward, done)
#' }
#'
#' reset = function() {
#'   state = list(mean = 0, sd = 1)
#'   state
#' }
#'
#' env = makeEnvironment(step = step, reset = reset)
#' env$reset()
#' env$step(100)
#'
#' # Create a Markov Decision Process.
#' P = array(0, c(2, 2, 2))
#' P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
#' P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
#' R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
#' env = makeEnvironment("MDP", transitions = P, rewards = R)
#'
#' env$reset()
#' env$step(1L)
#'
#' # Create a Gridworld.
#' grid = makeEnvironment("Gridworld", shape = c(4, 4),
#'   goal.states = 15, initial.state = 0)
#'
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python, gym and reticulate installed.
#' env = makeEnvironment("Gym", "MountainCar-v0")
#'
#' # Take random actions for 200 steps.
#' env$reset()
#' for (i in 1:200) {
#'   action = sample(env$actions, 1)
#'   env$step(action)
#' }
#' env$close()
#' }
makeEnvironment = function(type = "custom", ...) {
  checkmate::assertChoice(type,
    c("custom", "MDP", "Gym", "Gridworld", "WindyGridworld", "CliffWalking",
      "MountainCar", "MountainCarContinuous"))
  switch(type,
    custom = Environment$new(...), # default
    MDP = MdpEnvironment$new(...),
    Gym = GymEnvironment$new(...),
    Gridworld = Gridworld$new(...),
    WindyGridworld = WindyGridworld$new(...),
    CliffWalking = CliffWalking$new(...),
    MountainCar = MountainCar$new(...),
    MountainCarContinuous = MountainCarContinuous$new(...)
    )
}

#' Custom Reinforcement Learning Environment
#'
#' @section Usage:
#' \code{makeEnvironment("custom", step, reset, visualize)}
#'
#' @param step [\code{function(self, action)}] \cr
#'   Custom step function.
#' @param reset [\code{function()}] \cr
#'   Custom reset function.
#' @param visualize [\code{function(self)}] \cr
#'   Optional custom visualization function.
#'
#' @inheritSection makeEnvironment Methods
#'
#' @name Environment
#' @export
#'
#' @examples
#' step = function(self, action) {
#'   state = list(mean = action + rnorm(1), sd = runif(1))
#'   reward = rnorm(1, state[[1]], state[[2]])
#'   done = FALSE
#'   list(state, reward, done)
#' }
#'
#' reset = function() {
#'   state = list(mean = 0, sd = 1)
#'   state
#' }
#'
#' env = makeEnvironment(step = step, reset = reset)
#' env$reset()
#' env$step(100)
NULL

Environment = R6::R6Class("Environment",
  public = list(
    n.step = 0L,
    episode = 0L,
    episode.step = 0L,
    episode.return = 0,
    previous.state = NULL,
    state = NULL,
    reward = NULL,
    done = FALSE,
    # discount = NULL,

    reset = function() {
      self$episode.step = 0L
      self$episode.return = 0
      self$done = FALSE
      self$state = private$reset_()
      self$state
    },

    step = function(action) {
      self$previous.state = self$state
      self$n.step = self$n.step + 1L
      self$episode.step = self$episode.step + 1L
      res = private$step_(self, action)
      self$episode.return = self$episode.return +
        self$discount ^ self$episode.step * res[[2]]
      self$state = res[[1]]
      self$reward = res[[2]]
      self$done = res[[3]]
      if (self$done) {
        self$episode = self$episode + 1L
      }
      list(state = res[[1]], reward = res[[2]], done = res[[3]])
    },

    # an optional visualization function
    visualize = function() {
      private$visualize_(self)
    },

    initialize = function(step, reset, visualize) { # , discount = 1
      checkmate::assertFunction(step) # nargs ?
      checkmate::assertFunction(reset)

      private$step_ = step
      private$reset_ = reset
      # self$discount = discount
      if (!missing(visualize)) {
        checkmate::assertFunction(visualize)
        private$visualize_ = visualize
      } else {
        private$visualize_ = function() {}
      }
    }
  ),

  private = list(
    # step_: custom step method depending on problem that returns list with
    #   next state, reward, done
    step_ = NULL,
    # reset_: custom reset method depending on problem that returns state
    reset_ = NULL,
    visualize_ = NULL
  )
)
