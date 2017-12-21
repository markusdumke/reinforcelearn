#' Create reinforcement learning environment.
#'
#' This function creates an environment for reinforcement learning.
#'
#' Use the `step` method to interact with the environment.
#'
#' Note that all states and actions are numerated starting with 0!
#'
#' For a detailed explanation and more examples
#' have a look at the vignette "How to create an environment?".
#'
#' @param class \[`character(1)`] \cr
#'   Class of environment. One of `c("custom", "mdp", "gym", "gridworld")`.
#' @param discount \[`numeric(1)` in (0, 1)] \cr Discount factor.
#' @param ... \[`any`] \cr Arguments passed on to the specific environment.
#'
#' @return \[`Environment`] \cr
#'   Reinforcement learning environment (R6 class).
#'
#' @md
#'
#' @section Methods:
#' * `$step(action)` \cr
#'   Take action in environment.
#'   Returns a list with `state`, `reward`, `done`.
#' * `$reset()` \cr
#'   Resets the `done` flag of the environment and returns an initial state.
#'   Useful when starting a new episode.
#' * `$visualize()` \cr
#'   Visualizes the environment (if there is a visualization function).
#'
#' @section Environments:
#' * [Environment]
#' * [GymEnvironment]
#' * [MdpEnvironment]
#' * [Gridworld]
#' * [MountainCar]
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
#' reset = function(self) {
#'   state = list(mean = 0, sd = 1)
#'   state
#' }
#'
#' env = makeEnvironment(step = step, reset = reset, discount = 0.9)
#' env$reset()
#' env$step(100)
#'
#' # Create a Markov Decision Process.
#' P = array(0, c(2, 2, 2))
#' P[, , 1] = matrix(c(0.5, 0.5, 0, 1), 2, 2, byrow = TRUE)
#' P[, , 2] = matrix(c(0, 1, 0, 1), 2, 2, byrow = TRUE)
#' R = matrix(c(5, 10, -1, 2), 2, 2, byrow = TRUE)
#' env = makeEnvironment("mdp", transitions = P, rewards = R)
#'
#' env$reset()
#' env$step(1L)
#'
#' # Create a Gridworld.
#' grid = makeEnvironment("gridworld", shape = c(4, 4),
#'   goal.states = 15, initial.state = 0)
#' grid$visualize()
#'
#' \dontrun{
#' # Create an OpenAI Gym environment.
#' # Make sure you have Python, gym and reticulate installed.
#' env = makeEnvironment("gym", "MountainCar-v0")
#'
#' # Take random actions for 200 steps.
#' env$reset()
#' for (i in 1:200) {
#'   action = sample(env$actions, 1)
#'   env$step(action)
#'   env$visualize()
#' }
#' env$close()
#' }
makeEnvironment = function(class = "custom", discount = 1, ...) {
  checkmate::assertChoice(class,
    c("custom", "mdp", "gym", "gridworld", "windy.gridworld", "cliff.walking",
      "mountain.car", "mountain.car.continuous"))
  switch(class,
    custom = Environment$new(discount = discount, ...), # default
    mdp = MdpEnvironment$new(discount = discount, ...),
    gym = GymEnvironment$new(discount = discount, ...),
    gridworld = Gridworld$new(discount = discount, ...),
    windy.gridworld = WindyGridworld$new(discount = discount, ...),
    cliff.walking = CliffWalking$new(discount = discount, ...),
    mountain.car = MountainCar$new(discount = discount, ...),
    mountain.car.continuous = MountainCarContinuous$new(discount = discount, ...)
  )
}

#' Custom Reinforcement Learning Environment
#'
#' @section Usage:
#' `makeEnvironment("custom", step, reset, visualize = NULL, discount = 1, action.names = NULL)`
#'
#' @param step \[`function(self, action)`] \cr
#'   Custom step function.
#' @param reset \[`function(self)`] \cr
#'   Custom reset function.
#' @param visualize \[`function(self)`] \cr
#'   Optional custom visualization function.
#' @param discount \[`numeric(1)` in (0, 1)] \cr Discount factor.
#' @param action.names \[`named integer`] \cr
#'   Optional action names for a discrete action space.
#'
#' @md
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
#' reset = function(self) {
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
    action.names = NULL,
    n.step = 0L,
    episode = 0L,
    episode.step = 0L,
    episode.return = 0,
    previous.state = NULL,
    state = NULL,
    reward = NULL,
    done = FALSE,
    discount = NULL,

    resetEverything = function() {
      self$n.step = 0L
      self$episode = 0
      self$reset()
    },

    reset = function() {
      self$episode.step = 0L
      self$episode.return = 0
      self$done = FALSE
      self$state = private$reset_(self)
      self$state
    },

    step = function(action) {
      if (is.character(action)) {
        action = self$action.names[action]
      }
      self$previous.state = self$state
      res = private$step_(self, action)
      self$episode.return = self$episode.return +
        self$discount ^ self$episode.step * res[[2]]
      self$n.step = self$n.step + 1L
      self$episode.step = self$episode.step + 1L
      self$state = res[[1]]
      self$reward = res[[2]]
      self$done = res[[3]]
      if (self$done) {
        self$episode = self$episode + 1L
      }
      list(state = res[[1]], reward = res[[2]], done = res[[3]])
    },

    visualize = function() {
      private$visualize_(self)
    },

    initialize = function(step, reset, visualize = NULL, discount, action.names = NULL) {
      checkmate::assertFunction(step)
      checkmate::assertFunction(reset)
      checkmate::assertFunction(visualize, null.ok = TRUE)
      checkmate::assertNumber(discount, lower = 0, upper = 1)
      checkmate::assertIntegerish(action.names, null.ok = TRUE)

      private$step_ = step
      private$reset_ = reset
      self$discount = discount
      self$action.names = action.names
      if (!missing(visualize)) {
        checkmate::assertFunction(visualize)
        private$visualize_ = visualize
      } else {
        private$visualize_ = function(self) {}
      }
      self$reset()
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
