#' Experience Replay
#'
#' Create replay memory for experience replay.
#'
#' @param size \[`integer(1)`] \cr Size of replay memory.
#' @param batch.size \[`integer(1)`] \cr Batch size.
#'
#' @md
#' @aliases experiencereplay
#' @export
makeReplayMemory = function(size = 100L, batch.size = 16L) { # add arguments for priorization
  checkmate::assertInt(size, lower = 1)
  checkmate::assertInt(batch.size, lower = 1, upper = size)
  x = list(size = size, batch.size = batch.size)
  class(x) = "ReplayMemory"
  x
}

ReplayMemory = R6::R6Class("ReplayMemory",
  public = list(
    memory = NULL,
    size = NULL,
    batch.size = NULL,
    index = 0L,
    index.full = 0L,

    # fixme allow growing replay memory?
    initialize = function(size, batch.size) {
      self$size = size
      self$batch.size = batch.size
      self$memory = vector("list", length = self$size)
    },

    # # initialize following policy
    # initializeMemory = function(env, policy) {
    #   for (i in seq_len(self$size)) {
    #     action = policy$sampleAction()
    #     env$step(action)
    #     data = list(state = preprocessState(envir$previous.state), action = action,
    #       reward = envir$reward, next.state = preprocessState(envir$state))
    #   }
    # },

    observe = function(state, action, reward, next.state) {
      self$index = self$index + 1L
      self$index.full = self$index.full + 1L
      self$index.full = min(self$size, self$index.full)
      index = self$getReplacementIndex()
      obs = self$getReplayObservation(state, action, reward, next.state)
      self$add(obs, index)
    },

    getReplayObservation = function(state, action, reward, next.state) {
      list(state = state, action = action, reward = reward, next.state = next.state)
    },

    # e.g. oldest entry
    getReplacementIndex = function() {
      if (self$index > self$size) {
        self$index = 1L
      }
      self$index
    },

    add = function(observation, index) {
      self$memory[[index]] = observation
    },

    isFull = function(memory = self$memory) {
      # maybe it is enough to check the last entry
      full = !(any(purrr::map_lgl(memory, is.null)))
      full
    },

    extract = function(batch, member, fun = lapply) {
      states = fun(batch, "[[", member)
      states
    },

    # checkMemory = function(memory = self$memory, batch.size = self$batch.size) {
    #   if (!self$isFull()) {
    #     if (self$index < batch.size) {
    #       return(FALSE)
    #     }
    #   }
    # },

    sampleBatch = function(memory = self$memory[seq_len(self$index.full)], batch.size = self$batch.size) {
      if (length(memory) >= batch.size) {
        indices = self$getIndices(length(memory), batch.size)
        batch = memory[indices]
        return(purrr::transpose(batch))
      } else {
        message("Cannot sample from replay memory because batch size > number of non-empty entries in replay memory.")
      }
    },

    getIndices = function(memory.size, batch.size) {
      indices = sample(seq_len(memory.size), size = batch.size)
      indices
    }
  )
)

# ideas: maybe replay memory in future not list but hash table / dictionary etc
# data frame with list columns?
# fixme allow dynamic change of replay memory length
# store preprocessed state?
