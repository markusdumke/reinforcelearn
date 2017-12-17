Eligibility = R6::R6Class("Eligibility",
  public = list(
    lambda = 0,
    eligibility.type = NULL,
    E = NULL,
    initialize = function(lambda, traces) {
      self$lambda = lambda
      if (traces == "replace") {
        self$eligibility.type = 1
      } else if (traces == "accumulate") {
        self$eligibility.type = 0
      }


    },
    reset = function(val.fun) {
      self$E = matrix(0, nrow = nrow(val.fun), ncol = ncol(val.fun))
    },
    increase = function(s, a) {
      self$E[s + 1L, a + 1L] = (1 - self$eligibility.type) * self$E[s + 1L, a + 1L] + 1
    },
    decrease = function(discount) {
      self$E = discount * self$lambda * self$E # sarsa
    }
  )
)
