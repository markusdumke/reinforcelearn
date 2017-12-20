## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(message = TRUE, eval = TRUE, collapse = TRUE, comment = "#>")

## ------------------------------------------------------------------------
library(reinforcelearn)

## ------------------------------------------------------------------------
# Uniform random policy
policy = makePolicy("random")

# Epsilon-greedy policy
policy = makePolicy("epsilon.greedy", epsilon = 0.2)

# Softmax policy
policy = makePolicy("softmax")

## ------------------------------------------------------------------------
alg = makeAlgorithm("qlearning", lambda = 0.8, traces = "accumulate")

## ------------------------------------------------------------------------


