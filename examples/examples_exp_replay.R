#' ---
#' title: "Experience Replay"
#' author: Markus Dumke
#' output: github_document
#' ---

#+ setup, include=FALSE
library(knitr)
opts_chunk$set(comment = "#>", collapse = FALSE, message = FALSE)

#'
devtools::load_all()

#' replay memory class
replay = ExperienceReplay$new(size = 2, batch.size = 1L)

replay$size
replay$memory

#' add first observation
state = list(runif(3), matrix(1:4, 2))
reward = -1.5
action = 1L
next.state = list(10)

replay$observe(state, action, reward, next.state)
replay$memory

replay$isFull()

replay$sampleBatch(batch.size = 2L)

#' add second observation
state = 7.3
reward = 4.2
action = 0L
next.state = 22

replay$observe(state, action, reward, next.state)
replay$memory

#' sample batch
replay$sampleBatch(batch.size = 1L)
