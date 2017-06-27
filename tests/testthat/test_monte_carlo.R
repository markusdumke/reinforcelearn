# library(reinforcelearn)
# context("Monte Carlo Methods")
# 
# set.seed(1477)
# grid = makeGridworld()
# 
# # Define random policy
# n.states = nrow(grid$reward.matrix)
# n.actions = ncol(grid$reward.matrix)
# random.policy = matrix(1 / n.actions, nrow = n.states, ncol = n.actions)
# 
# # Compare results with expected result
# v.expected = c(NA, -14, -20, -22, -14, -18, -20, -20,
#   -20, -20, -18, -14, -22, -20, -14, NA)
# 
# test_that("Estimate return works for different discount.factors", {
#   rewards = c(3, 4, 2)
#   G = estimateReturn(rewards, discount.factor = 1)
#   expect_equal(G, sum(rewards))
#   
#   G = estimateReturn(rewards, discount.factor = 0.5)
#   expect_equal(G, 5.5)
#   
#   G = estimateReturn(rewards, discount.factor = 0.1)
#   expect_equal(G, 3.42)
# })
# 
# # test_that("Sample episode works", {
# #   grid = gridworld$new(shape = c(1, 2), terminal.states = 1)
# #   episode = sampleEpisode(random.policy, grid, initial.state = 2,
# #     initial.action = "right")
# #   expect_equal(episode, )
# # })
# 
# test_that("make greedy policy works", {
#   Q = matrix(c(1, 3, 2, 4), ncol = 2)
#   greedy = make_greedy_policy(Q)
#   expect_equal(greedy, matrix(c(0, 0, 1, 1), ncol = 2))
# })
# 
# # test_that("Test that Monte Carlo first-visit Prediction works", {
# #   v = predictMC(random.policy, grid, n.episodes = 10000, method = "first-visit")
# #   expect_equal(v, v.expected, tolerance = 0.5)
# # })
# # 
# # test_that("Test that Monte Carlo every-visit Prediction works", {
# #   v = predictMC(random.policy, grid, n.episodes = 10000, method = "every-visit")
# #   expect_equal(v, v.expected, tolerance = 0.5)
# # })
# # 
# # test_that("Test that discount.factor works", {
# #   # v = predictMC(random.policy, grid, n.episodes = 10000,
# #   #   method = "every-visit", discount.factor = 0.5)
# #   # expect_equal(v, v.expected, tolerance = 0.5)
# # })
# # 
# # test_that("Test that learning.rate works", {
# # #   v = predictMC(random.policy, grid, n.episodes = 10000, method = "every-visit",
# # #     learning.rate = 0.01)
# # #   expect_equal(v, v.expected, tolerance = 0.5)
# # })
# # 
# # test_that("Estimation of return via MC first visit works for one state-action pair", {
# #   states = c(1, 4, 6, 6, 4)
# #   actions = c("action1", "action2", "action1", "action3")
# #   rewards = c(4, 8, 1, 0)
# #   
# #   G = estimate_return_firstvisit_one_state_action_pair(states[-length(states)], actions, rewards, 
# #     state = 1, action = "action1", discount.factor = 1)
# #   expect_equal(G, 13)
# # })
