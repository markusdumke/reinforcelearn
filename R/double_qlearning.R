#' #' Double Q-Learning (Table-lookup)
#' #' 
#' #' Off-policy TD control algorithm. Double Q-Learning finds the optimal
#' #' action value function Q independent of the policy followed.
#' #' The idea behind Double Q-Learning is to have two separate action value 
#' #' functions Q1 and Q2. Actions are chosen from an epsilon-greedy policy derived 
#' #' from Q1 + Q2. With equal probability either Q1 or Q2 is then updated 
#' #' following the same update rule as in Q-Learning. This avoids maximization bias.
#' #'
#' #' Under the assumption that all state-action pairs are visited (which is 
#' #' achieved using a stochastic epsilon-greedy policy) Double Q-Learning converges to 
#' #' the optimal action value function Q*.
#' #' The update formulas are: 
#' #' \deqn{Q1(S, A) <- Q1(S, A) + \alpha [R + \gamma Q2(S', argmax_a Q1(S', a)) - Q1(S, A)]}
#' #' \deqn{Q2(S, A) <- Q2(S, A) + \alpha [R + \gamma Q1(S', argmax_a Q2(S', a)) - Q2(S, A)]}
#' #' 
#' #' @inheritParams qlearning
#' #' 
#' #' @return optimal action value function
#' #' @seealso [qlearning]
#' #' @references Sutton and Barto (Book draft 2016): Reinforcement Learning: An Introduction
#' #' @export
#' #' @examples
#' #' # Solve the WindyGridworld environment using Q-Learning
#' #' windygrid = WindyGridworld$new()
#' #' WindyGridworld1 = makeEnvironment(transition.array = windygrid$transition.array,
#' #'   reward.matrix = windygrid$reward.matrix,
#' #'   terminal.states = windygrid$terminal.states,
#' #'   initial.state = 30)
#' #' # res = doubleqlearning(WindyGridworld1, n.episodes = 100, seed = 123)
#' #' 
#' doubleqlearning <- function(envir, n.episodes = 10, learning.rate = 0.1, 
#'   epsilon = 0.1, epsilon.decay = 0.5, discount.factor = 1, 
#'   seed = NULL) {
#'   
#'   # input checking
#'   stopifnot(envir$state.space == "Discrete")
#'   if (!is.null(seed)) set.seed(seed)
#'   
#'   n.states = envir$n.states
#'   n.actions = envir$n.actions
#'   Q1 = matrix(0, nrow = n.states, ncol = n.actions)
#'   Q2 = matrix(0, nrow = n.states, ncol = n.actions)
#'   steps.per.episode = rep(0, n.episodes)
#'   rewards.per.episode = rep(0, n.episodes)
#'   
#'   for (i in seq_len(n.episodes)) {
#'     envir$reset()
#'     state = envir$state
#'     j = 0
#'     reward.sum = 0
#'     
#'     while (envir$episode.over == FALSE) {
#'       action = sample_epsilon_greedy_action(Q1[state, ] + Q2[state, ], epsilon)
#'       envir$step(action)
#'       next.state = envir$state
#'       reward = envir$reward
#'       reward.sum = reward.sum + reward
#'       
#'       which_q <- sample(c("Q1", "Q2"), size = 1)
#'       if (which_q == "Q1") {
#'         TD.target = reward + discount.factor * Q2[next.state + 1, argmax(Q1[next.state, ])]
#'         TD.error = TD.target - Q1[state + 1, action + 1] 
#'         Q1[state + 1, action + 1] = Q1[state + 1, action + 1] + learning.rate * TD.error
#'       } else {
#'         TD.target = reward + discount.factor * Q1[next.state + 1, argmax(Q2[next.state, ])]
#'         TD.error = TD.target - Q2[state + 1, action + 1] 
#'         Q2[state + 1, action + 1] = Q2[state + 1, action + 1] + learning.rate * TD.error
#'       }
#'       
#'       state = next.state
#'       j = j + 1
#'       
#'       if (envir$episode.over) {
#'         steps.per.episode[i] = j
#'         rewards.per.episode[i] = reward.sum
#'         print(paste("Episode", i, "finished after", j, "time steps."))
#'         if (i %% 100 == 0) {
#'           epsilon = epsilon.decay * epsilon
#'           print(paste("Average Reward of last 100 episodes:", sum(rewards.per.episode[seq(i - 99, i)]) / 100))
#'         }
#'         break
#'       } 
#'     }
#'   }
#'   
#'   list(Q1 = Q1, Q2 = Q2, steps.per.episode = steps.per.episode, 
#'     rewards.per.episode = rewards.per.episode)
#' }
