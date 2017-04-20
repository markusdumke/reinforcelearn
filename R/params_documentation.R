#' Documentation of all parameters
#' 
#' @param policy numeric matrix: a policy specified as a probability
#'   matrix (states x actions)
#' @param envir an R6 class: the reinforcement learning environment
#'   created by [makeEnvironment].
#' @param discount.factor scalar numeric, discounting future rewards
#' @param precision scalar numeric, algorithm stops when improvement is
#'   smaller than precision
#' @param lambda scalar numeric in (0, 1): Then lambda = 0 only
#'   current state is updated (this is equivalent to TD(0)), for
#'   lambda = 1 all states visited are updated, this is roughly
#'   equivalent to every-visit Monte Carlo.
#' @param bandit an R6 class: bandit problem
#' @param n.episodes scalar integer: number of episodes
#' @param action.selection scalar character: which method to use for 
#'   action selection, e.g. "epsilon-greedy", "greedy" or "UCB"
#' @param epsilon scalar numeric: ratio of random exploration in 
#'   epsilon-greedy action selection
#' @param initial.value scalar numeric: initial values for the action 
#'   values Q, set this to the maximal possible reward to encourage
#'   exploration (optimistic initialization)
#' @param initial.visits scalar integer: set this to a high number to 
#'   encourage exploration (together with a high initial.value)
#' @param epsilon.decay scalar numeric between 0 and 1: decay epsilon 
#'   by this factor
#' @param epsilon.decay.after scalar integer: number of episodes afer 
#'   which to decay epsilon
#' @param C scalar numeric: controls the degree of exploration. High C
#'   values lead to more exploration
#' @param seed scalar integer: random seed
#' @param method scalar character: Monte Carlo first-visit or
#'   every-visit method
#' @param learning.rate scalar numeric between 0 and 1: learning rate
#' @param n.steps integer scalar: number of evaluations (steps in the
#'   environment)
#' @param preprocessState function: takes a state observation as input
#'   and returns a preprocessed state, e.g. a one-hot vector
#' @param predict function: predict returns vector of q values for a 
#'   given preprocessed state observation
#' @param predict2 function: predict function for the target network
#' @param copy function: copy model parameters to target network
#' @param train function: train the model, update the weights
#' @param ... arguments passed on to preprocessState, predict or train
#' @param experience.replay logical scalar
#' @param replay.memory list: each list entry is a list with entries 
#'   state, action, reward, next.state. replay.memory might be filled 
#'   with experience sampled from a random policy.
#' @param replay.memory.size integer scalar: size of the replay memory
#' @param initial.replay.memory.size integer scalar: how much of the
#'   replay memory is filled initially
#' @param batch.size scalar integer: batch size, how many samples are 
#'   drawn from the replay memory. Must be smaller than size of the
#'   replay memory!
#' @param alpha positive scalar numeric: If alpha = 0 sampling 
#'   from replay memory will be uniform, otherwise observations with
#'   high td error will be prioritized.
#' @param theta positive scalar numeric: theta is a small positive 
#'   constant that prevents the edge-case of transitions not being 
#'   revisited once their error is zero. 
#' @param frozen.target scalar logical: Q-Learning with frozen target
#'   network
#' @param update.target.after scalar integer: copy parameters to fixed
#'   target network every n steps
#' @param double.qlearning logical scalar: whether to use double 
#'   qlearning
#' 
params = function(policy, envir, bandit, discount.factor, precision, lambda, epsilon, 
  epsilon.decay, epsilon.decay.after, seed, method, n.steps, n.episodes, 
  initial.value, initial.visits, C, action.selection, learning.rate, 
  preprocessState, predict, predict2, copy, train, ..., experience.replay, replay.memory, 
  replay.memory.size, initial.replay.memory.size, batch.size, alpha, theta,
  frozen.target, update.target.after, double.qlearning) {
  
}
