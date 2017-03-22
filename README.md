# reinforcelearn
Reinforcement Learning in R. Work in progress!

### Installation

```r
# install.packages("devtools")
devtools::install_github("markdumke/reinforcelearn")
```

### Create an environment

With `makeEnvironment` you can create a reinforcement learning environment, either from [OpenAI Gym](https://gym.openai.com/) or from the state transition matrix and reward matrix of a Markov Decision Process. To use a gym environment you need to have python and [gym-http-api](https://github.com/openai/gym-http-api) installed.

```r
library(reinforcelearn)

# Create an environment from an OpenAI Gym environment.
# First set the path to your gym-http-api folder.
options(gym.api.path = "C:/Users/M/Downloads/WinPython-64bit-3.6.0.1Qt5/scripts/gym-http-api")
FrozenLake = makeEnvironment("FrozenLake-v0")
FrozenLake$reset()
FrozenLake$step(action = 0)

# You can also create an environment from an MDP
grid = gridworld$new()
Gridworld1 = makeEnvironment(transition.array = grid$transition.array, 
  reward.matrix = grid$reward.matrix, terminal.states = grid$terminal.states,
  initial.state = grid$initial.state)
```

Environments always have an initialization function to create a new instance of the environment, a reset function, which returns an initial state observation and a step function, which takes a step in the environment given an action returning the next state observation, reward and if the episode is finished.

### Run a reinforcement learning algorithm

After you created an environment you can use various reinforcement learning algorithms. For example, for a tabular environment like  gridworld you can use tabular Q-Learning to solve it and find the optimal action value function Q*. You can set various parameters like the step size alpha, the number of episodes, the discount factor or epsilon, the ratio of random actions sampled by an epsilon-greedy behaviour policy.

```r
res = qlearning(Gridworld1, n.episodes = 10)
print(matrix(apply(res$Q, 1, max), ncol = 4, byrow = TRUE))
```

We can then define the optimal policy by taking the argmax over the action value function Q.

```r
optimal.policy = max.col(Q)
print(matrix(optimal.policy, ncol = 4, byrow = TRUE))
```

Also have a look at the vignettes for further examples.


### Overview about algorithms

| Algorithm                                 |  R function name  | Model | Prediction/Control | Policy/Value-based      | on-policy/off-policy | Return | Comments                       |
|-------------------------------------------|:-----------------:|-------|--------------------|-------------------------|----------------------|--------|--------------------------------|
| Policy Evaluation                         | evaluatePolicy    | yes   | Prediction         | value-based             |                      | V      |                                |
| Policy Iteration                          |      | yes   | Control            | value-based             |                      | V      |                                |
| Value Iteration                           |       | yes   | Control            | value-based             |                      | V      |                                |
| Monte Carlo Prediction                    | predictMC         | no    | Prediction         | value-based             | on                   | V      | first-visit and every-visit MC |
| Monte Carlo Exploring Starts              |       | no    | Control            | value-based             | on                   | Q      |                                |
| GLIE Monte Carlo Control                  |  | no    | Control            | value-based             | on                   | Q      | first-visit MC                 |
| Off-policy Monte Carlo Control            |                   | no    | Control            | value-based             | off                  | Q      | every-visit MC                 |
| TD                                        | td                | no    | Prediction         | value-based             | on                   | V      | lambda version                 |
| SARSA                                     | sarsa             | no    | Control            | value-based             | on                   | Q      | lambda version                 |
| Q-Learning                                | qlearning         | no    | Control            | value-based             | off                  | Q      |                                |
| Double Q-Learning                         |         | no    | Control            | value-based             | off                  | Q      |                                |
| Expected Sarsa                            |      | no    | Control            | value-based             | ?                    | Q      |                                |
| Asynchronous Advantage Actor-Critic (A3C) |                   | no    | Control            | policy- and value-based | ?                    | ?      |                                |

### More about reinforcement learning:
