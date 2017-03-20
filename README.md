# reinforcelearn
Reinforcement Learning in R. Work in progress!

### Installation

```r
if(!require(devtools)) install.packages("devtools")
devtools::install_github("markdumke/reinforcelearn")
```

 ### Overview

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
