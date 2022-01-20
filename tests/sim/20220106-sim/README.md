### Details about simulation

1. Objective function is weighted by inverse of the noise scale parameter.
1. \epsilon budget choose such that
    a. higher weight in more aggregated margins
    a. ratio of largest to smallest is 4 times.

This simulation has:

$$ \epsilon = 5 $$

and $\epsilon$ schedule,

|                          | State | County | Tract | [Total] |
|--------------------------|-------|--------|-------|---------|
| detailed                 |       |        |       | .2      |
| Own/rent                 |       |        |       | .25     |
| VoteAge x Hisp x CenRace |       |        |       | .25     |
| Age x Sex                |       |        |       | .3      |
| [Total]                  | .4    | .3     | .3    | 1       |


1. No partial levels of a marginal as part of a single workload-increment step.
   i.e. all-or-nothing when adding marginals to the workload.
   
### Naming conventions

* Owner/renter = V1 
* voting-age x hisp x cenRace = V2
* age x sex = V3

### Top-down nested workload structure

1. (0002 margins) sim0 = detailed x tract
1. (xxxx margins) sim1 = detailed x tract
1. (xxxx margins) sim2 = county x tract
1. (xxxx margins) sim3 = state
1. (xxxx margins) sim4 = 
1. (xxxx margins) sim5 = 
1. (xxxx margins) sim6 = 
1. (xxxx margins) sim7 = 
1. (xxxx margins) sim8 = 
1. (xxxx margins) sim9 = 

