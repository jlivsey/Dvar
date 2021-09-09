This simulation has:

$$ \epsilon = 12 $$

and $\epsilon$ schedule,

|                          | State | County | Tract | [Total] |
|--------------------------|-------|--------|-------|---------|
| detailed                 |       |        |       | .6      |
| Own/rent                 |       |        |       | .1      |
| VoteAge x Hisp x CenRace |       |        |       | .2      |
| Age x Sex                |       |        |       | .1      |
| [Total]                  | .1    | .3     | .6    | 1       |


For the following set of simulations we move outward in a nested structure from the no margins case.
This follows Table 2 in the write up, starting in the top-right with only detailed-tract.

Nested Structure:
-----------------
1. (0002 margins) non = just detailed at tract level
2. (0086 margins) tr1 = just own/rent margins                    
3. (0172 margins) tr2 = add (votingAge x hisp x cenRace) = (1, 1, 1)
4. (0256 margins) tr3 = add (votingAge x hisp x cenRace) = (2, 1, 1)
5. (0430 margins) tr4 = add (votingAge x hisp x cenRace) = (., 2, 1)
6. (0774 margins) tr5 = add (votingAge x hisp x cenRace) = (., ., 2)
7. (1290 margins) tr5 = add (votingAge x hisp x cenRace) = (., ., 3:4)
8. (1548 margins) tr6 = add all (votingAge x hisp x cenRace) AND all age x sex
9. (1800 margins) cnt = add all county 
10. (1872 margins) all = all margins                              

