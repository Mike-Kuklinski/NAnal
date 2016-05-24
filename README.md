#NAnal

**Author:** Mike Kuklinski  
**Date:** 5/10/16  
**Language:** R  
**Computer:** Windows 10, 64-bit  
**Description:**  

NAnal is a list of functions written in R which help identify the strength of
data frame variables that are NA heavy. The idea is that you might have a sparse 
data frame with many variables, some of which may have a lot of NA values. 
However, having a lot of NA values for one 
variable may not necessarily mean the values it does contain, are bad predictors.
These functions help get a gauge on the sparse variables and find the optimal subset
of the data frame which minimizes NAs and maximizes the number of observations
without deleting variables.

### Libraries Used  
dplyr  
foreach  
parallel  
doSNOW  
digest  
stringr


### Functions Included

**nanal.var2ans:** Function which takes a data frame and an answer variable and returns a table of
the data frame variables and their associated correlation to the answer variable
and the percent NA.

**nanal.reduce_comb:** Function which takes a data frame and returns all possible complete case 
combinations of variables the data frame can be subset by. 

**nanal.score:** Function which takes the same data frame passed to nanal.reduce_comb function as well
as and the reduce_comb list created and returns summary statistics of the data frame when 
subset by complete cases of the reduce_comb list. Included in the summary statistics
is a weighted score (true Bayesian) for each combination which evaluates the
percent of NAs remaining versus the number of observations/rows remaining.

See also *example.Rmd/example.pdf*