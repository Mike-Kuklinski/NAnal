# Project: NAnal Functions
# Author: Mike Kuklinski
# Date: 5/10/16
# Description: NAnal is a list of functions which help identify the strength of
# data frame variables that are NA heavy.

# The idea is that you might have a sparse data frame with many variables, some
# of which may have a lot of NA values. However, having a lot of NA values for one 
# variable may not necessarily mean the values it does contain, are bad predictors.
# These functions help get a gauge on the sparse variables and find the optimal subset
# of the data frame which minimizes NAs and maximizes the number of observations
# without deleting variables.


# Load Libraries
library(dplyr)
library(foreach)
library(parallel)
library(doSNOW)
cl <- makeCluster(2)
registerDoSNOW(cl)
library(digest)
library(stringr)


# ==============================================================================
# Load/Create Test Data frames
# ==============================================================================

# # Sample data frame
df_test <- data.frame(a = c(1,2,3,NA,NA,6,7,1,NA,NA),
                  b = c(1,2,NA,NA,NA,6,7,1,1,1),
                  c = c(1,2,3,NA,NA,6,7,1,1,1),
                  d = c(NA,NA,3,4,5,6,7,1,1,1),
                  e = c(1,2,3,NA,NA,6,7,1,1,1),
                  f = c(1,7,2,9,1,1,8,2,5,9),
                  g = c(1,2,3,NA,NA,6,7,1,1,1),
                  h = c(NA,2,3,NA,NA,NA,NA,NA,1,1),
                  i = c(1,2,3,4,5,6,7,1,1,1),
                  j = c(1,2,3,NA,NA,6,7,1,1,1),
                  k = c(1,3,3,NA,NA,6,7,1,NA,1),
                  l = c(1,2,NA,5,NA,6,7,1,1,1),
                  m = c(2,3,4,4,5,6,7,1,1,1),
                  n = c(1,2,3,4,5,6,7,1,1,1),
                  o = c(NA,2,NA,NA,NA,NA,NA,NA,1,1),
                  p = c(7,7,6,1,8,NA,NA,6,4,5),
                  ans = c(1,2,3,4,5,6,7,1,1,1))



# ==============================================================================
# NAnal.var2ans - Code
# ==============================================================================

# Function which takes a data frame and an answer variable and returns a table of 
# the data frame variables and their associated correlation to the answer variable
# and the percent NA.
#
# Inputs:
# df - Data frame
# lo_list - vector of string variable names to ignore in the function
# ans_var - String name of variable being predicted

NAnal.var2ans <- function(df, ans_var, lo_list = NA){
    # Subset data frame to exclude leave out items and extract answer variable
    lo_list <- which(names(df) %in% c(lo_list, ans_var))
    ans_var <- df[,ans_var]
    sub_df <- df[,-lo_list]
    # Calculate correlation between individual predictors and answer
    var_cor <- apply(sub_df, 2, function(x){cor(x, ans_var, use = 'pairwise.complete.obs')})
    var_cor_abs <- abs(var_cor)
    # Calculate the number and percent NAs for each predictor variable
    var_num_na <- apply(sub_df, 2, function(x){sum(is.na(x))})
    var_pct_na <- apply(sub_df, 2, function(x){sum(is.na(x))/length(x)})
    # Identify predictor variables with a correlation higher than the percent NA
    var_cor_napct <- abs(var_cor) >= var_pct_na
    # Calculate the linear regression p_value associated with each predictor
    var_pval <- apply(sub_df, 2, function(x){summary(lm(ans_var~x))$coefficients[2,4]})
    # Return breakdown of variables
    var2ans <- data.frame(variable = names(sub_df), 
                             index = which(names(df) %in% names(sub_df)), 
                             cor_to_ans = round(var_cor, 2), 
                             abs_cor_to_ans = round(var_cor_abs, 2),
                             num_NA = var_num_na,
                             pct_NA = var_pct_na,
                             NAlessCor = var_cor_napct,
                             pvalue = round(var_pval,5))
    var2ans
}


# ==============================================================================
# NAnal.reduce_comb - Code
# ==============================================================================

# Function which takes a data frame and returns all possible complete case 
# combinations of variables the data frame can be subset by. The function starts
# by taking a predictor variable and subsetting the data frame by its non-NA values.
# Then it looks at the reduced data frame and repeats the reduction process for 
# predictor variables which still contain NA values. It does this using recursion 
# until either no NAs exist or no rows remain in the data frame.
#
# Inputs:
# df - Data frame 
# selected_var - predictor variable for which to subset by non-NA values, 
# var_tail - list of selected variables preceding the currently selected variable
# pick_list - final list to be returned showing all the possible combinations
# pick_lu_list - string version of pick_list
# start_trg - Trigger which identifies if it's the start of the function 
# orig_names - Original Names of the variables in the data frame
# col_reduce - Option to have predictor variables with duplicate NA rows to be removed during
#              the reduction process

NAnal.reduce_comb <- function(df, 
                              selected_var = NA, 
                              var_tail = NA, 
                              pick_list = list(), 
                              pick_lu_list = c(), 
                              start_trg = T, 
                              orig_names = NA, 
                              col_reduce = T){
    # Check if start and adjust parameters accordingly
    if(start_trg == T){
        df <- data.frame(is.na(df)*1)
        orig_names <- names(df)
    }
    # Reduce dataframe rows by selected variables containing values (not na)
    if(is.na(selected_var)){
        df_reduced <- df
        select_var_cut <- 0
    }else{
        df_reduced <- df[df[, selected_var]==0,]
        select_var_cut <- selected_var
    }
    # Check if reduced dataframe contains rows and NAs
    if(nrow(df_reduced) > 0){
        if(sum(df_reduced) > 0){
            # Reduce/Remove column/variables with the same NA rows
            if(col_reduce){
                dups <- which(!duplicated(apply(df_reduced, 2, digest)))
            }else{dups <- 1:length(df_reduced)
            }
            dup_names <- names(df_reduced)[dups]
            # subset duplicate combinations to avoid repeat scenarios during recurtion
            sub_dups <- dups[which(dups > select_var_cut)]
            sub_dup_names <- names(df_reduced)[sub_dups]
            df_reduced <- data.frame(df_reduced[,dups])
            names(df_reduced) <- dup_names
            # Get list of new available selections (i.e. variables containing some(not all) NA values)
            new_pick_idx <- which(between(colSums(df_reduced), 1, (nrow(df_reduced)-1)))
            new_pick_names <- names(df_reduced)[new_pick_idx]
            # Subset new picks to only include sub_dup indices
            sub_pick_idx <- which(new_pick_names %in% sub_dup_names)
            new_pick_names <- new_pick_names[sub_pick_idx]
            new_pick_idx <- new_pick_idx[sub_pick_idx]
            orig_pick_idx <- which(orig_names %in% new_pick_names)
            # Add new selections to tail selections
            pick_adds <- mapply(function(x){list(sort(c(var_tail, x)))},orig_pick_idx)
            pick_lu <- mapply(function(x){paste(sort(c(var_tail, x)), collapse = '')},orig_pick_idx)
            # Using recursion, loop through new new available selections
            if(length(pick_adds) > 0){
                pick_list <- c(pick_list, pick_adds)
                pick_lu_list <- c(pick_lu_list, pick_lu)
                for(idx in 1:length(pick_adds)){
                    result <- NAnal.reduce_comb(df_reduced, 
                                                new_pick_idx[idx], 
                                                pick_adds[[idx]], 
                                                pick_list, 
                                                pick_lu_list, 
                                                F, 
                                                orig_names, 
                                                col_reduce)
                    #pick_list <- result[[1]]
                    #pick_lu_list <- result[[2]]
                    pick_list <- result$lists
                    pick_lu_list <- result$strings
                }
            }
        }
    }
    list('lists' = pick_list, 'strings' = pick_lu_list)
}


#===============================================================================
# NAnal.score - Code (evaluates NAnal.reduce_comb list)
#===============================================================================

# Function which takes the same data frame passed to NAnal.reduce_comb function as well
# as and the reduce_comb list created and returns summary statistics of the data when 
# subset by complete cases of the reduce_comb list. Included in the summary statistics
# is a weighted score (true bayesian) for each combination which evaluates the
# percent of NAs remaining versus the number of observations/rows remaining
#
# Inputs:
# df - Data frame
# reduce_comb - List of variable combinations to reduce by (generated by NAnal.reduce_comb)
# min_vote_counts - Target minimum number of observations/rows to use for comparing combinations

# Table Output names:
# AdjVarIdx - Data frame indices being subset by 
# ADJVarNames - Data frame variable names associated with the indices
# NumberObs - Number of remaining observations
# PercentCC - Percent of complete cases
# PercentNA - Percent NA remaining
# Wt_Vote - Weight Score of subset Data frame

NAnal.score <- function(df, reduce_comb, min_vote_count){
    reduce_comb <- c(reduce_comb, list(NULL))
    # Get initial size information from DF
    df_names <- names(df)
    # Calculate Average Vote score
    total_votes <- foreach(rd_idx = 1:length(reduce_comb),  .combine = c) %dopar% {
        # Record columns names and indices being removed
        temp_reduce_indices <- reduce_comb[[rd_idx]]
        # Remove indices from table and check for complete cases
        df_clone <- df[complete.cases(df[,temp_reduce_indices]),]
        votes <- apply((!is.na(df_clone))*1, 1, sum)
        votes/ncol(df_clone)
    }
    mean_total <- mean(total_votes)
    # Loop over each reduce_comb and check na analysis score
    var_na_anal <- foreach(rd_idx = 1:length(reduce_comb),  .combine = rbind) %dopar% {
        # Record columns names and indices being removed
        temp_reduce_indices <- reduce_comb[[rd_idx]]
        removed_col_idx <- paste(temp_reduce_indices, collapse = ' ')
        removed_col_names <- paste(df_names[temp_reduce_indices], collapse = ' ')
        # Remove indices from table and check for complete cases
        df_clone <- df[complete.cases(df[,temp_reduce_indices]),]
        num_cc <- sum(complete.cases(df_clone)*1)
        num_obs <- nrow(df_clone)
        num_col <- ncol(df_clone)
        num_tot <- num_obs * num_col
        # Calculate new percent of complete cases and NAs
        percent_cc <- round(num_cc/nrow(df_clone),2)
        percent_na <- round((sum(is.na(df_clone)*1)/num_tot), 2) 
        votes <- apply((!is.na(df_clone))*1, 1, sum)
        mean_vote <- mean(votes/num_col)
        wt_vote_1 <- (num_obs/(num_obs+min_vote_count))*mean_vote
        wt_vote_2 <- (min_vote_count/(min_vote_count+num_obs))*mean_total
        wt_vote <- wt_vote_1 + wt_vote_2
        data.frame(removed_col_idx, 
                   removed_col_names, 
                   num_obs,
                   percent_cc,
                   percent_na,
                   wt_vote)
    }
    names(var_na_anal) <- c('AdjVarIdx', 
                            'ADJVarNames',
                            'NumberObs', 
                            'PercentCC',
                            'PercentNA',
                            'Wt_Vote')
    # Order resulting list
    var_na_anal <- var_na_anal[order(var_na_anal$Wt_Vote, decreasing = T),]
    var_na_anal
}

