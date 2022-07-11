
### Deal with NAs in the predictors

deal_with_NA = function(X, X_fill_values = NA, X_is_train, var_cat, var_num){
    
    ### Training set
    if(X_is_train){
        X_fill_values = head(X, 0)
        for(j in 1:ncol(X)){
            # Categoric predictors:
            if(names(X)[j] %in% var_cat){
                # Empty strings:
                X[which(X[, j] == ""), j] = NA
                
                # Mode:
                x_mode = getmode(X[, j])[1]
                
                # Number of classes:
                lvls = unique(X[, j])
                lvls = lvls[!is.na(lvls)]
                n_classes = length(lvls)
                
                # Random class:
                set.seed(1)
                x_random_class = sample(x = lvls,
                                        size = 1,
                                        replace = FALSE)
                
                # Decide the fill value:
                if(n_classes == 2){
                    pis = table(X[, j])
                    pi_1 = pis[1][[1]]/sum(pis)
                    pi_2 = pis[2][[1]]/sum(pis)
                    # Two classes proportions:
                    if(abs(pi_1 - pi_2) < 0.05){
                        X[which(is.na(X[, j])), j] = x_random_class
                        X_fill_values[1, j] = x_random_class
                    } else{
                        X[which(is.na(X[, j])), j] = x_mode
                        X_fill_values[1, j] = x_mode
                    }
                } else{
                    n_classes_lim = 5
                    if(n_classes > n_classes_lim){
                        X[which(is.na(X[, j])), j] = x_random_class
                        X_fill_values[1, j] = x_random_class
                    } else{
                        X[which(is.na(X[, j])), j] = x_mode
                        X_fill_values[1, j] = x_mode
                    }
                }
            }
            
            # Numeric predictors:
            if(names(X)[j] %in% var_num){
                # Mean:
                x_mean = mean(X[, j],
                              na.rm = TRUE)
                
                # Median:
                x_median = median(X[, j],
                                  na.rm = TRUE)

                # Skewness:
                x_skewness = abs(moments::skewness(X[, j],
                                                   na.rm = TRUE))
                
                # Decide the fill value:
                if(x_skewness > 0.5){
                    X[is.na(X[, j]), j] = x_median
                    X_fill_values[1, j] = x_median
                } else{
                    X[is.na(X[, j]), j] = x_mean
                    X_fill_values[1, j] = x_mean
                }
            }
        }
    } 
    
    ### Test set
    else{
        for(j in 1:ncol(X)){
            # Empty strings:
            X[which(X[, j] == ""), j] = NA
            
            # Fill value from the training set:
            X[which(is.na(X[, j])), j] = X_fill_values[1, j]
        }
    }
    
    return(list(X, X_fill_values))
}

