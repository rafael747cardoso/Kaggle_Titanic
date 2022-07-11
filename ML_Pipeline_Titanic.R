
##################################################################################################################
############################################## Titanic Survival ##################################################
##################################################################################################################

require(plotly)
require(ggplot2)
require(grid)
require(gridExtra)
require(RColorBrewer)
require(GGally)
require(ggbiplot)
require(dplyr)
require(tidyr)
require(fastDummies)
require(Amelia)

require(splitstackshape)
require(moments)

require(boot)
require(glmnet)
require(class)
require(Hmisc)
require(ROCR)
require(klaR)

source("./funcs/fix_bad_levels.R")
source("./funcs/plot_density.R")
source("./funcs/plot_density_2_sets.R")
source("./funcs/getmode.R")


set.seed(111)

##################################################################################################################
################################################## Model #########################################################

#------------------------------------------ Data Preprocessing ---------------------------------------------------

# Read:
df_available = read.csv("./data/available_data.csv")

# Variable types:
var_response = "Survived"
var_id = "PassengerId"
var_cat = df_available %>%
              dplyr::select(-all_of(var_response),
                            -all_of(var_id)) %>%
              dplyr::select_if(~!is.numeric(.)) %>%
              names()
var_num = df_available %>%
              dplyr::select(-all_of(var_response),
                            -all_of(var_id)) %>%
              dplyr::select_if(~is.numeric(.)) %>%
              names()

# Stratified Train/Test split:
train_size = 0.8
var_strata = c(var_response, "Sex", "Embarked")
samps = splitstackshape::stratified(indt = df_available,
                                    group = var_strata,
                                    bothSets = TRUE,
                                    keep.rownames = TRUE,
                                    size = train_size)
df_train = as.data.frame(samps$SAMP1)
row.names(df_train) = df_train$rn
df_train = df_train[, -1]
df_test = as.data.frame(samps$SAMP2)
row.names(df_test) = df_test$rn
df_test = df_test[, -1]

# Predictors and response:
X_train = df_train[, c(var_cat, var_num)]
Y_train = df_train[, var_response]
X_test = df_test[, c(var_cat, var_num)]
Y_test = df_test[, var_response]

############ Transform

###### Missing data

X = X_train
X_is_train = TRUE

deal_with_NA = function(X, X_means, X_medians, X_modes, X_is_train, var_cat, var_num){
    
    if(X_is_train){
        ### Training set
        
        X_means = c()
        X_medians = c()
        X_modes = c()
        for(j in 1:ncol(X)){
            # Categoric predictors:
            if(names(X)[j] %in% var_cat){
                # Empty strings:
                X[which(X[, j] == ""), j] = NA
                
                # Measures of central tendency:
                X_means = c(X_means, NA)
                X_medians = c(X_medians, NA)
                x_mode = getmode(X[, j])[1]
                X_modes = c(X_modes, x_mode)
                
                # Number of classes:
                lvls = unique(X[, j])
                lvls = lvls[!is.na(lvls)]
                n_classes = length(lvls)
                if(n_classes == 2){
                    pis = table(X[, j])
                    pi_1 = pis[1][[1]]/sum(pis)
                    pi_2 = pis[2][[1]]/sum(pis)
                    # Two classes proportions:
                    if(abs(pi_1 - pi_2) < 0.05){
                        X[is.na(X[, j]), j] = NA #*  random class?
                    } else{
                        X[is.na(X[, j]), j] = x_mode
                    }
                } else{
                    n_classes_lim = 5
                    if(n_classes > n_classes_lim){
                        X[is.na(X[, j]), j] = NA #* random class?
                    } else{
                        X[is.na(X[, j]), j] = x_mode
                    }
                }
            }
            # Numeric predictors:
            if(names(X)[j] %in% var_num){
                # Measures of central tendency:
                x_mean = mean(X[, j],
                              na.rm = TRUE)
                x_median = median(X[, j],
                                  na.rm = TRUE)
                X_means = c(X_means, x_mean)
                X_medians = c(X_medians, x_median)
                X_modes = c(X_modes, NA)
                
                # Distribution skewness:
                x_skewness = abs(moments::skewness(X[, j],
                                                   na.rm = TRUE))
                if(x_skewness > 0.5){
                    X[is.na(X[, j]), j] = x_median
                } else{
                    X[is.na(X[, j]), j] = x_mean
                }
            }
        }
    } else{
        ### Test set
        
        X_means = NA
        X_medians = NA
        X_modes = NA
        
        
    }
    
    
    
    return(list(X, X_means, X_medians, X_modes))
}




###### Feature engineering

###### Update variable types

###### Dummy the categoric predictors

###### Update variable types

###### Predictors in common

###### Scaling of the numeric (not dummy) predictors



#------------------------------------------ Model Selection ------------------------------------------------------

#------------------------------------------ Training -------------------------------------------------------------

#------------------------------------------ Prediction -----------------------------------------------------------

#------------------------------------------ Evaluation -----------------------------------------------------------

#------------------------------------------ Final model ----------------------------------------------------------






##################################################################################################################
################################################ Production ######################################################

#------------------------------------------ Data Preprocessing ---------------------------------------------------

# Read:
df_new = read.csv("./data/new_data.csv")


#------------------------------------------ Prediction -----------------------------------------------------------















