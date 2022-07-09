
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

require(boot)
require(glmnet)
require(class)
require(Hmisc)
require(ROCR)
require(klaR)

source("./funcs/fix_bad_levels.R")
source("./funcs/plot_density.R")
source("./funcs/plot_density_2_sets.R")


set.seed(111)

##################################################################################################################
################################################## Model #########################################################

#------------------------------------------ Data Preprocessing ---------------------------------------------------

# Read:
df_available = read.csv("./data/available_data.csv")

# Variable types:
var_response = "Survived"
var_id = "PassengerId"
var_cat = df_availble %>%
              dplyr::select(-all_of(var_response),
                            -all_of(var_id)) %>%
              dplyr::select_if(~!is.numeric(.)) %>%
              names()
var_num = df_availble %>%
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

# Matrices:
X_train = as.matrix(df_train[, c(var_cat, var_num)])
Y_train = df_train[, var_response]
X_test = as.matrix(df_test[, c(var_cat, var_num)])
Y_test = df_test[, var_response]

############ Transform

###### Missing data

X = X_train
X_type = "train"

deal_with_NA = function(X, X_type, var_cat, var_num){
    
    if()
    
    
    
    return(list(X, train_mean, train_median, train_mode))
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















