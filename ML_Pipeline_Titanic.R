
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
source("./funcs/deal_with_NA.R")

set.seed(1)

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
df_X_train = df_train[, c(var_cat, var_num)]
Y_train = df_train[, var_response]
df_X_test = df_test[, c(var_cat, var_num)]
Y_test = df_test[, var_response]

############ Transform

###### Missing data

# Train:
NA_resp_list = deal_with_NA(X = df_X_train,
                            X_is_train = TRUE,
                            var_cat = var_cat,
                            var_num = var_num)
df_X_train = NA_resp_list[[1]]
X_train_fill_values = NA_resp_list[[2]]

# Test:
NA_resp_list = deal_with_NA(X = df_X_test,
                            X_fill_values = X_train_fill_values,
                            X_is_train = FALSE,
                            var_cat = var_cat,
                            var_num = var_num)
df_X_test = NA_resp_list[[1]]

###### Feature engineering

# Train:
df_X_train = feature_eng(X = df_X_train,
                         var_cat = var_cat,
                         var_num = var_num)

# Test:
df_X_test = feature_eng(X = df_X_test,
                        var_cat = var_cat,
                        var_num = var_num)

###### Update variable types

# Train:
var_cat_train = df_X_train %>%
                    dplyr::select_if(~!is.numeric(.)) %>%
                    names()
var_num_train = df_X_train %>%
                    dplyr::select_if(~is.numeric(.)) %>%
                    names()

# Test:
var_cat_test = df_X_test %>%
                   dplyr::select_if(~!is.numeric(.)) %>%
                   names()
var_num_test = df_X_test %>%
                   dplyr::select_if(~is.numeric(.)) %>%
                   names()

###### Dummy the categoric predictors

### Train

# Replace special characters:
for(var in var_cat_train){
    df_X_train[, var] = fix_bad_levels(df_X_train[, var])
}

# One-hot-encoding:
df_X_train_cats_dumm = fastDummies::dummy_cols(.data = df_X_train %>%
                                                           dplyr::select(all_of(var_cat_train)),
                                               select_columns = var_cat_train,
                                               remove_selected_columns = TRUE,
                                               remove_first_dummy = TRUE)
var_dummy_train = names(df_X_train_cats_dumm)
df_X_train = cbind(df_X_train %>%
                       dplyr::select(-all_of(var_cat_train)),
                   df_X_train_cats_dumm)

### Test

# Replace special characters:
for(var in var_cat_test){
    df_X_test[, var] = fix_bad_levels(df_X_test[, var])
}

# One-hot-encoding:
df_X_test_cats_dumm = fastDummies::dummy_cols(.data = df_X_test %>%
                                                          dplyr::select(all_of(var_cat_test)),
                                              select_columns = var_cat_test,
                                              remove_selected_columns = TRUE,
                                              remove_first_dummy = TRUE)
var_dummy_test = names(df_X_test_cats_dumm)
df_X_test = cbind(df_X_test %>%
                      dplyr::select(-all_of(var_cat_test)),
                  df_X_test_cats_dumm)

###### Update variable types  * intersect ... 

# Train:
var_num_train = df_X_train %>%
                    dplyr::select_if(~is.numeric(.)) %>%
                    names()

# Test:
var_cat_test = df_X_test %>%
                   dplyr::select_if(~!is.numeric(.)) %>%
                   names()
var_num_test = df_X_test %>%
                   dplyr::select_if(~is.numeric(.)) %>%
                   names()





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















