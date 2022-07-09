
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
require(splitTools)
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

df1 = df_available %>% dplyr::select(Survived, Fare, Embarked)
# df1$Embarked = as.factor(df1$Embarked)

inds = splitTools::partition(y = df1$Survived,
                             p = c(train = 0.8,
                                   test = 0.2),
                             type = "stratified"
                             # type = "grouped"
                             )

plot_density_2_sets(X = df1$Embarked,
                    ind_1 = inds$train,
                    ind_2 = inds$test)









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















