
############################################## Titanic Survival ##################################################

require(ggplot2)
require(gridExtra)
require(grid)
require(plotly)
require(dplyr)
require(tidyr)
require(fastDummies)
require(boot)
require(glmnet)
require(Amelia)
require(class)
require(Hmisc)

source("./funcs/fix_bad_levels.R")
source("./funcs/is_dummy.R")
source("./funcs/getmode.R")
source("./funcs/make_dens_plot.R")
source("./funcs/make_boxplot.R")
source("./funcs/make_shrinkage_plot.R")
source("./funcs/make_subset_selection_plot.R")
source("./funcs/make_dim_reduc_plot.R")
source("./funcs/multi_reg_plots.R")
source("./funcs/make_comparison_plot.R")
source("./funcs/make_mest_models_plot.R")
source("./funcs/make_cv_knn_plot.R")

set.seed(111)

############ Data

# Read:
df_train = read.csv("./data/train.csv")
df_test = read.csv("./data/test.csv")

# Special variables:
response_var = "Survived"
id_var = "PassengerId"

# Concatenate the training and test sets to maintain the structure:
df_train = df_train[, c(1, 3:(ncol(df_train)), 2)]
val_test = -1
df_test[response_var] = val_test # just to know that it is the test set
df_all = rbind(df_train,
               df_test)
boo_train = df_all[response_var] != val_test

# Variable types:
cat_vars = df_all %>%
               dplyr::select(-all_of(response_var),
                             -all_of(id_var)) %>%
               dplyr::select_if(~!is.numeric(.)) %>%
               names()
num_vars = df_all %>%
               dplyr::select(-all_of(response_var),
                             -all_of(id_var)) %>%
               dplyr::select_if(~is.numeric(.)) %>%
               names()

# Replace NA's with the mean (for numeric) or the mode (for categoric) in each column:
for(var in num_vars){
    if(sum(is.na(df_all[var])) > 0){
        df_all[is.na(df_all[var]) & boo_train, var] = mean(df_all[boo_train, var], na.rm = TRUE)
        df_all[is.na(df_all[var]) & !boo_train, var] = mean(df_all[boo_train, var], na.rm = TRUE)
    }
}
for(var in cat_vars){
    if(sum(is.na(df_all[var])) > 0){
        df_all[is.na(df_all[var] & boo_train), var] = getmode(df_all[boo_train, var])
        df_all[is.na(df_all[var] & !boo_train), var] = getmode(df_all[boo_train, var])
    }
}
Amelia::missmap(df_all)

###### Feature engineering for the nasty categorical variables

### Name

# Title:
passenger_names = df_all$Name
passenger_names = gsub(x = passenger_names,
                       pattern = '\\"',
                       replacement = "")
passenger_names = gsub(x = passenger_names,
                       pattern = ")",
                       replacement = "")
passenger_names = gsub(x = passenger_names,
                       pattern = "\\(",
                       replacement = "")
passenger_names = gsub(x = passenger_names,
                       pattern = "-",
                       replacement = " ")
passenger_names = gsub(x = passenger_names,
                       pattern = "'",
                       replacement = " ")
df_all$Name = passenger_names
lastname = c()
title = c()
for(i in 1:nrow(df_all)){
    s = strsplit(x = passenger_names[i],
                 split = "\\,")[[1]]
    lastname = c(lastname,
                 s[1])
    ss = strsplit(x = s[2],
                  split = "\\.")[[1]][1] %>%
             trimws()
    title = c(title,
              ss)
}
title[title == "Mlle"] = "Miss"
title[title == "Mme"] = "Mrs"
title[title %in% c("Don", "Jonkheer", "Lady", "Sir", "the Countess")] = "Nobility"
title[title %in% c("Col", "Major", "Capt")] = "Military"
df_all$title = title

# Number of last name relatives:
df_all$lastname = lastname
df_relatives = df_all %>%
                   dplyr::group_by(lastname) %>%
                   dplyr::summarise(lastname_relatives = n()) %>%
                   as.data.frame()
df_all = df_all %>%
             dplyr::left_join(df_relatives,
                              by = c("lastname" = "lastname"))

# Number of names:
number_names = c()
for(i in 1:nrow(df_all)){
    passenger_name = passenger_names[i]
    all_names = strsplit(x = passenger_name,
                         split = " ")[[1]][-2]
    number_names = c(number_names,
                     length(all_names))
}
df_all$number_names = number_names

# Spouses by name:
df_all$with_spouse = NA
lastnames = sort(unique(lastname))
for(i in 1:length(lastnames)){
    person_lastname = lastnames[i]
    df_i = df_all %>%
               dplyr::filter(lastname == person_lastname)
    firstnames = gsub(x = df_i$Name,
                      pattern = person_lastname,
                      replacement = "")
    firstnames = gsub(x = firstnames,
                      pattern = "\\, ",
                      replacement = "")
    firstnames = sapply(X = firstnames,
                        FUN = function(z) strsplit(x = z, split = " ")[[1]][2]) %>%
                     as.character()
    appeared = c()
    for(j in 1:length(firstnames)){
        ind = df_i$PassengerId[j]
        if(firstnames[j] %in% appeared){
            df_all$with_spouse[ind] = "yes"
        } else{
            df_all$with_spouse[ind] = "no"
        }
        appeared = c(appeared,
                     firstnames[j])
    }
}
df_all = df_all %>%
             dplyr::select(-Name,
                           -lastname)

###  Ticket

# Simplify the levels:
df_all$Ticket = as.factor(df_all$Ticket)
tickets = levels(df_all$Ticket)
tickets = gsub(x = tickets,
               pattern = "\\.",
               replacement = "")
tickets = gsub(x = tickets,
               pattern = "/",
               replacement = "")
tickets[!is.na(as.numeric(tickets))] = "N"
before = c("A ", "A4", "A5", "AQ", "AS", "C ", "CA", "FC", "Fa", "LP", "LI", "PP", "PC", "SC", 
           "SO", "SP", "ST", "SW", "WC", "WE")
after = c("A", "A", "A", "A", "A", "C", "C", "F", "F", "F", "F", "P", "P", "SC", "SO",
          "SO", "ST", "ST", "W", "W")
for(i in 1:length(before)){
    tickets[substr(x = tickets,
                   start = 1,
                   stop = 2) == before[i]] = after[i]
}
levels(df_all$Ticket) = tickets
df_all$Ticket = as.character(df_all$Ticket)

### Cabin

# Simplify the levels to Deck:
df_all$Cabin[df_all$Cabin == ""] = NA
df_all$Cabin = as.factor(df_all$Cabin)
cabins = levels(df_all$Cabin)
before = c("A", "B", "C", "D", "E", "F", "G", "T")
after = c("A", "B", "C", "D", "E", "F", "F", "F")
for(i in 1:length(before)){
    cabins[substr(x = cabins,
                  start = 1,
                  stop = 1) == before[i]] = after[i]
}
levels(df_all$Cabin) = cabins
df_all$Cabin = as.character(df_all$Cabin)
df_all = df_all %>%
             dplyr::rename(Deck = Cabin)

### K-Nearest Neighbors to fill in the gaps in Deck

# Submodel train and test sets:
resp = "Deck"
predctrs = c("Pclass", "Age", "SibSp", "Parch", "Fare", "lastname_relatives", "number_names")
df_withdeck = df_all %>%
                  dplyr::filter(!is.na(Deck))
df_nodeck = df_all %>%
                dplyr::filter(is.na(Deck))

# Cross-validation to find the best k in kNN:
k_partitions = 5
n_obs = nrow(df_withdeck)
int_n_obs = k_partitions*n_obs%/%k_partitions
X = 1:n_obs
prttns = Hmisc::partition.vector(x = 1:int_n_obs,
                                 sep = rep(int_n_obs/k_partitions, k_partitions)) 
if(int_n_obs < n_obs){
    prttns[k_partitions][[1]] = c(prttns[k_partitions][[1]], X[int_n_obs:n_obs])
}
ks = 1:n_obs
mean_cv_CER = c()
sd_cv_CER = c()
for(k_neighbors in ks){
    cv_CER = c()
    for(i in 1:k_partitions){
        test_ind = prttns[i][[1]]
        fit = class::knn(train = df_withdeck[-test_ind, predctrs],
                         test = df_withdeck[test_ind, predctrs],
                         cl = df_withdeck[-test_ind, resp],
                         k = k_neighbors) %>%
                  as.character()
        cv_CER = c(cv_CER,
                   mean(fit != df_withdeck[test_ind, resp]))
    }
    mean_cv_CER = c(mean_cv_CER,
                    mean(cv_CER))
    sd_cv_CER = c(sd_cv_CER,
                  sd(cv_CER))
}
df_ks = data.frame(
    ks = ks,
    mean_cv_CER = mean_cv_CER,
    sd_cv_CER = sd_cv_CER
)
df_best = df_ks %>%
              dplyr::filter(mean_cv_CER == min(mean_cv_CER))

make_cv_knn_plot(df_ks = df_ks,
                 df_best = df_best)

# Assign the Deck from the kNN fit:
fit = class::knn(train = df_withdeck[, predctrs],
                 test = df_nodeck[, predctrs],
                 cl = df_withdeck[, resp],
                 k = df_best$ks[1])
df_nodeck$Deck = as.character(fit)

### Embarked

df_all$Embarked[df_all$Embarked == "" & boo_train] = getmode(df_all[boo_train, "Embarked"])
df_all$Embarked[df_all$Embarked == "" & !boo_train] = getmode(df_all[boo_train, "Embarked"])

# Update the variable types:
cat_vars = df_all %>%
               dplyr::select(-all_of(response_var),
                             -all_of(id_var)) %>%
               dplyr::select_if(~!is.numeric(.)) %>%
               names()
num_vars = df_all %>%
               dplyr::select(-all_of(response_var),
                             -all_of(id_var)) %>%
               dplyr::select_if(~is.numeric(.)) %>%
               names()

# Fix the levels:
for(var in cat_vars){
    df_all[, var] = fix_bad_levels(df_all[, var])
}

# Dummies:










############ Model selection

###### Best Subset Selection - Logistic Regression




###### Forward Stepwise Selection - Logistic Regression




###### Ridge - Logistic Regression




###### Lasso - Logistic Regression




###### PCA - Logistic Regression




###### Regularized Discriminant Analysis




###### k-Nearest Neighbours




###### Comparison




############ Prediction

###### Estimated competition score for the test set

kaggle_score = function(y_pred, y_real, n_df, n_obs){
    estimated_score = 1 #
    return(estimated_score)
}

# Train/Test split:
ind_test = sample(x = 1:nrow(df_train_stand),
                  size = trunc(0.4*nrow(df_train_stand)),
                  replace = FALSE)
df_train2 = df_train_stand[-ind_test, ]
df_test2 = df_train_stand[ind_test, ]
X_df_test2 = df_test2 %>%
                 dplyr::select(-all_of(response_var))

# Best Subset Selection - Logistic Regression:



# Forward Stepwise Selection - Logistic Regression:



# Ridge - Logistic Regression:



# Lasso - Logistic Regression:



# PCA - Logistic Regression:



# Regularized Discriminant Analysis:



# k-Nearest Neighbours:



# Ensemble model:
y_pred = data.frame(
    y_pred_bestsub,
    y_pred_forward,
    y_pred_ridge,
    y_pred_lasso,
    y_pred_pcalr,
    y_pred_rda,
    y_pred_knn
) %>%
    rowMeans()
estimated_score_ensemble = kaggle_score(y_pred = y_pred_pls[y_pred_pls > 0],
                                        y_real = df_test2[response_var],
                                        n_df = pls::selectNcomp(fit_pls,
                                                                method = "onesigma",
                                                                plot = FALSE),
                                        n_obs = nrow(df_test2))

# Compare the scores:
df_models_score = data.frame(
    "models" = c("Best Subset", "Forward Stepwise", "Ridge", "Lasso", "PCA LR", "RDA", "kNN", "Ensemble"),
    "metric_name" = c(estimated_score_bestsub, estimated_forward, estimated_score_ridge,
                      estimated_score_lasso, estimated_score_pcalr, estimated_score_rda, 
                      estimated_score_knn, estimated_score_ensemble),
    "se_metric_name" = c(NA, NA, NA, NA, NA, NA, NA, NA),
    stringsAsFactors = FALSE
)
make_mest_models_plot(df_models = df_models_score,
                      metric = "Score")

###### Predict with an ensemble model

df_test_final = df_test_stand %>%
                     dplyr::select(-all_of(id_var))

y_pred_bestsub = predict(fit_bestsub,
                         df_test_final) %>%
                     as.numeric()
y_pred_forward = predict(fit_forward,
                         df_test_final) %>%
                     as.numeric()
y_pred_ridge = predict(fit_ridge,
                       as.matrix(df_test_final),
                       s = cv_ridge$lambda.1se,
                       alpha = 1,
                       standardize = FALSE,
                       family = "gaussian") %>%
                   as.numeric()
y_pred_lasso = predict(fit_lasso,
                       as.matrix(df_test_final),
                       s = cv_lasso$lambda.1se,
                       alpha = 1,
                       standardize = FALSE,
                       family = "gaussian") %>%
                   as.numeric()
y_pred_pcalr = predict(fit_pcalr,
                       df_test_final) %>%
                   as.numeric()
y_pred_rda = predict(fit_rda,
                     df_test_final) %>%
                 as.numeric()
y_pred_knn = predict(fit_knn,
                     df_test_final) %>%
                 as.numeric()
y_pred = data.frame(
    y_pred_lasso,
    y_pred_ridge,
    y_pred_forward,
    y_pred_pcr,
    y_pred_pls
) %>%
    rowMeans()
y_pred[y_pred < 0] = 0
df_pred = data.frame(
    "Id" = df_test_stand$Id,
    "SalePrice" = y_pred
)
write.csv(df_pred,
          file = "./data/submission_ensemble.csv",
          row.names = FALSE)












