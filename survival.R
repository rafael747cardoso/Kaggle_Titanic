
############################################## Titanic Survival ##################################################

require(ggplot2)
require(gridExtra)
require(RColorBrewer)
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
require(ROCR)
require(GGally)
require(ggbiplot)
require(klaR)

source("./funcs/fix_bad_levels.R")
source("./funcs/is_dummy.R")
source("./funcs/getmode.R")
source("./funcs/make_dens_plot.R")
source("./funcs/make_boxplot.R")
source("./funcs/make_shrinkage_plot.R")
source("./funcs/make_subset_selection_plot.R")
source("./funcs/make_dim_reduc_plot.R")
source("./funcs/make_comparison_plot.R")
source("./funcs/make_best_models_plot.R")
source("./funcs/make_cv_knn_plot.R")
source("./funcs/logit_reg_plots.R")
source("./funcs/score_accuracy.R")
source("./funcs/make_roc_curve.R")
source("./funcs/regularized_logit_reg_plots.R")
source("./funcs/make_pc_plot.R")
source("./funcs/make_cv_pc_plot.R")


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
Amelia::missmap(df_all)
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
df_withdeck = df_all %>%
                  dplyr::filter(!is.na(Deck))
df_nodeck = df_all %>%
                dplyr::filter(is.na(Deck))
predctrs_pre = c("Pclass", "Age", "SibSp", "Parch", "Fare", "lastname_relatives", "number_names")
df_plt = df_withdeck[df_withdeck$Survived != val_test, c(predctrs_pre, resp)]
df_plt[, resp] = as.factor(df_plt[, resp])
GGally::ggpairs(
    data = df_plt,
    aes(
        colour = Deck,
        alpha = 0.8
    )
)
predctrs = c("Pclass", "Fare")

# Cross-validation to find the best k in kNN:
k_partitions = 10
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
}
df_ks = data.frame(
    ks = ks,
    mean_cv_CER = mean_cv_CER
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
df_all = rbind(df_withdeck,
               df_nodeck)

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
df_all_cats_dumm = fastDummies::dummy_cols(.data = df_all %>%
                                                       dplyr::select(all_of(cat_vars)),
                                           select_columns = cat_vars,
                                           remove_selected_columns = TRUE,
                                           remove_first_dummy = TRUE)
dummy_vars = names(df_all_cats_dumm)

# Now it has a total of length(num_vars) + 1 response + 1 id + length(df_all_cats_dumm) variables:
df_all = cbind(df_all %>%
                   dplyr::select(-all_of(cat_vars)),
               df_all_cats_dumm)
p_predictors = df_all %>%
                   dplyr::select(-all_of(response_var),
                                 -all_of(id_var)) %>%
                   names()
predictors_not_dummies = num_vars

# Back to the train/test split:
df_train = df_all %>%
               dplyr::filter(eval(parse(text = response_var)) > val_test)
df_test = df_all %>%
              dplyr::filter(eval(parse(text = response_var)) == val_test) %>%
              dplyr::select(-all_of(response_var))

### Standardize the predictors from training and test

# Training:
df_train_prdnotdum = df_train[predictors_not_dummies]
X_means_train = c()
X_sd_train = c()
for(j in 1:ncol(df_train_prdnotdum)){
    x = df_train_prdnotdum[, j]
    X_means_train = c(X_means_train,
                      mean(x))
    X_sd_train = c(X_sd_train,
                   sd(x))
    df_train_prdnotdum[, j] = (x - X_means_train[j])/X_sd_train[j]
}
df_train_stand = data.frame(
    id_var = df_train[, id_var],
    df_train_prdnotdum,
    df_train[, dummy_vars],
    response_var = df_train[, response_var]
)
names(df_train_stand)[which(names(df_train_stand) == "id_var")] = id_var
names(df_train_stand)[which(names(df_train_stand) == "response_var")] = response_var

# Test:
df_test_prdnotdum = df_test[predictors_not_dummies]
for(j in 1:ncol(df_test_prdnotdum)){
    x = df_test_prdnotdum[, j]
    df_test_prdnotdum[, j] = (x - X_means_train[j])/X_sd_train[j]
}
df_test_stand = data.frame(
    id_var = df_test[, id_var],
    df_test_prdnotdum,
    df_test[, dummy_vars]
)
names(df_test_stand)[which(names(df_test_stand) == "id_var")] = id_var

# To each model:
df_train_stand = df_train_stand %>%
                     dplyr::select(-all_of(id_var))
df_train_forward = df_train_stand
df_train_ridge = df_train_stand
df_train_lasso = df_train_stand
df_train_pcalr = df_train_stand
df_train_rda = df_train_stand
df_train_knn = df_train_stand

############ Model selection

###### Forward Stepwise Selection - Logistic Regression

# Cross-validated classification error rate for multiple logistic regression:
cv_CER_logit = function(predictors, df_model){
    # Fit:
    df_model = df_model[, c(response_var, predictors)]
    fit = glm(formula = paste(response_var, "~.",
                              collapse = ""),
              data = df_model,
              family = "binomial")
    
    # 10-fold cross-validated deviance:
    cv_CER = boot::cv.glm(data = df_model,
                          glmfit = fit,
                          K = 10)$delta[1]
    return(cv_CER)
}

### K-Fold Cross-validated CER

cv_CER = c()
cv_CER_se = c()
num_predictors = c()
predictors_names = c()
p = length(p_predictors)

# Null model:
fit = glm(formula = paste(response_var, "~1",
                          collapse = ""),
          data = df_train_forward,
          family = "binomial")
cv_CER_null = boot::cv.glm(data = df_train_forward,
                           glmfit = fit,
                           K = 10)$delta[1]
cv_CER = c(cv_CER,
           cv_CER_null)
cv_CER_se = c(cv_CER_se,
              sd(cv_CER_null))
num_predictors = c(num_predictors,
                   0)
predictors_names = c(predictors_names,
                     "")

# Forward selection:
used_predictors = c()
for(k in 0:(p - 1)){
    print(p - 1 - k)
    # The p - k models that augment the predictors in one:
    cv_CER_k = c()
    predictors_k = c()
    available_predictors = p_predictors[!(p_predictors %in% used_predictors)]
    for(j in 1:length(available_predictors)){
        additional_predictor = available_predictors[j]
        cv_CER_kj = cv_CER_logit(predictors = c(used_predictors,
                                                additional_predictor),
                                 df_model = df_train_forward)
        cv_CER_k = c(cv_CER_k,
                     cv_CER_kj)
        predictors_k = c(predictors_k,
                         additional_predictor)
    }
    
    # Choose the best submodel:
    chosen_predictor = predictors_k[which(cv_CER_k == min(cv_CER_k))]
    used_predictors = c(used_predictors,
                        chosen_predictor)
    cv_CER = c(cv_CER,
               min(cv_CER_k))
    cv_CER_se = c(cv_CER_se,
                  sd(cv_CER_k)/sqrt(nrow(df_train_forward)))
    num_predictors = c(num_predictors,
                       k + 1)
    predictors_names = c(predictors_names,
                         paste(used_predictors,
                               collapse = ","))
}

# CER values:
df_eval = data.frame(
    "num_predictors" = num_predictors,
    "cv_CER" = cv_CER,
    "cv_CER_se" = cv_CER_se,
    "predictors" = predictors_names
)
df_eval$cv_CER_se[is.na(df_eval$cv_CER_se)] = 0

# Best model with the 1-standard-error rule:
min_cv_CER = min(df_eval$cv_CER)
for(i in 2:nrow(df_eval)){
    if(df_eval$cv_CER[i] - df_eval$cv_CER_se[i] <= min_cv_CER){
        best_p = i - 1
        break
    }
}
best_predictors = (df_eval %>%
                       dplyr::filter(num_predictors == best_p))$predictors
best_predictors = strsplit(x = best_predictors,
                           split = ",")[[1]]

# Plot:
make_subset_selection_plot(df_eval = df_eval,
                           df_plot = df_eval,
                           best_predictors = best_predictors)

# Estimated Test Accuracy:
test_acc_forward = 1 - (df_eval %>%
                           dplyr::filter(num_predictors == best_p))$cv_CER
test_acc_se_forward = (df_eval %>%
                          dplyr::filter(num_predictors == best_p))$cv_CER_se

# Best model from Forward Stepwise Selection - Logistic Regression:
df_model = df_train_forward %>%
               dplyr::select(all_of(best_predictors),
                             all_of(response_var))
logit_reg_plots(df_model = df_model,
                model_type = "Logistic Regression with Forward Selection",
                fig_path = "./figs/Logit_Regr_Forward.png")

###### Ridge - Logistic Regression

# Matrix data:
X = as.matrix(df_train_ridge[, p_predictors])
Y = df_train_ridge[, response_var]

# Fit:
fit_ridge = glmnet::glmnet(x = X,
                           y = Y,
                           alpha = 0,
                           standardize = FALSE,
                           family = "binomial")

# K-Fold Cross-validation:
cv_ridge = glmnet::cv.glmnet(x = X,
                             y = Y,
                             alpha = 0,
                             type.measure = "class",
                             nfolds = 10,
                             standardize = FALSE,
                             family = "binomial")

# Plot:
make_shrinkage_plot(cv = cv_ridge,
                    model_type = "Ridge Regression",
                    fig_path = "./figs/Ridge.png")

# Estimated Test Accuracy:
test_acc_ridge = 1 - cv_ridge$cvm[which(cv_ridge$lambda == cv_ridge$lambda.1se)]
test_acc_se_ridge = cv_ridge$cvsd[which(cv_ridge$lambda == cv_ridge$lambda.1se)]

# Best model from Ridge - Logistic Regression:
regularized_logit_reg_plots(df_model = df_train_forward,
                            best_lambda = cv_ridge$lambda.1se,
                            model_type = "Ridge Regression",
                            fig_path = "./figs/Logit_Regr_Ridge.png")

###### Lasso - Logistic Regression

# Matrix data:
X = as.matrix(df_train_lasso[, p_predictors])
Y = df_train_lasso[, response_var]

# Fit:
fit_lasso = glmnet::glmnet(x = X,
                           y = Y,
                           alpha = 1,
                           standardize = FALSE,
                           family = "binomial")

# K-Fold Cross-validation:
cv_lasso = glmnet::cv.glmnet(x = X,
                             y = Y,
                             alpha = 1,
                             type.measure = "class",
                             nfolds = 10,
                             standardize = FALSE,
                             family = "binomial")

# Plot:
make_shrinkage_plot(cv = cv_lasso,
                    model_type = "The Lasso",
                    fig_path = "./figs/Lasso.png")

# Estimated Test Accuracy:
test_acc_lasso = 1 - cv_lasso$cvm[which(cv_lasso$lambda == cv_lasso$lambda.1se)]
test_acc_se_lasso = cv_lasso$cvsd[which(cv_lasso$lambda == cv_lasso$lambda.1se)]

# Best model from Lasso - Logistic Regression:
regularized_logit_reg_plots(df_model = df_train_forward,
                            best_lambda = cv_lasso$lambda.1se,
                            model_type = "The Lasso",
                            fig_path = "./figs/Logit_Regr_Lasso.png")

###### Principal Components Analysis - Logistic Regression

# Principal components from PCA:
pc = prcomp(df_train_pcalr[, p_predictors],
            center = TRUE,
            scale. = FALSE)

# K-Fold Cross-validated CER:
cv_CER = c()
npc = dim(pc$x)[2]
for(k in 1:npc){
    df_pc_k = data.frame(pc$x[, 1:k],
                         df_train_pcalr[response_var])
    cv_CER = c(cv_CER,
               cv_CER_logit(predictors = names(df_pc_k)[-(k + 1)],
                            df_model = df_pc_k))
}
df_eval = data.frame(
    "num_pc" = 1:npc,
    "cv_CER" = cv_CER
)
df_best = df_eval %>%
               dplyr::filter(cv_CER == min(df_eval$cv_CER))

# Plots:
make_pc_plot(pc = pc)
make_cv_pc_plot(df_eval = df_eval,
                df_best = df_best)

# Estimated Test Accuracy:
test_acc_pcalr = 1 - df_best$cv_CER
test_acc_se_pcalr = NA

# Best model from PCA-LR:
df_pc = data.frame(pc$x[, 1:df_best$num_pc],
                   df_train_pcalr[response_var])
logit_reg_plots(df_model = df_pc,
                model_type = "Logistic Regression with PCA",
                fig_path = "./figs/Logit_Regr_PCA.png")

###### Regularized Discriminant Analysis

df_train_rda











###### k-Nearest Neighbours




###### Comparison




############ Prediction

### Predictions by type of model

# Training/Test split:
q = 0.8
n_all = nrow(df_train_stand)
inds = sample(x = 1:n_all,
              size = trunc(q*n_all))
df_train = df_train_stand[inds, ]
df_test = df_train_stand[-inds, ]
X_train = as.matrix(df_train[, p_predictors])
Y_train = df_train[, response_var]
X_test = as.matrix(df_test[, p_predictors])
Y_test = df_test[, response_var]
df_pc_train = df_pc[inds, ]
df_pc_test = df_pc[-inds, ]

# Forward:
fit_forward = glm(formula = paste(response_var, "~.",
                                  collapse = ""),
                  family = "binomial",
                  data = df_train[, c(best_predictors, response_var)])
probs = predict(object = fit_forward,
                newdata = df_test[, p_predictors],
                type = "response") %>%
            as.numeric()
threshold = 0.5
y_pred_forward = ifelse(probs > threshold,
                        1,
                        0) %>%
                     as.numeric()

# Ridge:
fit_ridge = glmnet::glmnet(x = X_train,
                           y = Y_train,
                           alpha = 0,
                           standardize = FALSE,
                           family = "binomial",
                           lambda = cv_ridge$lambda.1se)
probs = predict(object = fit_ridge,
                newx = X_test,
                type = "response")
threshold = 0.5
y_pred_ridge = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# Lasso:
fit_lasso = glmnet::glmnet(x = X_train,
                           y = Y_train,
                           alpha = 0,
                           standardize = FALSE,
                           family = "binomial",
                           lambda = cv_lasso$lambda.1se)
probs = predict(object = fit_lasso,
                newx = X_test,
                type = "response")
threshold = 0.5
y_pred_lasso = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# PCA-LR:
fit_pcalr = glm(formula = paste(response_var, "~.",
                                collapse = ""),
                family = "binomial",
                data = df_pc_train)
probs = predict(object = fit_pcalr,
                newdata = df_pc_test,
                type = "response") %>%
            as.numeric()
threshold = 0.5
y_pred_pcalr = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# RDA:



# kNN:







# Ensemble model:
y_pred_ensemble = apply(
    X = data.frame(
            y_pred_forward,
            y_pred_ridge,
            y_pred_lasso,
            y_pred_pcalr
            # y_pred_rda,
            # y_pred_knn
        ),
    MARGIN = 1,
    FUN = getmode
)
test_acc_ensemble = score_accuracy(y_pred = y_pred_ensemble,
                                   y_real = Y_test)

# Compare the estimated scores:
df_models_score = data.frame(
    "models" = c("Forward Stepwise", "Ridge", "Lasso", "PCA LR",
                 #"RDA", "kNN"
                 "Ensemble"),
    "metric_name" = c(test_acc_forward, test_acc_ridge, test_acc_lasso, test_acc_pcalr,
                      # test_acc_rda, test_acc_knn,
                      test_acc_ensemble),
    "se_metric_name" = c(test_acc_se_forward, test_acc_se_ridge, test_acc_se_lasso, NA,
                         # NA, NA,
                         NA),
    stringsAsFactors = FALSE
)
make_best_models_plot(df_models = df_models_score,
                      metric = "Score")

###### Prediction with an ensemble model

df_test_final = df_test_stand %>%
                     dplyr::select(-all_of(id_var))

# Forward:
probs = predict(object = fit_forward,
                newdata = df_test_final,
                type = "response") %>%
            as.numeric()
threshold = 0.5
y_pred_forward = ifelse(probs > threshold,
                        1,
                        0) %>%
                     as.numeric()

# Ridge:
probs = predict(object = fit_ridge,
                newx = as.matrix(df_test_final),
                type = "response")
threshold = 0.5
y_pred_ridge = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# Lasso:
probs = predict(object = fit_lasso,
                newx = as.matrix(df_test_final),
                type = "response")
threshold = 0.5
y_pred_lasso = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# PCA-LR:
pca = prcomp(df_test_final, 
             center = TRUE,
             scale = FALSE)
df_pc_test = predict(pca,
                     newdata = df_test_final)
probs = predict(object = fit_pcalr,
                newdata = as.data.frame(df_pc_test),
                type = "response") %>%
            as.numeric()
threshold = 0.5
y_pred_pcalr = ifelse(probs > threshold,
                      1,
                      0) %>%
                   as.numeric()

# RDA:
# y_pred_rda = predict(fit_rda,
#                      df_test_final) %>%
#                  as.numeric()

# kNN:
# y_pred_knn = predict(fit_knn,
#                      df_test_final) %>%
#                  as.numeric()

# Ensemble:
y_pred_ensemble = apply(
    X = data.frame(
            y_pred_forward,
            y_pred_ridge,
            y_pred_lasso,
            y_pred_pcalr
            # y_pred_rda,
            # y_pred_knn
        ),
    MARGIN = 1,
    FUN = getmode
)
df_pred = data.frame(
    id = df_test_stand$PassengerId,
    resp = y_pred_ensemble
)
names(df_pred) = c(id_var, response_var)
write.csv(df_pred,
          file = "./data/submission_ensemble2.csv",
          row.names = FALSE)


