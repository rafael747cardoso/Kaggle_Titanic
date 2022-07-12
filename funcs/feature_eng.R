
### Feature engineering (problem specific, but the same to train and test)

feature_eng = function(X, var_cat, var_num){
    
    ### Name
    
    # Title:
    passenger_names = X$Name
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
    X$Name = passenger_names
    lastname = c()
    title = c()
    for(i in 1:nrow(X)){
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
    X$title = title
    
    # Number of last name relatives:
    X$lastname = lastname
    df_relatives = X %>%
                       dplyr::group_by(lastname) %>%
                       dplyr::summarise(lastname_relatives = n()) %>%
                       as.data.frame()
    X = X %>%
                 dplyr::left_join(df_relatives,
                                  by = c("lastname" = "lastname"))
    
    # Number of names:
    number_names = c()
    for(i in 1:nrow(X)){
        passenger_name = passenger_names[i]
        all_names = strsplit(x = passenger_name,
                             split = " ")[[1]][-2]
        number_names = c(number_names,
                         length(all_names))
    }
    X$number_names = number_names
    
    # Spouses by name:
    X$with_spouse = NA
    lastnames = sort(unique(lastname))
    for(i in 1:length(lastnames)){
        person_lastname = lastnames[i]
        df_i = X %>%
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
                X$with_spouse[which(X$PassengerId == ind)] = "yes"
            } else{
                X$with_spouse[which(X$PassengerId == ind)] = "no"
            }
            appeared = c(appeared,
                         firstnames[j])
        }
    }
    X = X %>%
                 dplyr::select(-Name,
                               -lastname)
    
    ###  Ticket
    
    # Simplify the levels:
    X$Ticket = as.factor(X$Ticket)
    tickets = levels(X$Ticket)
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
    levels(X$Ticket) = tickets
    X$Ticket = as.character(X$Ticket)
    
    ### Cabin
    
    # Simplify the levels to Deck:
    X$Cabin[X$Cabin == ""] = NA
    X$Cabin = as.factor(X$Cabin)
    cabins = levels(X$Cabin)
    before = c("A", "B", "C", "D", "E", "F", "G", "T")
    after = c("A", "B", "C", "D", "E", "F", "F", "F")
    for(i in 1:length(before)){
        cabins[substr(x = cabins,
                      start = 1,
                      stop = 1) == before[i]] = after[i]
    }
    levels(X$Cabin) = cabins
    X$Cabin = as.character(X$Cabin)
    X = X %>%
                 dplyr::rename(Deck = Cabin)
    
    ### K-Nearest Neighbors to fill in the gaps in Deck
    
    # Submodel train and test sets:
    resp = "Deck"
    df_withdeck = X %>%
                      dplyr::filter(!is.na(Deck))
    df_nodeck = X %>%
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
    X = rbind(df_withdeck,
                   df_nodeck)
    
    ### Embarked
    
    X$Embarked[X$Embarked == "" & boo_train] = getmode(X[boo_train, "Embarked"])
    X$Embarked[X$Embarked == "" & !boo_train] = getmode(X[boo_train, "Embarked"])
    
        
    
    
    
    
    
    
}
