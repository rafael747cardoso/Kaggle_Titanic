
### Feature engineering (problem specific, but the same to train and test)

feature_eng = function(X, var_cat, var_num){
    
    ### Name
    
    # Is married:
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
    X$married = ifelse(title %in% c("Lady", "Mme", "Mrs"),
                       "yes",
                       "no")

    # Title:
    title[title %in% c("Mlle", "Ms")] = "Miss"
    title[title == "Mme"] = "Mrs"
    title[title %in% c("Don", "Jonkheer", "Lady", "Sir", "the Countess", "Dr", "Rev")] = "Elite"
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
    
    # With spouse onboard:
    X$with_spouse = NA
    lastnames = sort(unique(lastname))
    temp_id = 1:nrow(X)
    X$temp_id = temp_id
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
            ind = df_i$temp_id[j]
            if(firstnames[j] %in% appeared){
                X$with_spouse[which(X$temp_id == ind)] = "yes"
            } else{
                X$with_spouse[which(X$temp_id == ind)] = "no"
            }
            appeared = c(appeared,
                         firstnames[j])
        }
    }
    X = X %>%
            dplyr::select(-Name,
                          -lastname,
                          -temp_id)

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
    
    return(X)
}

