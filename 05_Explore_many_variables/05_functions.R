# Functions

# Generate models (formula)
generate.models <- function(predictors.vars, outcome.var){
  df.models <- NULL # here we store all possible models
  model.count <- 1 # counter for model 
  
  for(nr.predictors in 1:length(predictors.vars)){ # change number of predictors
    predictors <- combn(x = predictors.vars, m = nr.predictors) # generate all possible combinations of predictors
    for(combination in 1:ncol(predictors)){ # loop over every combination of predictors
      predictors.list <- paste0(predictors[,combination], collapse = "|") # all predictors
      formula <- paste0(outcome.var,"~",paste0(predictors[,combination], collapse = "+")) # model formula
      df.models <- rbind(df.models, c(model.count, nr.predictors, predictors.list, formula))
      model.count <- model.count + 1 # increase model count
    }
  }
  
  colnames(df.models) <- c("id", "nr. predictors", "predictors", "formula") # column names
  
  df.models <- df.models %>% 
    as.data.frame() %>% # convert to data frame
    mutate(id = as.numeric(as.character(id)), # convert to numeric
           `nr. predictors` = as.numeric(as.character(`nr. predictors`)), # convert to numeric
           formula = as.character(formula)) # convert to character
           
  return(df.models)
}

# Split sample (train ~ test)
split.sample <- function(df, train.size = 0.8){
  id.rows <- 1:nrow(df) # all rows ids
  id.train <- sample(x = id.rows, size = round(train.size * nrow(df)), replace = F) # train rows
  id.test <- setdiff(id.rows, id.train) # test rows
  
  # write back to data frame
  df[id.train,"sample"] <- "train"
  df[id.test,"sample"] <- "test"
  
  return(df)
}
