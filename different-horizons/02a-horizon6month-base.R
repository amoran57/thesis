# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)

infl_mbd <- embed(values_df$infl, 12)
infl_mbd <- as.data.frame(infl_mbd)

features <- as.character(seq(1:11))
for(i in 1:length(features)) {
  features[i] <- paste0("tmin", as.character(i))
}
names(infl_mbd) <- c("t", features)
rownames(infl_mbd) <- seq(1:nrow(infl_mbd))

#get formula call
ind <- glue::glue_collapse(x = features, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

#set parameters
penalties <- seq(0.7, 0.99, by = 0.005)
penalty <- 0.9
lib <- c("dplyr", "tictoc", "ggplot2")
formula <- call
feature_frac <- 0.7
sample_data <- TRUE
minsize <- NULL
data <- infl_mbd
n_trees <- 50
lag_name <- "tmin1"

#Functions -------------------------------------
#foundational
sse_var <- function(x, y) {
  this_df <- data.frame(x,y) %>% 
    dplyr::arrange(x)
  
  y <- this_df$y
  y_length <- length(y)
  
  sse <- c()
  
  #initialize values
  first_node <- c()
  first_mean <- 0
  first_sse <- 0
  second_node <- y
  second_mean <- mean(second_node)
  second_sse <- sum((second_node - second_mean)^2)
  
  for (i in 1:length(y)) {
    y_new <- y[i]
    
    #update values
    first_node <- c(first_node, y_new)
    first_mean <- (first_mean*(i - 1) + y_new)/i
    first_sse <- sum((first_node - first_mean)^2)
    second_node <- second_node[-1]
    second_mean <- (second_mean*(y_length - i + 1) - y_new)/(y_length - i)
    second_sse <- sum((second_node - second_mean)^2)
    sse[i] <- first_sse + second_sse
  }
  
  split_at <- this_df[which.min(sse),]$x
  return(c(sse = min(sse), split = split_at))
}
reg_tree <- function(formula, data, minsize = NULL, penalty = NULL) {
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)[,-1]
  
  # extract target
  y <- data[, as.character(formula)[2]]
  
  # initialize while loop
  do_splits <- TRUE
  
  # create output data.frame with splitting rules and observations
  tree_info <- data.frame(NODE = 1, NOBS = nrow(data), FILTER = NA, TERMINAL = "SPLIT",
                          stringsAsFactors = FALSE)
  
  # keep splitting until there are only leafs left
  while(do_splits) {
    
    # which parents have to be splitted
    to_calculate <- which(tree_info$TERMINAL == "SPLIT")
    
    for (j in to_calculate) {
      
      #initiate kill condition
      keep_going <- TRUE
      
      while(keep_going) {   
        # handle root node
        if (!is.na(tree_info[j, "FILTER"])) {
          # subset data according to the filter
          this_data <- subset(data, eval(parse(text = tree_info[j, "FILTER"])))
          # get the design matrix
          X <- model.matrix(formula, this_data)[,-1]
        } else {
          this_data <- data
        }
        
        #calculate current SSE
        this_y <- this_data[, as.character(formula)[2]]
        mean_this_y <- mean(this_y)
        this_sse <- sum((this_y - mean_this_y)^2)
        #update kill condition
        if(this_sse == 0) {
          split_here <- rep(FALSE, 2)
          keep_going <- FALSE
        }
        
        # estimate splitting criteria
        splitting <- apply(X,  MARGIN = 2, FUN = sse_var, y = this_y)
        
        # get the min SSE
        tmp_splitter <- which.min(splitting[1,])
        split_value <- splitting[2,tmp_splitter]
        split_value <- round(split_value, 9)
        new_sse <- splitting[1,tmp_splitter]
        improvement <- new_sse/this_sse
        
        # paste filter rules
        tmp_filter <- c(paste(names(tmp_splitter), ">", 
                              split_value),
                        paste(names(tmp_splitter), "<=", 
                              split_value))
        
        # Error handling! check if the splitting rule has already been invoked
        split_here  <- !sapply(tmp_filter,
                               FUN = function(x,y) any(grepl(x, x = y)),
                               y = tree_info$FILTER)
        
        if (!is.null(penalty)) {
          #check for valid split based on minsize or penalty
          if (is.na(improvement) | improvement > penalty) {
            split_here <- rep(FALSE, 2)
            #update kill condition
            keep_going <- FALSE
          }
        }
        
        # define maxnode
        mn <- max(tree_info$NODE)
        
        #update kill condition
        if(keep_going) {
          keep_going <- ifelse(all(split_here), TRUE, FALSE)
        }
        
        if (!is.na(tree_info[j, "FILTER"])) {
          # append the splitting rules
          tmp_filter  <- paste(tree_info[j, "FILTER"], 
                               tmp_filter, sep = " & ")
        }
        
        # get the number of observations in current node
        tmp_nobs <- sapply(tmp_filter,
                           FUN = function(i, x) {
                             nrow(subset(x = x, subset = eval(parse(text = i))))
                           },
                           x = this_data)
        
        if(any(tmp_nobs < 11)) {
          split_here <- rep(FALSE, 2)
        }
        #end while loop
        keep_going <- FALSE
      }
      
      
      # create children data frame
      children <- data.frame(NODE = c(mn+1, mn+2),
                             NOBS = tmp_nobs,
                             FILTER = tmp_filter,
                             TERMINAL = c(ifelse(tmp_nobs[1] > 1, "SPLIT", "LEAF"), ifelse(tmp_nobs[2] > 1, "SPLIT", "LEAF")),
                             row.names = NULL)[split_here,]
      
      # overwrite state of current node
      tree_info[j, "TERMINAL"] <- ifelse(all(!split_here), "LEAF", "PARENT")
      
      # bind everything
      tree_info <- rbind(tree_info, children)
      
      # check if there are any open splits left
      do_splits <- !all(tree_info$TERMINAL != "SPLIT")
    } # end for
  } # end while
  
  
  # calculate fitted values, sorting criteria, and predictions for each criterion
  leafs <- tree_info[tree_info$TERMINAL == "LEAF", ]
  fitted <- c()
  criteria <- c()
  predictions <- c()
  for (i in seq_len(nrow(leafs))) {
    criterion <- leafs[i, "FILTER"]
    # extract index
    if(!is.na(criterion)) {
      ind <- as.numeric(rownames(subset(data, eval(parse(text = criterion)))))
    } else {
      ind <- as.numeric(rownames(data))
    }
    
    # estimator is the mean y value of the leaf
    mean_ind <- mean(y[ind])
    fitted[ind] <- mean_ind
    predictions[i] <- mean_ind
    criteria[i] <- criterion
  }
  
  pred <- data.frame(criteria, predictions)
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data, pred = pred, penalty = penalty))
}
sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalty = NULL) {
  # extract features
  features <- all.vars(formula)[-1]
  # extract target
  target <- all.vars(formula)[1]
  #add data trend
  data$trend <- seq(1:nrow(data))
  features <- c(features, "trend")
  # bag the data
  # - randomly sample the data with replacement (duplicate are possible)
  if (sample_data == TRUE) {
    train <- data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
  } else {
    train <- data
  }
  train <- dplyr::arrange(train, trend)
  rownames(train) <- seq(1:nrow(train))
  # randomly sample features
  # - only fit the regression tree with feature_frac * 100 % of the features
  features_sample <- sample(features,
                            size = ceiling(length(features) * feature_frac),
                            replace = FALSE)
  # create new formula
  formula_new <-
    as.formula(paste0(target, " ~ ", paste0(features_sample,
                                            collapse =  " + ")))
  # fit the regression tree
  if(!is.null(penalty)) {
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     penalty = penalty)
  } else if (is.null(minsize)) {
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = ceiling(nrow(train) * 0.1))
  } else {
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = minsize)
  }
  
  # return the tree
  return(tree)
}

#forest
parallel_reg_rf <- function(formula, n_trees = 50, feature_frac = 0.7, sample_data = TRUE, minsize = NULL, data, penalty = NULL) {
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  
  formula <- formula
  n_trees <- n_trees
  feature_frac <- feature_frac
  sample_data <- sample_data
  minsize <- minsize
  data <- data
  penalty <- penalty
  
  split <- floor(detectCores()/1.2)
  print(paste0("Cores to use: ", as.character(split)))
  tic("Parallel")
  if(n_trees < split) {
    print("Will only run once")
    #reduce split and run only once
    split <- n_trees
    cl <- makeCluster(split)
    registerDoParallel(cl)
    clusterExport(cl, c("lib", "formula", "n_trees", "feature_frac", "sample_data", 
                        "minsize", "data", "penalty", "sprout_tree", 
                        "reg_tree", "sse_var"))
    init <- clusterEvalQ(cl, lapply(lib, require, character.only = TRUE))
    
    trees <- foreach(
      rep(1, split),
      .combine = list,
      .multicombine = TRUE) %dopar%
      sprout_tree(
        formula = formula,
        feature_frac = feature_frac,
        sample_data = sample_data,
        minsize = minsize,
        data = data,
        penalty = penalty
      )
    
    
    stopCluster(cl)
  } else {
    iterate <- ceiling(n_trees/split)
    print(paste0("Will run ", as.character(iterate), " times"))
    trees <- list()
    cl <- makeCluster(split)
    registerDoParallel(cl)
    clusterExport(cl, c("lib", "formula", "n_trees", "feature_frac", "sample_data", 
                        "minsize", "data", "penalty", "sprout_tree", 
                        "reg_tree", "sse_var"))
    init <- clusterEvalQ(cl, lapply(lib, require, character.only = TRUE))
    
    for(i in 1:iterate) {
      tic(paste0("batch"," ", as.character(i)))
      these_trees <- foreach(
        rep(1, split),
        .combine = list,
        .multicombine = TRUE) %dopar%
        sprout_tree(
          formula = formula,
          feature_frac = feature_frac,
          sample_data = sample_data,
          minsize = minsize,
          data = data,
          penalty = penalty
        )
      
      trees <- c(trees, these_trees)
      toc()
    }
    
    stopCluster(cl)
    
  }
  toc()
  return(trees)
}

#prediction
get_prediction <- function(forest, X_test) {
  num_trees <- length(forest)
  all_predictions <- c()
  
  for (i in 1:num_trees) {
    #get each tree from the forest
    temp_tree <- forest[[i]]
    temp_tree_pred <- temp_tree$pred
    temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
    
    #get appropriate row from tree_info
    tf <- c()
    if(nrow(temp_tree_pred) == 1) {
      tf <- TRUE
    } else {
      for(j in 1:nrow(temp_tree_pred)) {
        f <- eval(parse(text = temp_tree_pred$criteria[j]), envir = X_test)
        tf <- c(tf, f)
      }
    }
    
    #get constant and beta_hat and predict
    temp_pred <- temp_tree_pred[tf,]
    all_predictions[i] <- temp_pred$predictions
  }
  #get the forest prediction and return it
  forest_prediction <- mean(all_predictions)
  return(forest_prediction)
}
#Predict using random forest method --------------------------------------
monthly_dates <- seq(as.Date("1999/1/1"), as.Date("2020/1/1"), "month")
lag_order <- 12
variables <- all.vars(call)
variables[1] <- "trend"
forecasts_rf <- c()

tic("expanding horizon forest")
for (k in 1:length(monthly_dates)) {
  monthx <- monthly_dates[k]
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    dplyr::filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  raw_mbd <- embed(train_tsData, lag_order)
  raw_mbd <- as.data.frame(raw_mbd)
  
  #adjust for the 3 month horizon
  this_y <- raw_mbd[,1]
  this_x <- raw_mbd[,-1]
  
  adj_y <- this_y[-c(1:5)]
  adj_x <- this_x[-c((nrow(this_x)-4):nrow(this_x)),]
  
  infl_mbd <- data.frame(adj_y, adj_x)
  names(infl_mbd) <- c("t", features)
  
  #set training and test sets
  X_test <- infl_mbd[nrow(infl_mbd), ]
  X_test$trend <- nrow(infl_mbd)
  infl_mbd <- infl_mbd[-nrow(infl_mbd),]
  
  #fit the forest
  timestamp()
  tic(paste0("Bayesian forest iteration ", as.character(k), " complete"))
  bayes <- parallel_reg_rf(formula, sample_data = sample_data, data = infl_mbd, penalty = penalty)
  toc()
  
  #get the prediction
  predict_rf <- get_prediction(forest = bayes, X_test = X_test)
  
  forecasts_rf[k] <- predict_rf
}
toc()

forest_forecast_ts <- ts(forecasts_rf, start = c(1999, 1), frequency = 12)

#Predict using ARIMA -----------------------------
arima_monthly_dates <- seq(as.Date("1998/7/1"), as.Date("2019/7/1"), "month") 
pred_ar1 <- c()
for (k in 1:length(arima_monthly_dates)) {
  monthx <- arima_monthly_dates[k]
  #initialize training data according to expanding horizon
  train_df <- values_df %>%
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  pred_a <- forecast(arima(train_tsData, order = c(1,0,0)), 6)$mean[6]
  
  pred_ar1[k] <- pred_a
}
pred_arima <- ts(pred_ar1, start = c(1999,1), frequency = 12)

naive <- window(tsData, start = c(1998, 7), end = c(2019, 7))
naive_forecast <- ts(naive, start = c(1999, 1), frequency = 12)

#Compare ----------------------------------------
accuracy(tsData, forest_forecast_ts)
accuracy(tsData, pred_arima)
accuracy(tsData, naive_forecast)

#Export ----------------------------------
write_rds(forest_forecast_ts, paste0(export,"different_horizons/horizon6month/base_forecast.rds"))
write_rds(pred_arima, paste0(export, "different_horizons/horizon6month/ar1_forecast.rds"))
write_rds(naive, paste0(export, "different_horizons/horizon6month/naive_forecast"))
