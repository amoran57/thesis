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
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
rownames(infl_mbd) <- seq(1:nrow(infl_mbd))

#get formula call
call <- as.character(seq(1:(length(infl_mbd)- 1)))
for(i in 1:length(call)) {
  call[i] <- paste0("tmin", as.character(i))
}

ind <- glue::glue_collapse(x = call, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

penalties <- seq(0.75, 0.99, by = 0.01)
feature_frac <- 1
data <- infl_mbd[sample(1:nrow(infl_mbd), size = nrow(infl_mbd), replace = TRUE),]
sample_data <- FALSE


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
        
        if(any(tmp_nobs < 2)) {
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
sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
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
  if(!is.null(penalties)) {
    # "cross-validate" by testing the tree for each penalty over the most recent four years
    rmses <- c()
    for(penalty in penalties) {
      #get test and training data.frames
      train_df <- train[1:(nrow(train)-48), all.vars(formula_new)]
      test_df <- train[(nrow(train)-47):nrow(train), all.vars(formula_new)]
      rownames(test_df) <- seq(1:nrow(test_df))
      
      #get a tree built on the training data and the current penalty
      temp_tree <- reg_tree(formula_new, train_df, minsize = NULL, penalty = penalty)
      temp_tree_pred <- temp_tree$pred
      temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
      
      #predict each value in test_df
      if(nrow(temp_tree_pred) > 1) {
        tree_predictions <- c()
        for(j in 1:nrow(test_df)) {
          tf <- c()
          for(i in 1:nrow(temp_tree_pred)) {
            f <- eval(parse(text = temp_tree_pred$criteria[i]), envir = test_df[j,])
            tf <- c(tf, f)
          }
          temp_tree_pred$tf <- tf
          temp_pred <- temp_tree_pred %>% 
            dplyr::filter(tf)
          
          #append to vector
          tree_predictions <- c(tree_predictions, temp_pred$predictions)
        }
      } else {
        tree_predictions <- rep(temp_tree_pred[1, "predictions"], nrow(test_df))
      }
      
      #get the RMSE for that penalty
      ind_var <- as.character(target)
      temp_df <- data.frame(tree_predictions, test_df[ind_var])
      names(temp_df) <- c("prediction", "real")
      temp_rmse <- ModelMetrics::rmse(temp_df$real, temp_df$prediction)
      rmses <- c(rmses, temp_rmse)
    }
    
    #find the penalty which minimizes RMSE
    penalty_df <- t(data.frame(rmses, penalties))
    best_penalty <- which.min(penalty_df[1,])
    penalty <- penalty_df[2, best_penalty]
    
    #build a tree on the full subsample using that penalty
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     penalty = penalty)
    
  } else if (is.null(minsize)) {
    #if penalties and minsize are both null, use a rough minsize
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = ceiling(nrow(train) * 0.1))
  } else {
    #if penalties is null but minsize exists, use minsize
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = minsize)
  }
  
  # return the tree
  return(tree)
}
new_sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
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
  if(!is.null(penalties)) {
    # "cross-validate" by testing the tree for each penalty over the most recent four years
    rmses <- c()
    for(penalty in penalties) {
      #get test and training data.frames
      train_df <- train[1:(nrow(train)-48), all.vars(formula_new)]
      test_df <- train[(nrow(train)-47):nrow(train), all.vars(formula_new)]
      rownames(test_df) <- seq(1:nrow(test_df))
      
      #get a tree built on the training data and the current penalty
      temp_tree <- reg_tree(formula_new, train_df, minsize = NULL, penalty = penalty)
      temp_tree_pred <- temp_tree$pred
      temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
      
      #predict each value in test_df
      tree_predictions <- c()
      
      while(length(tree_predictions) < 48 | any(is.na(tree_predictions))) {
      if(nrow(temp_tree_pred) > 1) {
        for (i in seq_len(nrow(temp_tree_pred))) {
          criterion <- temp_tree_pred[i, "criteria"]
          # extract indices
          if(!is.na(criterion)) {
            ind <- as.numeric(rownames(subset(test_df, eval(parse(text = criterion)))))
          } else {
            ind <- as.numeric(rownames(data))
          }
          #update tree predictions
          tree_predictions[ind] <- temp_tree_pred[i, "predictions"]
        }
      } else {
        tree_predictions <- rep(temp_tree_pred[1, "predictions"], nrow(test_df))
      }
    }
      
      #get the RMSE for that penalty
      ind_var <- as.character(target)
      temp_df <- data.frame(tree_predictions, test_df[ind_var])
      names(temp_df) <- c("prediction", "real")
      temp_rmse <- ModelMetrics::rmse(temp_df$real, temp_df$prediction)
      rmses <- c(rmses, temp_rmse)
    }
    
    penalty_df <- t(data.frame(rmses, penalties))
    best_penalty <- which.min(penalty_df[1,])
    penalty <- penalty_df[2, best_penalty]
    
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
reg_rf <- function(formula, n_trees = 50, feature_frac = 0.7, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  trees <- plyr::raply(
    n_trees,
    new_sprout_tree(
      formula = formula,
      feature_frac = feature_frac,
      sample_data = sample_data,
      minsize = minsize,
      data = data,
      penalties = penalties
    ),
    .progress = "text"
  )
  
  return(trees)
}

tic("slow")
slow <- plyr::raply(
  1,
  sprout_tree(call, feature_frac, sample_data, data = data, penalties = penalties),
  .progress = "text"
)
toc()

tic("fast")
fast <- plyr::raply(
  1,
  new_sprout_tree(call, feature_frac, sample_data, data = data, penalties = penalties),
  .progress = "text"
)
toc()

#isolate problematic subsamples
for(i in 1:1000) {
  formula <- call
  data <- infl_mbd
  feature_frac <- 0.7
  features <- all.vars(formula)[-1]
  # extract target
  target <- all.vars(formula)[1]
  #add data trend
  data$trend <- seq(1:nrow(data))
  features <- c(features, "trend")

  train <- data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]

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
  tree <- reg_tree(formula_new, train, penalty = 0.9)
}
