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
    
    #get test and training data.frames
    train_df <- train[1:(nrow(train)-48), all.vars(formula_new)]
    test_df <- train[(nrow(train)-47):nrow(train), all.vars(formula_new)]
    rownames(test_df) <- seq(1:nrow(test_df))
    
    rmses <- c()
    for(penalty in penalties) {
      #get a tree built on the training data and the current penalty
      temp_tree <- reg_tree(formula_new, train_df, minsize = NULL, penalty = penalty)
      temp_tree_pred <- temp_tree$pred
      temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
      
      #predict each value in test_df
      tree_predictions <- c()
      
      while(length(tree_predictions) < 48 | any(is.na(tree_predictions))) {
        
        if(nrow(temp_tree_pred) > 1) {
          #for every prediction in temp_tree_pred
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
    
    graph_df <- data.frame(rmses, penalties)
    plot <- ggplot(data = graph_df, aes(x = penalties, y = rmses)) +
      geom_line() +
      ylim(min(rmses)/1.2, max(rmses)*1.2)
    
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
  return(list(tree = tree, penalty_plot = plot))
}
bayesian_sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
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
    
    #get test and training data.frames
    train_df <- train[1:(nrow(train)-48), all.vars(formula_new)]
    test_df <- train[(nrow(train)-47):nrow(train), all.vars(formula_new)]
    rownames(test_df) <- seq(1:nrow(test_df))
    
    min_penalty <- min(penalties)
    max_penalty <- max(penalties)
    
    #get initial penalty values to try
    set.seed(90210)
    N <- length(penalties)/5
    N <- ifelse(N %% 2 == 0, N, N + 1)
    rand_penalties <- runif(N, min = min(penalties), max = max(penalties))
    rmses <- c()
    
    #get RMSEs for those penalties
    for(penalty in rand_penalties) {
      #get a tree built on the training data and the current penalty
      temp_tree <- reg_tree(formula_new, train_df, minsize = NULL, penalty = penalty)
      temp_tree_pred <- temp_tree$pred
      temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
      
      #predict each value in test_df
      tree_predictions <- c()
      
      while(length(tree_predictions) < 48 | any(is.na(tree_predictions))) {
        
        if(nrow(temp_tree_pred) > 1) {
          #for every prediction in temp_tree_pred
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
    
    #split into two groups
    rand_penalty_df <- data.frame(rmses, rand_penalties) %>% 
      dplyr::arrange(rmses)
    sorted_rmses <- rand_penalty_df$rmses
    split_options <- unique(sorted_rmses)
    differences <- c()
    for(i in 1:length(split_options)) {
      ind <- sorted_rmses <= split_options[i]
      count_lower <- sum(ind)
      count_higher <- sum(!ind)
      difference <- abs(count_lower - count_higher)
      differences <- c(differences, difference)
    }
    
    split_at <- which.min(differences)
    split_here <- split_options[split_at]
    ind <- sorted_rmses <= split_here
    
    l_function <- sort(rand_penalty_df$rand_penalties[ind]) 
    g_function <- sort(rand_penalty_df$rand_penalties[!ind])
    l_distribution <- data.frame(penalties)
    g_distribution <- data.frame(penalties)
    uniform_distribution <- unique(dunif(penalties, min = min_penalty, max = max_penalty))
    #get distributions for l_function
    for(l in 1:length(l_function)) {
      mean <- l_function[l]
      ses <- c(abs(l_function[l]-l_function[l-1]), 
              abs(l_function[l]-l_function[l+1]),
              abs(l_function[l]-min_penalty),
              abs(l_function[l]-max_penalty))
      se <- min(ses, 0.075, na.rm = TRUE)
      se <- ifelse(se > 0.01, se, 0.01)
      dist <- dnorm(penalties, mean = mean, sd = se)
      normalize_by <- 100/sum(dist)
      dist <- dist*normalize_by
      dist <- dnorm(penalties, mean = mean, sd = se)
      avg_dist <- (dist + uniform_distribution)/2
      dist_df <- data.frame(avg_dist, penalties)
      l_distribution <- left_join(l_distribution, dist_df, by = "penalties")
    }
    l_distribution$mean <- rowMeans(l_distribution[2:length(l_distribution)])
    # l_plot <- ggplot(data = l_distribution, aes(x = penalties, y = mean)) +
    #   geom_point()
    # 
    # l_plot
    
    #get distributions for g_function
    for(g in 1:length(g_function)) {
      mean <- g_function[g]
      ses <- c(abs(g_function[g]-g_function[g-1]), 
               abs(g_function[g]-g_function[g+1]),
               abs(g_function[g]-min_penalty),
               abs(g_function[g]-max_penalty))
      se <- min(ses, 0.075, na.rm = TRUE)
      se <- ifelse(se > 0.01, se, 0.01)
      dist <- dnorm(penalties, mean = mean, sd = se)
      normalize_by <- 100/sum(dist)
      dist <- dist*normalize_by
      avg_dist <- (dist + uniform_distribution)/2
      dist_df <- data.frame(avg_dist, penalties)
      g_distribution <- left_join(g_distribution, dist_df, by = "penalties")
    }
    g_distribution$mean <- rowMeans(g_distribution[2:length(g_distribution)])
    # g_plot <- ggplot(data = g_distribution, aes(x = penalties, y = mean)) +
    #   geom_point()
    # 
    # g_plot
    
    #now we draw many candidates from l_distribution
    
    
    
    
    
    
    
    
    
    
    rmses <- c()
    for(penalty in penalties) {
      #get a tree built on the training data and the current penalty
      temp_tree <- reg_tree(formula_new, train_df, minsize = NULL, penalty = penalty)
      temp_tree_pred <- temp_tree$pred
      temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
      
      #predict each value in test_df
      tree_predictions <- c()
      
      while(length(tree_predictions) < 48 | any(is.na(tree_predictions))) {
        
        if(nrow(temp_tree_pred) > 1) {
          #for every prediction in temp_tree_pred
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
    
    graph_df <- data.frame(rmses, penalties)
    plot <- ggplot(data = graph_df, aes(x = penalties, y = rmses)) +
      geom_line() +
      ylim(min(rmses)/1.2, max(rmses)*1.2)
    
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
  return(list(tree = tree, penalty_plot = plot))
}
