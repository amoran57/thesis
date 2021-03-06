# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Set up for tree -------------------------------
yt <- read_rds(paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
y_mbd <- as.data.frame(embed(yt, 12))
names(y_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
zt <- read_rds(paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
z_mbd <- as.data.frame(embed(zt, 12))
names(z_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
#get formula call
call <- as.character(seq(1:(length(y_mbd)- 1)))
for(i in 1:length(call)) {
  call[i] <- paste0("tmin", as.character(i))
}

ind <- glue::glue_collapse(x = call, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

penalties <- seq(0.7, 0.99, by = 0.005)
formula <- call
feature_frac <- 1
sample_data <- FALSE
minsize <- NULL
data <- y_mbd

#Tree model -------------------------------------
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

#bayesian
get_rmses <- function(these_penalties, formula, train_df, test_df, target) {
  rmses <- c()
  for(penalty in these_penalties) {
    #get a tree built on the training data and the current penalty
    temp_tree <- reg_tree(formula, train_df, minsize = NULL, penalty = penalty)
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
  return(rmses)
}
split_lg <- function(these_rmses, these_penalties) {
  rand_penalty_df <- data.frame(rmses = these_rmses, penalties = these_penalties) %>% 
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
  
  
  l_function <- sort(rand_penalty_df$penalties[ind]) 
  g_function <- sort(rand_penalty_df$penalties[!ind])
  
  return(list(ind = ind, split_here = split_here, l_function = l_function, g_function = g_function))
}
get_distribution <- function(these_penalties, min_penalty, max_penalty, uniform_distribution) {
  this_distribution <- data.frame(penalties)
  for(l in 1:length(these_penalties)) {
    mean <- these_penalties[l]
    ses <- c(abs(these_penalties[l]-these_penalties[l-1]), 
             abs(these_penalties[l]-these_penalties[l+1]),
             abs(these_penalties[l]-min_penalty),
             abs(these_penalties[l]-max_penalty))
    se <- min(ses, 0.075, na.rm = TRUE)
    se <- ifelse(se > 0.01, se, 0.01)
    dist <- dnorm(penalties, mean = mean, sd = se)
    normalize_by <- 100/sum(dist)
    dist <- dist*normalize_by
    dist <- dnorm(penalties, mean = mean, sd = se)
    avg_dist <- (dist + uniform_distribution)/2
    dist_df <- data.frame(avg_dist, penalties)
    this_distribution <- left_join(this_distribution, dist_df, by = "penalties")
  }
  this_distribution$mean <- rowMeans(this_distribution[2:length(this_distribution)])
  return(this_distribution)
}
generate_custom_random <- function(l_distribution) {
  #first we create a mapping data.frame to map from a uniform distribution to our custom one
  normalize_by <- 100/sum(l_distribution$mean)
  l_distribution$mean <- l_distribution$mean*normalize_by
  l_distribution$cumsum <- cumsum(l_distribution$mean)
  l_distribution$rule <- paste0("randNum <= ", as.character(l_distribution$cumsum))
  l_distribution$other_rule <- c("randNum > 0", paste0("randNum > ", as.character(l_distribution$cumsum[-nrow(l_distribution)])))
  l_distribution$rule <- paste0(l_distribution$rule, " & ", l_distribution$other_rule)
  l_map <- data.frame(penalties = l_distribution$penalties, rule = l_distribution$rule)
  l_map$rule <- as.character(l_map$rule)
  
  #then we generate 10 random numbers from a uniform distribution
  rand_unis <- runif(10, min = 0, max = 100)
  new_penalties <- c()
  
  #and we check them against our mapping function
  for(randNum in rand_unis) {
    tf <- c()
    for(i in 1:nrow(l_map)) {
      f <- eval(parse(text = l_map$rule[i]))
      tf <- c(tf, f)
    }
    #get the penalty from our custom distribution
    new_penalty <- l_map$penalties[tf]
    new_penalties <- c(new_penalties, new_penalty)
  }
  
  new_penalties <- sort(new_penalties)
  
  return(new_penalties)
}
evaluate_penalties <- function(these_penalties, l_distribution, g_distribution) {
  #we have our penalties, selected from l_distribution. Now we evaluate them by l_distribution/g_distribution
  evaluate <- data.frame(penalties = these_penalties)
  evaluate <- left_join(evaluate, l_distribution, by = "penalties")
  evaluate <- data.frame(penalties = these_penalties, l_mean = evaluate$mean)
  evaluate <- left_join(evaluate, g_distribution, by = "penalties")
  evaluate <- data.frame(penalties = these_penalties, l_mean = evaluate$l_mean, g_mean = evaluate$mean)
  evaluate$score <- evaluate$l_mean/evaluate$g_mean
  
  #get the best score and fit a tree for it
  evaluate <- evaluate[order(-evaluate$score),]
  unique_scores <- unique(evaluate$score)
  best_vector <- evaluate$score == unique_scores[1]
  next_best_vector <- evaluate$score == unique_scores[2]
  third_best_vector <- evaluate$score == unique_scores[3]
  best_penalty <- unique(evaluate$penalties[best_vector])
  next_best_penalty <- unique(evaluate$penalties[next_best_vector])
  third_best_penalty <- unique(evaluate$penalties[third_best_vector])
  
  return(c(best_penalty, next_best_penalty, third_best_penalty))
}
bayesian_sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
  # extract features
  features <- all.vars(formula)[-c(1:2)]
  # extract target
  target <- all.vars(formula)[1]
  #make sure we include first lag
  first_lag <- all.vars(formula)[2]
  #add data trend
  data$trend <- seq(1:nrow(data))
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
    as.formula(paste0(target, " ~ ", first_lag, " + trend + ", paste0(features_sample,
                                                                      collapse =  " + ")))
  # fit the regression tree
  if(!is.null(penalties)) {
    # "cross-validate" by testing the tree for each penalty over the most recent four years
    
    #get test and training data.frames
    train_df <- train[1:(nrow(train)-48), all.vars(formula_new)]
    test_df <- train[(nrow(train)-47):nrow(train), all.vars(formula_new)]
    rownames(test_df) <- seq(1:nrow(test_df))
    
    tic("Bayes")
    
    min_penalty <- min(penalties)
    max_penalty <- max(penalties)
    uniform_distribution <- unique(dunif(penalties, min = min_penalty, max = max_penalty))
    
    #get initial penalty values to try
    N <- ceiling(length(penalties)/5)
    N <- ifelse(N %% 2 == 0, N, N + 1)
    # Theoretically we'd use random parameter values, but in a small one-dimensional parameter space like ours
    # I think this initial grid search makes sense
    rand_penalty_index <- round(seq(1, length.out = N, by = length(penalties)/N))
    rand_penalties <- penalties[rand_penalty_index]
    # rand_penalties <- round(runif(N, min = min_penalty*100, max = max_penalty*100))/100
    
    #get scores from random penalties above
    rmses <- get_rmses(rand_penalties, formula_new, train_df, test_df, target)
    
    #create an object to store penalties and scores
    history <- data.frame(penalties = rand_penalties, score = rmses)
    
    #this Bayesian scheme will only run 3/5 as many evaluations as a gridwise search
    num_iterations <- ceiling((ceiling(0.5*length(penalties)) - N)/3)
    
    #this is the part to iterate
    #now we have our initial conditions
    #to proceed:
    #step 1 -- split our history into two penalty vectors
    #step 2 -- get the distributions for each penalty vector
    #step 3 -- generate 10 random penalties from the l_distribution
    #step 4 -- see which of the 10 penalties are the best
    #step 5 -- get the RMSE from those penalty
    #step 6 -- feed it back into our history and start over
    #step 7 -- start over! get 10 random penalties from the l_distribution, etc.
    
    for(i in 1:num_iterations) {
      #step 1
      split <- split_lg(history$score, history$penalties)
      l_function <- split$l_function
      g_function <- split$g_function
      
      #step 2
      l_distribution <- get_distribution(l_function, min_penalty, max_penalty, uniform_distribution)
      if(length(l_function) != nrow(history)) {
        g_distribution <- get_distribution(g_function, min_penalty, max_penalty, uniform_distribution)
      } else {
        g_distribution <- data.frame(penalties, mean = rep(uniform_distribution))
      }
      
      
      #step 3
      new_random_penalties <- generate_custom_random(l_distribution)
      #step 4
      next_penalties <- evaluate_penalties(new_random_penalties, l_distribution, g_distribution)
      #step 5
      new_rmses <- c()
      for(next_penalty in next_penalties) {
        #let's check if we've already evaluated this penalty
        if(any(history$penalties == next_penalty)) {
          #if we've already seen this penalty, no need to calculate the tree again
          next_rmse <- history$score[history$penalties == next_penalty]
        } else {
          #if we haven't seen it yet, we'll calculate the RMSE
          next_rmse <- get_rmses(next_penalty, formula_new, train_df, test_df, target)
        }
        new_rmses <- c(new_rmses, next_rmse)
        #step 6
        history <- rbind(history, c(next_penalty, next_rmse))
      }
    }
    
    #now we have half of the total possible iterations -- good enough for me!
    
    #grab the "best" penalty
    best_penalties <- evaluate_penalties(penalties, l_distribution, g_distribution)
    best_penalty <- best_penalties[1]
    
    bayes_done <- toc()
    bayes_time <- bayes_done$toc - bayes_done$tic
    
    l_plot <- ggplot(data = l_distribution, aes(x = penalties, y = mean)) +
      geom_point()
    
    bayes_tree <- reg_tree(formula = formula_new,
                           data = train,
                           penalty = best_penalty)
    bayes_tree$time <- bayes_time
    
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
  return(list(tree = bayes_tree, l_plot = l_plot))
}

#prediction
get_prediction <- function(tree, X_test) {
  this_lag <- X_test$tmin1
  
  #get the tree
  temp_tree_pred <- tree$tree$pred
  temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
  
  #get appropriate row from tree_info
  tf <- c()
  for(j in 1:nrow(temp_tree_pred)) {
    f <- eval(parse(text = temp_tree_pred$criteria[j]), envir = X_test)
    tf <- c(tf, f)
  }
  
  if(length(tf) == 1) {
    tf <- c(TRUE)
  }
  
  #get constant and beta_hat and predict
  this_prediction <- temp_tree_pred[tf, "predictions"]
  return(this_prediction)
}

#Predict using tree --------------------------------------
dates <- seq(411,511)
forecasts_y <- c()
forecasts_z <- c()
tic("expanding horizon y")
for (datex in dates) {
  #initialize training data according to expanding horizon
  train <- yt[1:datex]
  y_mbd <- as.data.frame(embed(train, 12))
  names(y_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  
  #set training and test sets
  X_test <- y_mbd[nrow(y_mbd), ]
  X_test$trend <- nrow(y_mbd)
  y_mbd <- y_mbd[-nrow(y_mbd),]
  
  #fit the tree
  tic("Make tree")
  tree <- bayesian_sprout_tree(formula = call, 
                               feature_frac = feature_frac, 
                               sample_data = sample_data, 
                               minsize = minsize, 
                               data = y_mbd, 
                               penalties = penalties)
  toc()
  
  #get the prediction
  prediction <- get_prediction(tree, X_test)
  
  forecasts_y <- c(forecasts_y, prediction)
}
toc()


tic("expanding horizon z")
for (datex in dates) {
  #initialize training data according to expanding horizon
  train <- zt[1:datex]
  z_mbd <-as.data.frame(embed(train, 12))
  names(z_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  
  #set training and test sets
  X_test <- z_mbd[nrow(z_mbd), ]
  X_test$trend <- nrow(z_mbd)
  z_mbd <- z_mbd[-nrow(z_mbd),]
  
  #fit the tree
  tic("Make tree")
  tree <- bayesian_sprout_tree(formula = call, 
                               feature_frac = feature_frac, 
                               sample_data = sample_data, 
                               minsize = minsize, 
                               data = z_mbd, 
                               penalties = penalties)
  toc()
  
  #get the prediction
  prediction <- get_prediction(tree, X_test)
  
  forecasts_z <- c(forecasts_z, prediction)
}
toc()

#Fit tree -------------------------------------
yt <- read_rds(paste0(simulate_ar1, "ar1-data/simulated-data.rds"))
y_mbd <- as.data.frame(embed(yt, 12))
names(y_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
zt <- read_rds(paste0(simulate_ar1, "evolving-data/simulated-evolving.rds"))
z_mbd <- as.data.frame(embed(zt, 12))
names(z_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")

ytree <- bayesian_sprout_tree(formula = call, 
                              feature_frac = feature_frac, 
                              sample_data = sample_data, 
                              minsize = minsize, 
                              data = y_mbd, 
                              penalties = penalties)
ytree_fit <- ytree$tree$fit

ztree <- bayesian_sprout_tree(formula = call, 
                              feature_frac = feature_frac, 
                              sample_data = sample_data, 
                              minsize = minsize, 
                              data = z_mbd, 
                              penalties = penalties)
ztree_fit <- ztree$tree$fit

#Export ---------------------------------------
write_rds(ytree_fit, paste0(simulate_ar1, "ar1-data/mean-fit-ar.rds"))
write_rds(ztree_fit, paste0(simulate_ar1, "evolving-data/mean-fit-evolving.rds"))
write_rds(forecasts_y, paste0(simulate_ar1, "ar1-data/mean-fit-forecast.rds"))
write_rds(forecasts_z, paste0(simulate_ar1, "evolving-data/mean-fit-forecast.rds"))