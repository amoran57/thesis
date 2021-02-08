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

penalties <- seq(0.7, 0.99, by = 0.005)
penalty <- 0.9
libs <- c("dplyr", "tictoc", "ggplot2")
formula <- call
feature_frac <- 0.7
sample_data <- FALSE
minsize <- NULL
data <- infl_mbd
n_trees <- 50
lag_name <- "tmin1"

#Functions ----------------------------------------------
#foundational
new_obj_function <- function(split_var, y, lag) {
  this_df <- data.frame(split_var, y, lag) %>% 
    dplyr::arrange(split_var)
  
  y <- this_df$y
  x <- this_df$lag
  data_length <- length(y)
  ssr <- c()
  
  #calculate at the first split ----------
  #below
  yi_1 <- c()
  xi_1 <- c()
  meanx_1 <- 0
  meany_1 <- 0
  res_y_1 <- yi_1 - meany_1
  res_x_1 <- xi_1 - meanx_1
  sxx_1 <- sum(res_x_1^2)
  sxy_1 <- sum(res_x_1*res_y_1)
  beta_1 <- sxy_1/sxx_1
  alpha_1 <- meany_1 - beta_1*meanx_1
  yhati_1 <- alpha_1 + beta_1*xi_1
  res_1 <- yi_1 - yhati_1
  sq_res_1 <- res_1^2
  ssr_1 <- sum(sq_res_1)
  
  #above
  yi_2 <- y
  xi_2 <- lag
  meanx_2 <- mean(xi_2)
  meany_2 <- mean(yi_2)
  res_y_2 <- yi_2 - meany_2
  res_x_2 <- xi_2 - meanx_2
  sxx_2 <- sum(res_x_2^2)
  sxy_2 <- sum(res_x_2*res_y_2)
  beta_2 <- sxy_2/sxx_2
  alpha_2 <- meany_2 - beta_2*meanx_2
  yhati_2 <- alpha_2 + beta_2*xi_2
  res_2 <- yi_2 - yhati_2
  sq_res_2 <- res_2^2
  ssr_2 <- sum(sq_res_2)
  
  
  for(i in 1:data_length) {
    #Now we update -------------
    #update yi
    y_new <- y[i]
    yi_1 <- c(yi_1, y_new)
    yi_2 <- yi_2[-1]
    #update xi
    x_new <- x[i]
    xi_1 <- c(xi_1, x_new)
    xi_2 <- xi_2[-1]
    #update meanx
    meanx_1 <- (meanx_1*(i - 1) + x_new)/i
    meanx_2 <- (meanx_2*(data_length - i + 1) - x_new)/(data_length - i)
    #update meany
    meany_1 <- (meany_1*(i - 1) + y_new)/i
    meany_2 <- (meany_2*(data_length - i + 1) - y_new)/(data_length - i)
    #update sxx
    res_x_1 <- xi_1 - meanx_1
    res_x_2 <- xi_2 - meanx_2
    sxx_1 <- sum(res_x_1^2)
    sxx_2 <- sum(res_x_2^2)
    #update sxy
    res_y_1 <- yi_1 - meany_1
    res_y_2 <- yi_2 - meany_2
    sxy_1 <- sum(res_x_1*res_y_1)
    sxy_2 <- sum(res_x_2*res_y_2)
    #update beta
    beta_1 <- sxy_1/sxx_1
    beta_2 <- sxy_2/sxx_2
    #update alpha
    alpha_1 <- meany_1 - beta_1*meanx_1
    alpha_2 <- meany_2 - beta_2*meanx_2
    #update yhat
    yhati_1 <- alpha_1 + beta_1*xi_1
    yhati_2 <- alpha_2 + beta_2*xi_2
    #update residuals
    res_1 <- yi_1 - yhati_1
    res_2 <- yi_2 - yhati_2
    #update ssr
    sq_res_1 <- res_1^2
    sq_res_2 <- res_2^2
    ssr_1 <- sum(sq_res_1)
    ssr_2 <- sum(sq_res_2)
    ssr[i] <- ssr_1 + ssr_2
  }
  
  
  split_at <- this_df[which.min(ssr),]$split_var
  return(c(ssr = ssr[which.min(ssr)], split = split_at))
}
ar1_reg_tree <- function(formula, data, minsize = NULL, penalty = NULL, lag_name) {
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)[,-1]
  
  # extract target
  y_name <- as.character(formula)[2]
  y <- data[, y_name]
  
  # get lag
  lag <- data[, lag_name]
  
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
        this_lm <- lm(this_data[,y_name] ~ this_data[,lag_name])
        these_residuals <- this_lm$residuals
        this_ssr <- sum(these_residuals^2)
        
        this_y <- this_data[, y_name]
        this_lag <- this_data[, lag_name]
        #update kill condition
        if(this_ssr == 0) {
          split_here <- rep(FALSE, 2)
          keep_going <- FALSE
        }
        
        # estimate splitting criteria
        splitting <- apply(X,  MARGIN = 2, FUN = new_obj_function, y = this_y, lag = this_lag)
        
        # get the min SSE
        tmp_splitter <- which.min(splitting[1,])
        split_value <- splitting[2,tmp_splitter]
        split_value <- round(split_value, 9)
        new_ssr <- splitting[1,tmp_splitter]
        improvement <- new_ssr/this_ssr
        
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
        
        if(any(tmp_nobs < 6)) {
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
  constants <- c()
  beta_hats <- c()
  for (i in seq_len(nrow(leafs))) {
    criterion <- leafs[i, "FILTER"]
    # extract index
    if(!is.na(criterion)) {
      ind <- as.numeric(rownames(subset(data, eval(parse(text = criterion)))))
    } else {
      ind <- as.numeric(rownames(data))
    }
    
    # estimator is the fitted AR(1) value of the leaf 
    this_leaf_lm <- lm(y[ind] ~ lag[ind])
    this_leaf_fit <- this_leaf_lm$fitted
    
    fitted[ind] <- this_leaf_fit
    constants[i] <- this_leaf_lm$coefficients[1]
    beta_hats[i] <- this_leaf_lm$coefficients[2]
    criteria[i] <- criterion
  }
  
  pred <- data.frame(criteria, constants, beta_hats)
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data, pred = pred, penalty = penalty))
}

#bayesian
get_rmses <- function(these_penalties, formula, train_df, test_df, target, lag_name) {
  rmses <- c()
  for(penalty in these_penalties) {
    #get a tree built on the training data and the current penalty
    temp_tree <- ar1_reg_tree(formula, train_df, minsize = NULL, penalty = penalty, lag_name = lag_name)
    temp_tree_pred <- temp_tree$pred
    temp_tree_pred$criteria <- as.character(temp_tree_pred$criteria)
    
    #predict each value in test_df
    tree_predictions <- c()
    
    while(length(tree_predictions) < nrow(test_df) | any(is.na(tree_predictions))) {
      
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
          
          #get the lag and manually compute values
          this_lag <- test_df[ind, lag_name]
          this_constant <- temp_tree_pred[i, "constants"]
          this_beta_hat <- temp_tree_pred[i, "beta_hats"]
          these_predictions <- this_constant + this_beta_hat*this_lag
          
          #update tree predictions
          tree_predictions[ind] <- these_predictions
        }
      } else {
        this_lag <- test_df[, lag_name]
        this_constant <- temp_tree_pred[1, "constants"]
        this_beta_hat <- temp_tree_pred[1, "beta_hats"]
        tree_predictions <- this_constant + this_beta_hat*this_lag
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
bayesian_sprout_ar1_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
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
    sample_options <- seq(50, 100)
    sample_size <- sample(sample_options, 1)
    train <- data[(nrow(data)-sample_size):nrow(data),]
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
    train_df <- train[1:(nrow(train)-12), all.vars(formula_new)]
    test_df <- train[(nrow(train)-11):nrow(train), all.vars(formula_new)]
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
    
    #get scores from random penalties above
    rmses <- get_rmses(rand_penalties, formula_new, train_df, test_df, target, lag_name = first_lag)
    
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
          next_rmse <- get_rmses(next_penalty, formula_new, train_df, test_df, target, lag_name = first_lag)
        }
        new_rmses <- c(new_rmses, next_rmse)
        #step 6
        history <- rbind(history, c(next_penalty, next_rmse))
      }
    }
    
    #grab the "best" penalty
    best_penalties <- evaluate_penalties(penalties, l_distribution, g_distribution)
    best_penalty <- history$penalties[which.min(history$score)]
    
    bayes_done <- toc()
    bayes_time <- bayes_done$toc - bayes_done$tic
    
    l_plot <- ggplot(data = l_distribution, aes(x = penalties, y = mean)) +
      geom_point()
    
    bayes_tree <- ar1_reg_tree(formula = formula_new,
                               data = train,
                               penalty = best_penalty,
                               lag_name = first_lag)
    bayes_tree$time <- bayes_time
    
  } else if (is.null(minsize)) {
    tree <- ar1_reg_tree(formula = formula_new,
                         data = train,
                         minsize = ceiling(nrow(train) * 0.1),
                         lag_name = first_lag)
  } else {
    tree <- ar1_reg_tree(formula = formula_new,
                         data = train,
                         minsize = minsize,
                         lag_name = first_lag)
  }
  
  # return the tree
  return(list(tree = bayes_tree, l_plot = l_plot))
}

#forest
bayes_reg_parallel_rf <- function(formula, n_trees = 50, feature_frac = 0.7, sample_data = TRUE, minsize = NULL, data, penalties = NULL) {
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  formula <- formula
  n_trees <- n_trees
  feature_frac <- feature_frac
  sample_data <- sample_data
  minsize <- minsize
  data <- data
  penalties <- penalties
  
  split <- detectCores()/1.2
  print(paste0("Cores to use: ", as.character(split)))
  tic("Parallel")
  if(n_trees <= split) {
    print("Will only run once")
    #reduce split and run only once
    split <- n_trees
    cl <- makeCluster(split)
    registerDoParallel(cl)
    x <- c("dplyr", "tictoc", "ggplot2")
    clusterExport(cl, c("libs", "formula", "n_trees", "feature_frac", "sample_data", 
                        "minsize", "data", "penalties", "bayesian_sprout_ar1_tree", "evaluate_penalties", 
                        "generate_custom_random", "get_distribution", "new_obj_function",
                        "split_lg", "get_rmses", "ar1_reg_tree"))
    init <- clusterEvalQ(cl, lapply(libs, require, character.only = TRUE))
    
    trees <- foreach(
      rep(1, split),
      .combine = list,
      .multicombine = TRUE) %dopar%
      bayesian_sprout_ar1_tree(
        formula = formula,
        feature_frac = feature_frac,
        sample_data = sample_data,
        minsize = minsize,
        data = data,
        penalties = penalties
      )
    
    
    stopCluster(cl)
  } else {
    iterate <- ceiling(n_trees/split)
    print(paste0("Will run ", as.character(iterate), " times"))
    trees <- list()
    cl <- makeCluster(split)
    registerDoParallel(cl)
    x <- c("dplyr", "tictoc", "ggplot2")
    clusterExport(cl, c("libs", "formula", "n_trees", "feature_frac", "sample_data", 
                        "minsize", "data", "penalties", "bayesian_sprout_ar1_tree", "evaluate_penalties",
                        "generate_custom_random", "get_distribution", "new_obj_function",
                        "split_lg", "get_rmses", "ar1_reg_tree"))
    init <- clusterEvalQ(cl, lapply(libs, require, character.only = TRUE))
    
    for(i in 1:iterate) {
      tic(paste0("batch ", as.character(i), " of ", as.character(iterate), " complete"))
      these_trees <- foreach(
        rep(1, split),
        .combine = list,
        .multicombine = TRUE) %dopar%
        bayesian_sprout_ar1_tree(
          formula = formula,
          feature_frac = feature_frac,
          sample_data = sample_data,
          minsize = minsize,
          data = data,
          penalties = penalties
        )
      
      trees <- c(trees, these_trees)
      toc()
    }
    
    stopCluster(cl)
    
  }
  toc()
  return(trees)
}

results <- lapply(bayes, function(x) answer <- nrow(x$tree$pred))

#prediction
get_prediction <- function(forest, X_test) {
  num_trees <- length(forest)
  this_lag <- X_test$tmin1
  all_predictions <- c()
  
  for (i in 1:num_trees) {
    #get each tree from the forest
    temp_tree <- forest[[i]]
    temp_tree_pred <- temp_tree$tree$pred
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
    this_constant <- temp_pred$constants
    this_beta_hat <- temp_pred$beta_hats
    all_predictions[i] <- this_constant + this_beta_hat*this_lag
  }
  #get the forest prediction and return it
  forest_prediction <- mean(all_predictions)
  return(forest_prediction)
}
get_prediction_08 <- function(forest, X_test) {
  num_trees <- length(forest)
  this_lag <- X_test$tmin1
  all_predictions <- c()
  
  for (i in 1:num_trees) {
    #get each tree from the forest
    temp_tree <- forest[[i]]
    temp_tree_pred <- temp_tree$tree$pred
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
    this_constant <- temp_pred$constants
    this_beta_hat <- temp_pred$beta_hats
    all_predictions[i] <- this_constant + this_beta_hat*this_lag*0.8
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
variable_mentions <- list()
all_real_trees <- c()

tic("expanding horizon forest")
for (k in 1:253) {
  monthx <- monthly_dates[k]
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    dplyr::filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  infl_mbd <- embed(train_tsData, lag_order)
  infl_mbd <- as.data.frame(infl_mbd)
  names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  
  #set training and test sets
  X_test <- infl_mbd[nrow(infl_mbd), ]
  X_test$trend <- nrow(infl_mbd)
  infl_mbd <- infl_mbd[-nrow(infl_mbd),]
  
  #fit the forest
  timestamp()
  tic(paste0("Bayesian forest iteration ", as.character(k), " complete"))
  bayes <- bayes_reg_parallel_rf(formula, sample_data = sample_data, data = infl_mbd, penalties = penalties)
  toc()
  
  #get the prediction
  predict_rf <- get_prediction(forest = bayes, X_test = X_test)
  
  forecasts_rf <- c(forecasts_rf, predict_rf)
  
  n_rows <- lapply(bayes, function(x) answer <- nrow(x$tree$pred))
  real_trees <- bayes[which(n_rows > 1)]
  n_real_trees <- length(real_trees)
  if(n_real_trees > 0) {
    real_preds_list <- lapply(real_trees, function(x) answer <- x$tree$pred)
    real_preds <- do.call(rbind, real_preds_list)
    real_preds$criteria <- as.character(real_preds$criteria)
    real_criteria <- as.list(real_preds$criteria)
    real_preds_string <- do.call(paste, real_criteria)
    mentions <- c()
    for(variable in variables) {
      mentions[variable] <- str_count(real_preds_string, pattern = variable)
    }
    these_mentions <- data.frame(variables, mentions)
    
  } else {
   these_mentions <- data.frame(variables, mentions = rep(0, length(variables)))
  }
  
  variable_mentions[[k]] <- these_mentions
  all_real_trees <- c(all_real_trees, n_real_trees)
  
}
toc()

all_mentions <- lapply(variable_mentions, function(x) answer <- x$mentions)
all_mentions_df <- data.frame(do.call(cbind, all_mentions))
rownames(all_mentions_df) <- variables
colnames(all_mentions_df) <- monthly_dates
t_df <- as.data.frame(t(all_mentions_df))
t_df$real_trees <- all_real_trees
all_mentions_df <- as.data.frame(t(t_df))
all_mentions_df$total <- rowSums(all_mentions_df)

forest_forecast_ts <- ts(forecasts_rf, start = c(1999, 1), frequency = 12)

pred_arima <- read_rds(paste0(export, "4_year_forecasts/arima_forecast.rds"))

accuracy(tsData, forest_forecast_ts)
accuracy(tsData, pred_arima)

write_rds(forest_forecast_ts, paste0(export,"4_year_forecasts/ar1_obj_forecast_straight.rds"))
write_rds(all_mentions_df, paste0(export, "custom_forest_analysis/non_sample_mentions.rds"))
