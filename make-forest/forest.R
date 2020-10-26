# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

tsData <- ts(values_df$infl, start = c(1959, 1), frequency = 12)

infl_mbd <- embed(values_df$infl, 12)
unemp_mbd <- as.data.frame(embed(values_df$unemp, 6))[-c(1:6),]
infl_mbd <- as.data.frame(infl_mbd)
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
names(unemp_mbd) <- c("un", "unmin1", "unmin2", "unmin3", "unmin4", "unmin5")
infl_mbd <- cbind(infl_mbd, unemp_mbd)
rownames(infl_mbd) <- seq(1:nrow(infl_mbd))
# Random Forest --------------------------------------------
# Credit: https://www.statworx.com/blog/coding-regression-trees-in-150-lines-of-code
#Credit: https://www.r-bloggers.com/2019/06/coding-random-forests-in-100-lines-of-code/
sse_var <- function(x, y) {
  splits <- sort(unique(x))
  sse <- c()
  for (i in seq_along(splits)) {
    sp <- splits[i]
    sse[i] <- sum((y[x < sp] - mean(y[x < sp]))^2) + sum((y[x >= sp] - mean(y[x >= sp]))^2) 
  }
  split_at <- splits[which.min(sse)]
  return(c(sse = min(sse), split = split_at))
}
reg_tree <- function(formula, data, minsize) {
  
  # coerce to data.frame
  data <- as.data.frame(data)
  
  # handle formula
  formula <- terms.formula(formula)
  
  # get the design matrix
  X <- model.matrix(formula, data)
  
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
      
      # handle root node
      if (!is.na(tree_info[j, "FILTER"])) {
        # subset data according to the filter
        this_data <- subset(data, eval(parse(text = tree_info[j, "FILTER"])))
        # get the design matrix
        X <- model.matrix(formula, this_data)
      } else {
        this_data <- data
      }
      
      # estimate splitting criteria
      splitting <- apply(X,  MARGIN = 2, FUN = sse_var, y = this_data[, all.vars(formula)[1]])
      
      # get the min SSE
      tmp_splitter <- which.min(splitting[1,])
      
      # define maxnode
      mn <- max(tree_info$NODE)
      
      # paste filter rules
      tmp_filter <- c(paste(names(tmp_splitter), ">=", 
                            splitting[2,tmp_splitter]),
                      paste(names(tmp_splitter), "<", 
                            splitting[2,tmp_splitter]))
      
      # Error handling! check if the splitting rule has already been invoked
      split_here  <- !sapply(tmp_filter,
                             FUN = function(x,y) any(grepl(x, x = y)),
                             y = tree_info$FILTER)
      
      # append the splitting rules
      if (!is.na(tree_info[j, "FILTER"])) {
        tmp_filter  <- paste(tree_info[j, "FILTER"], 
                             tmp_filter, sep = " & ")
      } 
      
      # get the number of observations in current node
      tmp_nobs <- sapply(tmp_filter,
                         FUN = function(i, x) {
                           nrow(subset(x = x, subset = eval(parse(text = i))))
                         },
                         x = this_data)  
      
      # insufficient minsize for split
      if (any(tmp_nobs <= minsize)) {
        split_here <- rep(FALSE, 2)
      }
      
      # create children data frame
      children <- data.frame(NODE = c(mn+1, mn+2),
                             NOBS = tmp_nobs,
                             FILTER = tmp_filter,
                             TERMINAL = rep("SPLIT", 2),
                             row.names = NULL)[split_here,]
      
      # overwrite state of current node
      tree_info[j, "TERMINAL"] <- ifelse(all(!split_here), "LEAF", "PARENT")
      
      # bind everything
      tree_info <- rbind(tree_info, children)
      
      # check if there are any open splits left
      do_splits <- !all(tree_info$TERMINAL != "SPLIT")
    } # end for
  } # end while
  
  # calculate fitted values
  leafs <- tree_info[tree_info$TERMINAL == "LEAF", ]
  fitted <- c()
  for (i in seq_len(nrow(leafs))) {
    # extract index
    ind <- as.numeric(rownames(subset(data, eval(parse(text = leafs[i, "FILTER"])))))
    # estimator is the mean y value of the leaf
    fitted[ind] <- mean(y[ind])
  }
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data))
}
# define function to sprout a single tree
sprout_tree <- function(formula, feature_frac, sample_data = TRUE, minsize = NULL, data) {
  # extract features
  features <- all.vars(formula)[-1]
  # extract target
  target <- all.vars(formula)[1]
  # extract target data
  y <- data[, as.character(formula)[2]]
  # bag the data
  # - randomly sample the data with replacement (duplicate are possible)
  if (sample_data == TRUE) {
    train <- data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
  } else {
    train <- data
  }
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
  if (is.null(minsize)) {
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = ceiling(nrow(train) * 0.1))
  } else {
    tree <- reg_tree(formula = formula_new,
                     data = train,
                     minsize = minsize)
  }

  # calculate fitted values
  leafs <- tree$tree[tree$tree$TERMINAL == "LEAF", ]
  fitted <- c()
  criteria <- c()
  predictions <- c()
  for (i in seq_len(nrow(leafs))) {
    criterion <- leafs[i, "FILTER"]
    # extract index
    ind <- as.numeric(rownames(subset(data, eval(parse(text = leafs[i, "FILTER"])))))
    # estimator is the mean y value of the leaf
    fitted[ind] <- mean(y[ind])
    pred <- mean(y[ind])
    criteria <- c(criteria, criterion)
    predictions <- c(predictions, pred)
  }
  
  pred <- data.frame(criteria, predictions)
  tree$pred <- pred
  tree$fit <- fitted

  # return the tree
  return(tree)
}
reg_rf <- function(formula, n_trees = 50, feature_frac = 0.7, sample_data = TRUE, minsize = NULL, data) {
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  trees <- plyr::raply(
    n_trees,
    sprout_tree(
      formula = formula,
      feature_frac = feature_frac,
      sample_data = sample_data,
      minsize = minsize,
      data = data
    ),
    .progress = "text"
  )

  # extract fit
  fits <- do.call("cbind", trees[, 2])
  fits <- as.data.frame(fits)
  # calculate the final fit as a mean of all regression trees
  means <- rowMeans(fits, na.rm = TRUE)
  fits <- cbind(fits, means)
  return(list(trees = trees, fit = fits))
  # return(forest_df)
}
get_forecast <- function(forest, data) {
  predictions <- forest$trees[,5]
  predictions_df <- do.call(rbind, predictions)
  tf <- c()
  for(i in 1:nrow(predictions_df)) {
    f <- nrow(subset(data, eval(parse(text = predictions_df$criteria[i])))) > 0
    tf <- c(tf, f)
  }
  predictions_df$tf <- tf
  predictions_df <- predictions_df %>% 
    dplyr::filter(tf)
  
  pred <- mean(predictions_df$predictions)
}
forecast_rf <- function(formula, data, horizon, n_trees = 50, feature_frac = 0.7, sample_data = TRUE, minsize = NULL) {
  #get y and X trainers
  y_train <- data[, 1]
  X_train <- data[, -1]
  X_test <- data[nrow(data), c(1:(length(data)-1))]
  names(X_test) <- names(data[-1])
  #update based on horizon
  y_train <- y_train[-c(1:(horizon - 1))] 
  X_train <- X_train[-c((nrow(X_train) - (horizon - 2)):nrow(X_train)), ] 
  
  adj_data <- data.frame(y_train, X_train)
  names(adj_data) <- names(data)
  
  forest <- reg_rf(formula = formula,
                   n_trees = n_trees,
                   feature_frac = feature_frac,
                   sample_data = sample_data,
                   minsize = minsize,
                   data = adj_data)
  
  pred <- get_forecast(forest, X_test)
  return(pred)
}
ts_forest <- function(y, x = NULL, y_lag_order = 12, x_lag_order = NULL, horizon = 1) {
  
  #get y data frame
  y_df <- as.data.frame(embed(y, y_lag_order))
  y_names <- as.character(seq(1:(length(y_df) - 1)))
  y_names <- paste0("tmin", y_names)
  y_names <- c("t", y_names)
  names(y_df) <- y_names
  
  #get x data frame
  X <- data.frame()
  num_x <- 0
  for(this_x in x) {
    x_df <- as.data.frame(embed(this_x, x_lag_order))
    x_names <- as.character(seq(1:(length(x_df))))
    x_prefix <- paste0("x", as.character(num_x), "min")
    x_names <- paste0(x_prefix, x_names)
    names(x_df) <- x_names
    X <- cbind(X, x_df)
    num_x <- num_x + 1
  }
  
  #get formula
  call <- as.character(c(2:length(ts_df)))
  call <- glue::glue_collapse(x = call, " + ")
  call <- paste0("1 ~ ", call)
  call <- as.formula(call)
  
  #get forecast data
  X_test <- ts_df[nrow(ts_df), c(1:(length(ts_df)-1))]
  names(X_test) <- names(ts_df[-1])
  #get forest
  forest <- reg_rf(formula = call, data = ts_df)
  
  #get prediction
  pred <- get_forecast(forest, X_test)
}

#Use sprout trees ------------------------------------
#get formula call
call <- as.character(seq(1:11))
for(i in 1:length(call)) {
  call[i] <- paste0("tmin", as.character(i))
}

call <- c(call, names(unemp_mbd))
ind <- glue::glue_collapse(x = call, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

tree <- sprout_tree(formula = call, feature_frac = 1, data = infl_mbd)
real_tree <- reg_tree(call, infl_mbd, 10)

#predict 1 month out ----------------------------
pred_month <- c()
monthly_dates <- seq(as.Date("1999/12/1"), as.Date("2017/12/1"), "month")
for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  infl_mbd <- embed(train_tsData, 12)
  infl_mbd <- as.data.frame(infl_mbd)
  names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  
  temp_pred <- forecast_rf(call, infl_mbd, 1)
  pred_month <- c(pred_12, temp_pred)
}
pred_month <- ts(pred_12, start = c(2000, 1), frequency = 12)
real_ts <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
accuracy (pred_12, real_ts)

pred_arima <- c()
for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  pred_a <- forecast(auto.arima(train_tsData), 1)$mean
  pred_arima <- c(pred_arima, pred_a)
}
pred_arima <- ts(pred_arima, start = c(2000, 1), frequency = 12)
accuracy(pred_arima, real_ts)

month_df <- as.data.frame(pred_month) %>% 
  dplyr::select(forest = x) %>% 
  dplyr::mutate(date = seq(as.Date("2000/1/1"), as.Date("2018/1/1"), "month"))

arima_df <- as.data.frame(pred_arima) %>% 
  dplyr::select(arima = x) %>% 
  dplyr::mutate(date = seq(as.Date("2000/1/1"), as.Date("2018/1/1"), "month"))

infl_df <- as.data.frame(tsData) %>% 
  dplyr::select(infl = x) %>% 
  mutate(date = seq(as.Date("1959/1/1"), as.Date("2020/8/1"), "month"))

compare <- infl_df %>% left_join(month_df) %>% left_join(arima_df) %>% dplyr::select(date, everything())
tidy_compare <- gather(data = compare, key = "key", value = "value", "infl":"arima")

compare_plot <- ggplot(data = tidy_compare, aes(x = date, y = value, color = key)) +
  geom_line()


pdf("plot.pdf")
compare_plot
dev.off()

#Use forest ---------------------------------------------
#create forest and trees
forest <- reg_rf(formula = call, minsize = 10, data = infl_mbd)

#get and graph fit
forest_fit <- forest$fit$means
tree_fit <- tree$new_fit
real_fit <- real_tree$fit
real <- infl_mbd$t
graph <- data.frame(real, forest_fit, real_fit, tree_fit) 
graph <- graph %>% 
  dplyr::mutate(obs = seq(1:nrow(graph)))
tidy_graph <- gather(graph, key = "key", value = "value", "real":"tree_fit")

plot <- ggplot(data = tidy_graph, aes(x = obs, y = value, color = key)) +
  geom_line()

plot