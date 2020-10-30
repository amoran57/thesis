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
reg_tree <- function(formula, data, minsize, penalty) {
  
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
      
      #calculate current SSE
      this_y <- this_data[, as.character(formula)[2]]
      this_sse <- sum((this_y[1:length(this_y)] - mean(this_y))^2)
      
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
      
      if (all(split_here)) {
        if (any(tmp_nobs == 0)) {
            split_here <- rep(FALSE, 2)
        } else {
      # evaluate quality of split based on minsize or penalty
            if (!is.null(minsize)) {
            # insufficient minsize for split
              if (any(tmp_nobs <= minsize)) {
                split_here <- rep(FALSE, 2)
              }
            } else {
        
              #calculate improvement in SSE
              first_branch <- subset(this_data, eval(parse(text = tmp_filter[1])))[, as.character(formula)[2]]
              second_branch <- subset(this_data, eval(parse(text = tmp_filter[2])))[, as.character(formula)[2]]
        
              first_sse <- sum((first_branch[1:length(first_branch)] - mean(first_branch))^2)
              second_sse <- sum((second_branch[1:length(second_branch)] - mean(second_branch))^2)
              new_sse <- sum(first_sse, second_sse)
        
              improvement <- new_sse/this_sse
              #if the change in SSE is less than demanded, don't split
              if(improvement > penalty) {
                split_here <- rep(FALSE, 2)
              }
            }
      }
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
    ind <- as.numeric(rownames(subset(data, eval(parse(text = leafs[i, "FILTER"])))))
    # estimator is the mean y value of the leaf
    fitted[ind] <- mean(y[ind])
    pred <- mean(y[ind])
    criteria <- c(criteria, criterion)
    predictions <- c(predictions, pred)
  }
  
  pred <- data.frame(criteria, predictions)
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data, pred = pred))
}

#use tree -------------------------
#get formula call
call <- as.character(seq(1:(length(infl_mbd)- 1)))
for(i in 1:length(call)) {
  call[i] <- paste0("tmin", as.character(i))
}

ind <- glue::glue_collapse(x = call, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

#predict one month out
pred_month <- c()
monthly_dates <- seq(as.Date("2000/1/1"), as.Date("2018/1/1"), "month")
for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    dplyr::filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)

  
  #convert to data.frame
  infl_mbd <- embed(train_tsData, 12)
  infl_mbd <- as.data.frame(infl_mbd)
  names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  test_df <- infl_mbd[nrow(infl_mbd), -1]
  infl_mbd <- infl_mbd[-nrow(infl_mbd),]
  rownames(infl_mbd) <- seq(1:nrow(infl_mbd))
  rownames(test_df) <- 1
  
  #get forecast
  temp_tree <- reg_tree(call, infl_mbd, minsize = NULL, penalty = 0.9)$pred
  tf <- c()
  for(i in 1:nrow(temp_tree)) {
    f <- eval(parse(text = temp_tree$criteria[i]), envir = test_df)
    tf <- c(tf, f)
  }
  temp_tree$tf <- tf
  temp_pred <- temp_tree %>% 
    dplyr::filter(tf)
  
  #append to vector
  pred_month <- c(pred_month, temp_pred$predictions)
}
#get accuracy data
pred_month <- ts(pred_month, start = c(2000, 1), frequency = 12)
real_ts <- ts(values_df$infl, start = c(1959, 1), frequency = 12)
accuracy (pred_month, real_ts)

pred_min <- c()
for (monthx in monthly_dates) {
  #initialize training data according to expanding horizon
  train_df <- values_df %>% 
    dplyr::filter(date <= monthx)
  train_tsData <- ts(train_df$infl, start = c(1959, 1), frequency = 12)
  
  
  #convert to data.frame
  infl_mbd <- embed(train_tsData, 12)
  infl_mbd <- as.data.frame(infl_mbd)
  names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")
  test_df <- infl_mbd[nrow(infl_mbd), -1]
  infl_mbd <- infl_mbd[-nrow(infl_mbd),]
  rownames(infl_mbd) <- seq(1:nrow(infl_mbd))
  rownames(test_df) <- 1
  
  #get forecast
  temp_tree <- reg_tree(call, infl_mbd, minsize = 10)$pred
  tf <- c()
  for(i in 1:nrow(temp_tree)) {
    f <- eval(parse(text = temp_tree$criteria[i]), envir = test_df)
    tf <- c(tf, f)
  }
  temp_tree$tf <- tf
  temp_pred <- temp_tree %>% 
    dplyr::filter(tf)
  
  #append to vector
  pred_min <- c(pred_min, temp_pred$predictions)
}
pred_min <- ts(pred_min, start = c(2000,1), frequency = 12)
accuracy(pred_min, real_ts)

infl <- values_df %>% 
  dplyr::filter(date >= as.Date("2000/1/1") & date <= as.Date("2018/1/1"))

fits <- data.frame(date = seq(as.Date("2000/1/1"), as.Date("2018/1/1"), "month"),
                              min = pred_min, penalty = pred_month, infl = infl$infl)

tidy_fits <- gather(fits, key = "key", value = "value", "min":"infl")



plot <- ggplot(tidy_fits, aes(x = date, y = value, color = key)) +
  geom_line() +
  scale_color_manual(values = c("black", "red", "darkgreen"))

plot
