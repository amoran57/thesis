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
  
  split_at <- this_df[which.min(sse) + 1,]$x
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
      
      
      if (!is.na(tree_info[j, "FILTER"])) {
        # append the splitting rules
        tmp_filter  <- paste(tree_info[j, "FILTER"], 
                             tmp_filter, sep = " & ")
      }
      
      #to catch repeated observations when this is run in the context of sprout_tree
      if(this_sse == 0) {
        split_here <- rep(FALSE, 2)
      } else {
        # get the number of observations in current node
        tmp_nobs <- sapply(tmp_filter,
                           FUN = function(i, x) {
                             nrow(subset(x = x, subset = eval(parse(text = i))))
                           },
                           x = this_data)  
      }
      
      #check for valid split based on minsize or penalty
      if (all(split_here)) {
        if (any(tmp_nobs < 2)) {
          split_here <- rep(FALSE, 2)
        } else {
          # evaluate quality of split based on minsize or penalty
          if (!is.null(minsize)) {
            # insufficient minsize for split
            if (any(tmp_nobs <= minsize)) {
              split_here <- rep(FALSE, 2)
            }
          } else if (!is.null(penalty)) {
            
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
    if(!is.na(criterion)) {
      ind <- as.numeric(rownames(subset(data, eval(parse(text = criterion)))))
    } else {
      ind <- as.numeric(rownames(data))
    }
    # estimator is the mean y value of the leaf
    fitted[ind] <- mean(y[ind])
    pred <- mean(y[ind])
    criteria <- c(criteria, criterion)
    predictions <- c(predictions, pred)
  }
  
  pred <- data.frame(criteria, predictions)
  
  # return everything
  return(list(tree = tree_info, fit = fitted, formula = formula, data = data, pred = pred, penalty = penalty))
}
new_reg_tree <- function(formula, data, minsize = NULL, penalty = NULL) {
  
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
        keep_going <- ifelse(this_sse == 0, FALSE, TRUE)
        
        # estimate splitting criteria
        splitting <- apply(X,  MARGIN = 2, FUN = sse_var, y = this_y)
        
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
        #update kill condition
        keep_going <- ifelse(all(split_here), TRUE, FALSE)
        
        if (!is.na(tree_info[j, "FILTER"])) {
          # append the splitting rules
          tmp_filter  <- paste(tree_info[j, "FILTER"], 
                               tmp_filter, sep = " & ")
        }
        
        # get the number of observations in current node
        tmp_nobs <- sapply(tmp_filter,
                           FUN = function(i, x) {
                             subset(x = x, subset = eval(parse(text = i)))
                           },
                           x = this_data)
        
        first_node <- do.call(cbind, tmp_nobs[,1])
        second_node <- do.call(cbind, tmp_nobs[,2])
        tmp_nobs <- c(nrow(first_node), nrow(second_node))
        
        
        #check for valid split based on minsize or penalty
        if (any(tmp_nobs < 2)) {
          split_here <- rep(FALSE, 2)
          #update kill condition
          keep_going <- FALSE
          #check for insufficient node sizes based on minsize
        } else if (!is.null(minsize) & any(tmp_nobs <= minsize)) {
          # insufficient minsize for split
          split_here <- rep(FALSE, 2)
          #update kill condition
          keep_going <- FALSE
          
        } else if (!is.null(penalty)) {
          
          #calculate improvement in SSE
          first_branch <- first_node[, as.character(formula)[2]]
          first_branch_mean <- mean(first_branch)
          second_branch <- second_node[, as.character(formula)[2]]
          second_branch_mean <- mean(second_branch)
          
          first_sse <- sum((first_branch - first_branch_mean)^2)
          second_sse <- sum((second_branch - second_branch_mean)^2)
          new_sse <- sum(first_sse, second_sse)
          
          improvement <- new_sse/this_sse
          
          #if the change in SSE is less than demanded, don't split
          if(improvement > penalty) {
            split_here <- rep(FALSE, 2)
            #update kill condition
            keep_going <- FALSE
          }
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


tic("slow")
slow <- plyr::raply(
  20,
  reg_tree(call, infl_mbd, penalty = 0.9),
  .progress = "text"
)
toc()

tic("fast")
slow <- plyr::raply(
  20,
  new_reg_tree(call, infl_mbd, penalty = 0.9),
  .progress = "text"
)
toc()
