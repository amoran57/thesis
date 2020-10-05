# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

infl_mbd <- embed(values_df$infl, 12)
infl_mbd <- as.data.frame(infl_mbd)
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")

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
sprout_tree <- function(formula, feature_frac, data) {
  # extract features
  features <- all.vars(formula)[-1]
  # extract target
  target <- all.vars(formula)[1]
  # bag the data
  # - randomly sample the data with replacement (duplicate are possible)
  train <-
    data[sample(1:nrow(data), size = nrow(data), replace = TRUE),]
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
  tree <- reg_tree(formula = formula_new,
                   data = train,
                   minsize = ceiling(nrow(train) * 0.1))
  # save the fit and the importance
  return(tree)
}
reg_rf <- function(formula, n_trees, feature_frac, data) {
  forest_df <- data.frame(matrix(ncol = 5, nrow = 0))
  names(forest_df) <- c("node", "nobs", "filter", "terminal", "tree_num")
  # apply the rf_tree function n_trees times with plyr::raply
  # - track the progress with a progress bar
  trees <- plyr::raply(
    n_trees,
    sprout_tree(
      formula = formula,
      feature_frac = feature_frac,
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


#Use forest ---------------------------------------------
#get formula call
one_ten <- as.character(seq(1:10))
for(i in 1:10) {
  one_ten[i] <- paste0("tmin", as.character(i))
}
ind <- glue::glue_collapse(x = one_ten, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

#create forest and trees
forest <- reg_rf(formula = call, n_trees = 5, feature_frac = 0.7, data = infl_mbd)
trees <- forest$trees[,1]
forest_df <- do.call(rbind, trees)

#get leaves, find importance of each variable
leaves_df <- forest_df %>% 
  filter(TERMINAL == "LEAF") %>% 
  select(-TERMINAL)
for (i in one_ten) {
  temp <- grepl(i, leaves_df$FILTER, fixed = TRUE)
  leaves_df[i] <- ifelse(temp, leaves_df$NOBS, NA)
}
leaf_filters <- leaves_df$FILTER
leaf_filters[(length(leaf_filters) + 1)] <- NA_character_
leaves_df <- leaves_df %>% 
  select(-FILTER)
sums <- base::colSums(leaves_df, na.rm = TRUE)
leaves_df <- rbind(leaves_df, sums = sums)
leaves_df <- cbind(leaves_df, leaf_filters)
