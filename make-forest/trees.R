# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")

#Code ------------------------------------------
# Credit: https://www.statworx.com/blog/coding-regression-trees-in-150-lines-of-code

df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

infl_mbd <- embed(values_df$infl, 12)
infl_mbd <- as.data.frame(infl_mbd)
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")

# Regression Tree ---------------------------------------------------------
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


#' reg_tree
#' Fits a simple regression tree with SSE splitting criterion. The estimator function
#' is the mean.
#' 
#' @param formula an object of class formula
#' @param data a data.frame or matrix
#' @param minsize a numeric value indicating the minimum size of observations
#'                in a leaf
#'
#' @return \itemize{
#' \item tree - the tree object containing all splitting rules and observations
#' \item fit - our fitted values, i.e. X %*% theta
#' \item formula - the underlying formula
#' \item data - the underlying data
#' }
#' @export
#'
#' @examples # Complete runthrough see: www.github.com/andrebleier/cheapml
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


# Get tree ---------------------------------------------
one_ten <- as.character(seq(1:10))
for(i in 1:10) {
  one_ten[i] <- paste0("tmin", as.character(i))
}
ind <- glue::glue_collapse(x = one_ten, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)
tree <- reg_tree(formula = call, data = infl_mbd, 10)
node <- tree$tree$NODE
nobs <- tree$tree$NOBS
filter <- tree$tree$FILTER
terminal <- tree$tree$TERMINAL
tree_df <- data.frame(node, nobs, filter, terminal)
real_tree <- tree_df %>% 
  filter(terminal == "LEAF")
pred <- tree$fit
obs <- seq(1:729)
infl_mbd <- cbind(infl_mbd, pred, obs)
trim_infl <- infl_mbd %>% 
  select(t, pred, obs)
tidy_infl <- gather(trim_infl, key = "key", value = "value", "t", "pred")

plot <- ggplot(data = tidy_infl, aes(x = obs, y = value, color = key)) +
  geom_line()

plot
