# Header ---------------------------------------
rm(list=ls()) 
header <- source("header.R")
source("trees.R")

#Code ------------------------------------------
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

infl_mbd <- embed(values_df$infl, 12)
infl_mbd <- as.data.frame(infl_mbd)
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")

#Use tree ------------------------------------------------------
#get formula call
one_ten <- as.character(seq(1:10))
for(i in 1:10) {
  one_ten[i] <- paste0("tmin", as.character(i))
}
ind <- glue::glue_collapse(x = one_ten, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

#get tree as df
tree <- reg_tree(formula = call, data = infl_mbd, 10)
node <- tree$tree$NODE
nobs <- tree$tree$NOBS
filter <- tree$tree$FILTER
terminal <- tree$tree$TERMINAL
tree_df <- data.frame(node, nobs, filter, terminal)

#get terminal nodes
real_tree <- tree_df %>% 
  filter(terminal == "LEAF")

#graph fit vs actual
pred <- tree$fit
obs <- seq(1:729)
infl_mbd <- cbind(infl_mbd, pred, obs)
trim_infl <- infl_mbd %>% 
  select(t, pred, obs)
tidy_infl <- gather(trim_infl, key = "key", value = "value", "t", "pred")

plot <- ggplot(data = tidy_infl, aes(x = obs, y = value, color = key)) +
  geom_line()

plot

#Use forest --------------------------------------------------
source("forest.R")
df <- read_rds(paste0(export, "master_data.rds"))

values_df <- df %>% 
  dplyr::filter(year >= 1959)

infl_mbd <- embed(values_df$infl, 12)
infl_mbd <- as.data.frame(infl_mbd)
names(infl_mbd) <- c("t", "tmin1", "tmin2","tmin3","tmin4","tmin5","tmin6","tmin7","tmin8","tmin9","tmin10","tmin11")

#get formula call
one_ten <- as.character(seq(1:10))
for(i in 1:10) {
  one_ten[i] <- paste0("tmin", as.character(i))
}
ind <- glue::glue_collapse(x = one_ten, " + ")
call <- paste0("t ~ ", ind)
call <- as.formula(call)

#get forest
forest <- reg_rf(formula = call, n_trees = 50, feature_frac = 0.7, data = infl_mbd)
