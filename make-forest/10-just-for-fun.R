# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
mentions <- read_rds(paste0(export, "custom_forest_analysis/mentions.rds"))
totals <- data.frame(name = rownames(mentions), total = mentions$total)
totals$per_forest <- totals$total/253
totals$per_tree <- totals$per_forest/50
totals$per_real_tree <- totals$per_tree/totals[13,4]
barplot(H = totals$total, xlab = totals$name, height = totals$total)

mean_mentions <- read_rds(paste0(export, "custom_forest_analysis/mean_forest_mentions.rds"))
mean_totals <- data.frame(name = rownames(mean_mentions), total = mean_mentions$total)
mean_totals$per_forest <- mean_totals$total/253
mean_totals$per_tree <- mean_totals$per_forest/50
mean_totals$per_real_tree <- mean_totals$per_tree/mean_totals[13,4]
barplot(H = mean_totals$toal, xlab = mean_totals$name, height = mean_totals$total)

diff_df <- data.frame(name = rownames(mentions), mean_tree = mean_totals$per_real_tree, tree = totals$per_real_tree)
diff_df$diff <- diff_df$tree/diff_df$mean_tree
