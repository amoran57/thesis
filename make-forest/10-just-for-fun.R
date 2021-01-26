# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
mentions <- read_rds(paste0(export, "custom_forest_analysis/mentions.rds"))
totals <- data.frame(name = rownames(mentions), total = mentions$total)
barplot(H = totals$total, xlab = totals$name, height = totals$total)
