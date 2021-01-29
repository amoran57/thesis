# Header ---------------------------------------
rm(list=ls())
header <- source("header.R")

#Code ------------------------------------------
#Pull mentions dataframe
mentions <- read_rds(paste0(export, "custom_forest_analysis/sample_mentions.rds"))

#Make relative numbers
t_mentions <- data.frame(t(mentions))
t_mentions <- t_mentions[,-13]
t_mentions$total <- rowSums(t_mentions)
t_mentions[,14:25] <- t_mentions[,1:12]/t_mentions$total
freq_mentions <- data.frame(t(t_mentions[,14:25]))
rownames(freq_mentions) <- rownames(mentions)[-13]
colnames(freq_mentions) <- colnames(mentions)

#Get rolling average
horizon <- 12
avg_mentions <- data.frame(matrix(nrow = nrow(freq_mentions), ncol = ncol(freq_mentions) - horizon))
rownames(avg_mentions) <- rownames(mentions)[-13]
colnames(avg_mentions) <- colnames(mentions)[-c(1:horizon)]
for(i in horizon:(ncol(avg_mentions + horizon))) {
  small_df <- freq_mentions[,c((i-horizon+1):i)]
  avg <- rowMeans(small_df, na.rm = TRUE)
  #get only the top 3 values
  top_indices <- which(avg >= 0.2)
  top_avg <- avg
  top_avg[-top_indices] <- NA_real_
  avg_mentions[,(i-horizon+1)] <- top_avg
}

#Make tidy and plot
avg_df <- data.frame(data.frame(date = colnames(avg_mentions), t(avg_mentions)))
tidy_avg <- gather(avg_df, key = "key", value = "value", "trend":"tmin11")
bad_indices <- which(is.na(tidy_avg$value))
clean_tidy_avg <- tidy_avg[-bad_indices,]
clean_tidy_avg <- clean_tidy_avg[order(clean_tidy_avg$date),]
clean_tidy_avg$date <- as.Date(clean_tidy_avg$date)
rownames(clean_tidy_avg) <- seq(1, nrow(clean_tidy_avg))
avg_plot <- ggplot(clean_tidy_avg, aes(x = date, y = value, color = key)) +
  geom_point()
avg_plot

#Code for non-sample dataframe------------------------------------------
rm(list=ls())
header <- source("header.R")

#Pull mentions dataframe
mentions <- read_rds(paste0(export, "custom_forest_analysis/non_sample_mentions.rds"))

#Make relative numbers
t_mentions <- data.frame(t(mentions))
t_mentions <- t_mentions[,-13]
t_mentions$total <- rowSums(t_mentions)
t_mentions[,14:25] <- t_mentions[,1:12]/t_mentions$total
freq_mentions <- data.frame(t(t_mentions[,14:25]))
rownames(freq_mentions) <- rownames(mentions)[-13]
colnames(freq_mentions) <- colnames(mentions)

#Get rolling average
horizon <- 12
avg_mentions <- data.frame(matrix(nrow = nrow(freq_mentions), ncol = ncol(freq_mentions) - horizon))
rownames(avg_mentions) <- rownames(mentions)[-13]
colnames(avg_mentions) <- colnames(mentions)[-c(1:horizon)]
for(i in horizon:(ncol(avg_mentions + horizon))) {
  small_df <- freq_mentions[,c((i-horizon+1):i)]
  avg <- rowMeans(small_df, na.rm = TRUE)
  #get only the top 3 values
  top_indices <-  order(avg, decreasing=TRUE)[1:3]
  top_avg <- avg
  top_avg[-top_indices] <- NA_real_
  avg_mentions[,(i-horizon+1)] <- top_avg
}

#Make tidy and plot
avg_df <- data.frame(data.frame(date = colnames(avg_mentions), t(avg_mentions)))
tidy_avg <- gather(avg_df, key = "key", value = "value", "trend":"tmin11")
bad_indices <- which(is.na(tidy_avg$value))
clean_tidy_avg <- tidy_avg[-bad_indices,]
clean_tidy_avg <- clean_tidy_avg[order(clean_tidy_avg$date),]
rownames(clean_tidy_avg) <- seq(1, nrow(clean_tidy_avg))
clean_tidy_avg$date <- as.Date(clean_tidy_avg$date)
avg_plot <- ggplot(clean_tidy_avg, aes(x = date, y = value, color = key)) +
  geom_point()
avg_plot

