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

this <- list(infl_mbd$tmin2, infl_mbd$tmin2)
this <- c(this, this, this)
this <- c(this, this, this, this, this)

old_obj_function <- function(x, y, lag) {
  this_df <- data.frame(x, y, lag) %>% 
    dplyr::arrange(x)
  
  y <- this_df$y
  lag <- this_df$lag
  x <- this_df$x
  
  splits <- unique(x)
  ssr <- c()
  
  for(i in seq_along(splits)) {
    sp <- splits[i]
    
    first_y <- y[x < sp]
    first_lag <- lag[x < sp]
    second_y <- y[x >= sp]
    second_lag <- lag[x >= sp]
    
    if(length(first_y) > 0) {
      first_reg <- lm(first_y ~ first_lag)
      first_residuals <- first_reg$residuals
      first_ssr <- sum(first_residuals^2)
    } else {
      first_ssr <- 0
    }
    
    second_reg <- lm(second_y ~ second_lag)
    second_residuals <- second_reg$residuals
    second_ssr <- sum(second_residuals^2)
    
    ssr[i] <- first_ssr + second_ssr
  }
  
  split_at <- splits[which.min(ssr)]
  return(c(ssr = min(ssr), split = split_at))
}
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

tic("slow")
slow <- plyr::laply(
  this,
  old_obj_function,
  y = infl_mbd$t,
  lag = infl_mbd$tmin1,
  .progress = "text"
)
toc()

tic("fast")
fast <- plyr::laply(
  this,
  old_obj_function,
  y = infl_mbd$t,
  lag = infl_mbd$tmin1,
  .progress = "text"
)
toc()
