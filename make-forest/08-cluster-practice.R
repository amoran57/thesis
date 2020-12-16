library(parallel)
library(iterators)
library(foreach)
library(doParallel)

# parallel
split <- detectCores()
eachStart <- 25

cl <- makeCluster(split)
init <- clusterEvalQ(cl, { library(MASS); NULL })
results <- parLapplyLB(cl
                      ,rep(eachStart, split)
                      ,function(nstart) kmeans(Boston, 4, nstart=nstart))
withinss <- sapply(results, function(result) result$tot.withinss)
result <- results[[which.min(withinss)]]
stopCluster(cl)

result$tot.withinss
#[1] 1814438

# foreach
split <- detectCores()
eachStart <- 25
# set up iterators
iters <- iter(rep(eachStart, split))
# set up combine function
comb <- function(res1, res2) {
  if(res1$tot.withinss < res2$tot.withinss) res1 else res2
}

cl <- makeCluster(split)
registerDoParallel(cl)
result <- foreach(nstart=iters, .combine="comb", .packages="MASS") %dopar%
  kmeans(Boston, 4, nstart=nstart)
stopCluster(cl)

result$tot.withinss
#[1] 1814438