# function for training and testing a linear encoding model with cross-validation
encoding_model <- function(response, predictors, nFolds=10) {
  N_resp <- dim(response)[2] 
  ids = 1:nrow(response)
  pred <- list()
  lm.fit <- list()
  
  # generate training samples for 10-fold cross validation
  trainFolds <- createFolds(response[,1], k=10, returnTrain = TRUE)
  
  # loop over different training samples for cross validation
  for (k in 1:length(trainFolds)) {
    train_ids <- trainFolds[[k]]
    lm.fit[[k]] <- list()
    fitted <- data.frame(id = ids[-train_ids])
    
    # create a lm for each feature of the response vector and store the predicted values for 
    # the test set in fitted
    for (i in 1:N_resp) {
      Y <- response[,i]
      X <- predictors
      lm.fit[[k]][[i]] <- lm(Y ~ ., data=X, subset=train_ids)
      fitted[,i+1] <- predict(lm.fit[[k]][[i]], data.frame(X[-train_ids,]), type="response")
    }
    
    # save predictions
    pred[[k]] <- fitted
  }
  
  return(pred)
}


# function for calculating percentile rank
get_percentile_rank_acc <- function(obs, pred) {
  prank <- list()
  for (k in 1:length(pred)) {
    # get predicted and observed maps for this fold
    PM <- pred[[k]]
    OM <- obs[PM$id,]
    
    # calculate euclidean distance between each stim's PM and the OMs of all stims
    dists <- matrix(0, nrow=length(PM$id), ncol=length(PM$id))
    for (i in 1:length(PM$id)) {
      dists[i,] <- sqrt(rowSums((sweep(OM,2,as.vector(t(PM[i,-1]))))^2))
    }
    
    # get percentile rank
    prank[[k]] <- 1-diag((t(apply(dists,1,rank))-1)/dim(dists)[2])
  }
  return(prank)
}