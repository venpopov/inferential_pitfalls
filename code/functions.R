# function for training and testing a linear encoding model with cross-validation
encoding_model <- function(response, model, nFolds=10, ...) {
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
      X <- model
      lm.fit[[k]][[i]] <- lm(Y ~ ., data=X, subset=train_ids)
      fitted[,i+1] <- predict(lm.fit[[k]][[i]], data.frame(X[-train_ids,]), type="response")
    }
    
    # save predictions
    pred[[k]] <- fitted
  }
  
  return(pred)
}

# function to transform a decimal number into a binary vector with length nFeat
dec2bin = function(number, nFeat=32) {
  binary_vector = rev(as.numeric(intToBits(number)))
  binary_vector[-(1:(length(binary_vector) - nFeat))]
}

# function to transform a number into a vector of length nFeat
num2vect <- function(number, nFeat) {
  aschar <- as.character(number)
  vector <- as.numeric(unlist(strsplit(aschar,'')))
  vector <- c(rep(0, nFeat-length(vector)), vector)
  return(vector)
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

# function for getting voxel activations by setting preferred value for each
# neuron/voxel, and getting a response based on a normal distribution centered
# around that prefered value with a selectivity_sd as the std. The noise_sd
# specifies the sd of gausian noise added to each voxel activation
population_code_response <- function(att, ...) {
  nDimensions = ncol(repr)
  nVoxels <- length(prefered)
  nObs <- dim(repr)[1]
  voxels <- c()
  
  # loop over dimensions in the representational space. Get gaussian response to
  # stimulus based on prefered value and add gaussian noise
  for (i in 1:nDimensions) {
    # if attention is modulated towards the current dimension, increase selectivity:
    selectivity_sd_actual = ifelse(i == att, att_selectivity_sd, selectivity_sd)
    
    dim <- repr[,i]
    voxels_dim <- sapply(prefered, function(x) dnorm(dim, mean=x, sd=selectivity_sd_actual))
    noise <- rnorm(nObs*nVoxels, sd=noise_sd) %>% matrix(nrow=nObs)
    voxels_dim <- round(voxels_dim + noise, 2)
    voxels <- cbind(voxels, voxels_dim)
  }
  return(voxels)
}

# workflow function toget population code response, fit encoding model and extract percential rank accuracy
run_simulation <- function(...) {
  voxels <- population_code_response(...)
  pred <- encoding_model(voxels, ...)
  prank <- get_percentile_rank_acc(voxels, pred)
  acc <- mean(unlist(prank))  
  return(acc)
}

# function for running all models and conditions and returning the accuracy for each
run_color_simulation <- function(selectivity_sd, noise_sd, att_selectivity_sd) {
  acc <<- rep(0, 8)
  prefered <<- c(0.1,0.3,0.5,0.7,0.9)
  repr <<- vrgb[1:3]
  att_selectivity_sd <<- att_selectivity_sd
  selectivity_sd <<- selectivity_sd
  noise_sd <<- noise_sd
  
  
  acc[1] <- run_simulation(model=vrgb[1:3],att=0) # no attention
  acc[2] <- run_simulation(model=vrgb[1:3],att=1) # r attention
  acc[3] <- run_simulation(model=vrgb[1:3],att=2) # g attention
  acc[4] <- run_simulation(model=vrgb[1:3],att=3) # b attention
  acc[5] <- run_simulation(model=vhsv[1:3],att=0) # no attention
  acc[6] <- run_simulation(model=vhsv[1:3],att=1) # r attention
  acc[7] <- run_simulation(model=vhsv[1:3],att=2) # g attention
  acc[8] <- run_simulation(model=vhsv[1:3],att=3) # b attention
  
  res <- data.frame(att = rep(c('control','r','g','b'),2),
                    model = rep(c('rgb','hsv'), each=4),
                    acc = acc)  
  res$att <- factor(res$att, levels=c('control','r','g','b'))
  return(res)
}  