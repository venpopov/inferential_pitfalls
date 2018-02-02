# --------------------------------------------------------------------------
# PRELIMINARIES
# --------------------------------------------------------------------------
rm(list=ls())
library(class)
library(MASS)
library(caret)
library(reshape2)
library(tidyverse)
library(here)
setwd(here()) # set wd as the parent folder which contains an .rproj file
source('code/functions.R')

# --------------------------------------------------------------------------
# GENERATE DATA
# --------------------------------------------------------------------------
# number to use as stimuli
items <- 0:99999  

# calculate number of decimal and binary features
nDecFeat <- max(nchar(items))                   
nBinFeat <- 32-which(dec2bin(max(items))==1)[1] 

# get a data.frame with each item, its decimal and binary vector representations
numbers <- cbind(items,
                 t(sapply(items, num2vect, nFeat=nDecFeat)),
                 t(sapply(items, dec2bin, nFeat=nBinFeat)))
numbers <- as.data.frame(numbers)
names(numbers) <- c('item', paste0('d',1:nDecFeat), paste0('b',1:nBinFeat))
#remove random numbers to reduce set size to 10 000 items
set.seed(5346)
propKeep <- 0.1
idxKeep <- sample(nrow(numbers), size=round(propKeep*nrow(numbers))) 
numbers <- numbers[idxKeep,]
items <- items[idxKeep]


# --------------------------------------------------------------------------
# MODELLING
# --------------------------------------------------------------------------
# fit encoding model and extract percential rank accuracy
set.seed(345)
pred <- encoding_model(numbers[,paste0('b',1:nBinFeat)], 
                       numbers[,paste0('d',1:nDecFeat)])
prank <- get_percentile_rank_acc(numbers[,paste0('b',1:nBinFeat)], pred)

# mean rank accuracy and wilcox test for significance
mean(unlist(prank))
wilcox.test(unlist(prank), mu=0.5)
