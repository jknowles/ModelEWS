################################################################################
# The EWS Testing Routine
#
# 1. Get data set up into test and training sets
# 2. Set test parameters
# 3. Set up functions to get outputs from the model fit tests
# 4. Set tolerances and run times
# 5. Import models to test
# 6. Execute test and capture results
################################################################################

################################################################################
##
## Set Test Parameters
##
################################################################################


#' Set up parameters to control the modelSearch and modelBuild tasks in DEWS
#' @param mode character vector with three choices - "DEV", "PROD", and "TEST"
#' @param grade a numeric specifying the grade level
#' @description This function defines three possible streams for the DEWS model 
#' searching and model training routines to take. PROD is the most rigorous, while 
#' DEV is the quickest for debugging and testing purposes. TEST is somewhere in the 
#' middle. Here the 
#' @importFrom caret trainControl
#' @return A list with elements 
#' \itemize{
#' \item upsample - a character representing the upsampling technique to use, currently 
#' only SMOTE is supported
#' \item length - a numeric representing the length of the tuneGrid to be passed to 
#' the tuneLength option in \code{\link{caret}}
#' \item metric - a character representing the metric to use to identify the 
#' best model, currently ROC is all that is supported
#' \item core - a numeric specifying the number of cores to use. In DEV, this is 
#' capped at 1/2 the maximum number of available cores, in other modes it is 
#' 2/3
#' \item folds - a numeric representing the number of folds to use in training models
#' \item reps - numeric representing the number of repeats to use in training models
#' \item cvmethod - character representing the method to use for cross-validation, 
#' PROD is repeatedcv, dev and test are cv
#' \item dewsTrControl - a \code{\link{trainControl}} object derived from values
#' \item mode - a character storing the mode
#' }
#' Any of these values can be modified posthoc by editing the list elements 
#' directly.
#' @note This function is extremely proscriptive and should be modified 
#' based on the user environment. 
#' @importFrom parallel detectCores
#' @export
DEWScontrol <- function(mode, grade=NULL){
  mode <- match.arg(mode, choices = c("DEV", "PROD", "TEST"))
  machCores <- parallel::detectCores()
  if(mode == "DEV"){
    upsample <- "SMOTE"
    cores <- ceiling(machCores / 2)
    # Metric
    metric <- "ROC"
    length <- 4
  } else if(mode == "PROD"){
    upsample <- "SMOTE"
    cores <- ceiling(machCores / 1.5)
    metric <- "ROC"
    length <- 9
  } else{
    upsample <- "NONE"
    cores <- ceiling(machCores / 1.5)
    metric <- "ROC"
    length <- 6
  }
  # CV settings
  folds <- ifelse(mode == "DEV", 5, 10)
  reps  <- ifelse(mode == "DEV", 1, 3)
  cvmethod <- ifelse(mode == "DEV", "cv", "repeatedcv")
  # Train methods for caret package to estimate the test error
  if(mode == "PROD"){
    ctrl <- caret::trainControl(method='cv', number=folds, 
                         classProbs=TRUE, 
                         summaryFunction=ifelse(metric == "Dist", fourStatsSummary, twoClassSummary), 
                         savePredictions = FALSE)
  } else if(mode == "DEV"){
    ctrl <- caret::trainControl(method='cv', number=folds, savePredictions = FALSE, 
                         classProbs=TRUE, 
                         summaryFunction=ifelse(metric == "Dist", fourStatsSummary, twoClassSummary))
  }
  return(list(upsample = upsample, length = length, metric = metric, 
              cores = cores, folds = folds, reps = reps, cvmethod = cvmethod, 
              dewsTrControl = ctrl, mode = mode))
}


#' Clean up the workspace after fitting models
#' @description a helper function to clean up the workspace after running a modelSearch
#' @return nothing
#' @import plyr
#' @import doParallel
#' @import pROC
#' @import EWStools
#' @import caret
cleanUp <- function(){
  pkgs <- names(sessionInfo()$otherPkgs) 
  pkgs <- paste('package:', pkgs, sep = "")
  try(lapply(pkgs, detach, character.only = TRUE, unload = TRUE, force = TRUE))
  library(plyr)
  library(doParallel)
  library(caret)
  library(pROC)
  library(EWStools)
  Sys.sleep(10)
}

#' Build a list of methods to evaluate performance of
#' @param n numeric, number of models to extract
#' @param full logical, get all methods or just a small subset
#' @return vector of character strings that represent methods in caret
#' @description Identify the caret methods for training DEWS models on. This can 
#' build a list out of the data built into EWStools or restrict to a small subset 
#' of models. 
#' @import EWStools
#' @note Excludes all methods that require Java by default. Restricted list 
#' includes the list used in the Wisconsin implementation of DEWS as of June 2015.
#' @export
getCandidateMethods <- function(n=NULL, full=FALSE){
  data(caretMethods, package = "EWStools")
  candidatemods <- caretMethods[caretMethods$requiresJava == "no" & 
                                  caretMethods$type != "Regression", ]
  candidatemods <- unique(candidatemods$methodName)
  if(full == FALSE){
    candidatemods <- c("slda", "mda", "treebag", "hdda", "pls", "ORFlog", "C5.0Rules", 
      "rf", "earth", "stepLDA", "xyf", "kernelpls", 
      "spls", "pda", "pam", "svmRadialCost", "ORFpls", 
      "rpart", "plr", "pcaNNet", "svmLinear", "glmnet", "nb", "gcvEarth")
    # Methods that take too long Boruta, RRFglobal, evtree, svmPoly
  }
  #Shuffle up the models
  if(missing(n)){
    n <- length(unique(candidatemods))
  }
  candidatemods <- sample(candidatemods, n, replace = FALSE)
  return(candidatemods)
}

#' Evaluate and store model performance across a list of models
#' @description This is the workhorse of DEWS as it conducts a search of models 
#' using DEWScontrol on the data in the DEWSList. 
#' @param DEWSList a DEWSList created using the makeDEWSList function
#' @param DEWScontrol a control object specifying what kind of search to make
#' @return A data.frame with model performance parameters for each of the methods 
#' tested.
#' @import EWStools
#' @export
DEWS_search <- function(DEWSList, DEWScontrol){
  if(DEWScontrol$metric == "ROC") {
    ModelFits <- EWStools:::buildROCcurveFrame("glm")[1,]
    } else {
    ModelFits <- EWStools:::buildDISFrame("glm")[1,]
    }
  cleanUp()
  if(DEWScontrol$upsample == "SMOTE"){
    DEWSList$traindata <- smote(DEWSList$traindata)
    message("Training data upsampled using SMOTE.")
  } 
  
  candidatemods <- getCandidateMethods()
  chunks <- 1:floor(length(candidatemods)/6)
  for(i in chunks){
    strt <- length(candidatemods) / length(chunks) * (i - 1)
    strt <- floor(strt) + 1
    myend <- length(candidatemods) / length(chunks) * i
    myend <- floor(myend)
    tmpMods <- candidatemods[strt:myend]
    callList <- list("methods" = quote(tmpMods), 
                     "datatype" = c("train", "test"), 
                     "traindata" = quote(DEWSList$traindata),
                     "testdata" = quote(DEWSList$testdata), 
                     "modelKeep" = FALSE, 
                     "fitControl" = quote(DEWScontrol$dewsTrControl),
                     "length" = DEWScontrol$length, 
                     "metric" = DEWScontrol$metric,
                     "cores" = DEWScontrol$cores,
                     "maximize" = ifelse(DEWScontrol$metric == "ROC", TRUE, FALSE))
    
    ModelFits.tmp <- do.call(modSearch, callList)
    
    ModelFits <- rbind(ModelFits, ModelFits.tmp)
    Sys.sleep(10)
    cleanUp()
    print(paste("Finished chunk", i, sep = " "))
  }
  return(ModelFits)
}

#' A wrapper function to simplify the DEWS search process
#' @param grade numeric corresponding to the cohort grade
#' @param mode a single character vector, one of "DEV", "PROD", or "TEST" for DEWSControl
#' @param fileName a character for a stub of a filename to store results to
#' @param DV a character with the dependent variable name
#' @return Nothing, stores the results of a DEWS_search to disk
#' @export
modelSearch <- function(grade, mode = "PROD", fileName, DV = "grad_ind"){
  devControl <- DEWScontrol(mode=mode)
  samp1 <- gatherData(grade = grade, conn = ds[[4]], DV = DV)
  full <- makeDEWSList(samp1, MODE = devControl$mode); rm(samp1)
  modelMethods <- DEWS_search(DEWSList = full, DEWScontrol = devControl)
  
  fileName <- paste0(fileName, "_GRADE_", grade, "_", devControl$mode, ".rda")
  save(modelMethods, file = fileName)
}