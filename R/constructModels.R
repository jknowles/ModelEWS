# ------------------------------------------------------------------------------
# Functions to train EWS models for later prediction
# These functions rely on model search results being stored in the following 
# format: "path/to/models/filename_GRADE_MODE.rda" where GRADE and MODE are 
# variables supplied by the user and modelResultsName is the path to the filename
# ------------------------------------------------------------------------------

#' Parse results of a model search
#'
#' @param modelResultsName a prefix that includes the path and filename of the 
#' file on the disk where the model search results object is stored
#' @param grade a numeric representing the grade level of the object
#' @param mode a character representing whether the model was run in dev or 
#' in production mode
#'
#' @return Returns a data.frame with the best fitting models. 
#' @export
modelSearchResults <- function(modelResultsName, grade, mode){
  fileName <- paste0(modelResultsName, "_GRADE_", grade, "_", mode, ".rda")
  load(file = fileName)
  NMODELS <- 8 # TODO: Expose this as user configurable
  modSuc <- unique(ModelFits$method[!is.na(ModelFits$auc)])
  print(modSuc)
  # Fails
  modFail <- unique(ModelFits$method[is.na(ModelFits$auc)])
  print(modFail)
  # Top 5
  teststats <- ModelFits[ModelFits$grp == "test",]
  topAUC <- unique(teststats$auc)[order(-unique(teststats$auc))][1:5]
  badAUC <- unique(teststats$auc)[order(unique(teststats$auc))][1:5]
  #Some stats on these
  bestMethod <- unique(ModelFits$method[ModelFits$auc %in% topAUC])
  
  # Calculate speed vs. auc
  modelMethods <- ModelFits[ModelFits$grp == "test", 
                            3:7][!duplicated(ModelFits[ModelFits$grp == "test", 
                                                       3:7]),]
  modelMethods <- modelMethods[!duplicated(modelMethods),]
  modelMethods$elapsedTime <- as.numeric(modelMethods$elapsedTime)
  modelMethods$efficiency <- (modelMethods$auc / sqrt(modelMethods$elapsedTime)) * 1000
  
  # Get top N model methods
  bestMethod <- modelMethods$method[order(-modelMethods$efficiency)][1:NMODELS]
  dissimMethods <- row.names(dissimMethod(na.omit(modelMethods$method), k = 4, 
                                          distancemethod = "Jaccard", n = 2, 
                                          what = "matrix"))
  print(dissimMethods)
  print(bestMethod)
  return(modelMethods)
}

#' Construct ensemble models from a data.frame of optional models
#'
#' @param modelResultsName a prefix that includes the path and filename of the 
#' file on the disk where the model search results object is stored
#' @param grade a numeric representing the grade level of the object
#' @param mode a character representing whether the model was run in dev or 
#' in production mode
#' @param DV a character representing the name of the dependent variable in the 
#' model
#' @param nmodels the number of models to include in an ensemble
#' @param conn a connection string pointing to the database where the gatherData 
#' function can extract data from for model building
#'
#' @return Nothing. Writes the model objects to disk
#' @details By default the function builds four kinds of models. It builds ensembles 
#' based on the top N performing models, and on the N most dissimilar models. To 
#' build these it uses both the caretStack and caretEnsemble method. 
#' @export
constructModels <- function(modelResultsName, grade, mode, DV = "grad_ind", 
                            nmodels = 8, conn){
  modelMethods <- modelSearchResults(modelResultsName = modelResultsName, grade = grade, 
                     mode = mode)
  NMODELS <- nmodels
  timeAllow <- grade * 100 * .85
  bestMethod <- modelMethods$method[order(-modelMethods$efficiency) & 
                                      modelMethods$elapsedTime < timeAllow][1:NMODELS]
  bestMethod <- na.omit(bestMethod)
  dissimMethods <- row.names(dissimMethod(na.omit(modelMethods$method), 
                                          k = 3, distancemethod = "Jaccard", 
                                          n = 3, what = "matrix"))
  dissimMethods <- modelMethods[modelMethods$method %in% dissimMethods, 
                                c("method", "elapsedTime", "efficiency")]
  dissimMethods <- dissimMethods[order(dissimMethods$efficiency),]
  dissimMethods <- unique(dissimMethods$method[dissimMethods$elapsedTime < timeAllow])[1:NMODELS]
  dissimMethods <- na.omit(dissimMethods)
  samp1 <- gatherData(grade = grade, conn = conn, DV = DV)
  devControl <- DEWScontrol(mode=mode)
  DEWSList <- makeDEWSList(samp1, MODE = devControl$mode); rm(samp1)
  fitControl2 <- trainControl(method='cv', number=10, 
                              classProbs=TRUE, summaryFunction=twoClassSummary, 
                              savePredictions = TRUE)
  if(devControl$upsample == "SMOTE"){
    DEWSList$traindata <- smote(DEWSList$traindata)
    message("Training data upsampled using SMOTE.")
  } 
  
  cl <- makeCluster(devControl$cores) 
  registerDoParallel(cl)
  out1 <- caretList(methodList = c(bestMethod), 
                    trControl = fitControl2, x = DEWSList$traindata$preds, 
                    y = DEWSList$traindata$class, 
                    tuneLength = 16, metric = "ROC")
  stopCluster(cl)
  out1.ens <- caretEnsemble(out1)
  cl <- makeCluster(devControl$cores) 
  registerDoParallel(cl)
  out1.stack <- caretStack(out1, method = "rf")
  stopCluster(cl)
  preds1 <- predict(out1.ens); rm(out1, preds1)
  cl <- makeCluster(devControl$cores) 
  registerDoParallel(cl)
  out2 <- caretList(methodList = c(dissimMethods), 
                    trControl = fitControl2, x = DEWSList$traindata$preds, 
                    y = DEWSList$traindata$class, 
                    tuneLength = 16, metric = "ROC")
  stopCluster(cl)
  out2.ens <- caretEnsemble(out2, optFUN = safeOptAUC)
  cl <- makeCluster(devControl$cores) 
  registerDoParallel(cl)
  out2.stack <- caretStack(out2, method = "gbm")
  stopCluster(cl)
  preds2 <- predict(out2.ens); rm(out2, preds2)
  
  varScales <- DEWSList$scale
  # Drop out all DEWSList elements but $validdata
  validSet <- vector(1, mode = 'list')
  names(validSet) <- "validdata"
  validSet$validdata <- DEWSList$validdata
  eval(parse(text = paste0("save(out1.ens, out1.stack, out2.ens, out2.stack, 
        varScales,validSet,
        file = 'cache/models/GRADE_", grade, "_EnsembleModels_APRIL.rda')")))
}
