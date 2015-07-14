#-------------------------------------------------------------------------------
# Predictions
#-------------------------------------------------------------------------------

#' Generate predictions for a new cohort of students using a model trained by DEWS
#' @description Blah blah
#' @param stub
#' @param grade
#' @param mode
#' @param year
#' @param findBest
#' @param cache
#' @return Nothing.
#' @export
rawPreds <- function(stub, grade, conn, mode = "PROD", year = "2013-2014", 
                     findBest = TRUE, cache=FALSE){
  fileName <- paste0("cache/models/GRADE_", grade, "_", stub, ".rda")
  load(file = fileName)
  grade2 <- paste0("0", grade, "")
  estCohort <- gatherData.pred(grade = grade2, year = year, conn = conn, 
                               na.omit = TRUE, type = "predict")
  
  if(findBest == TRUE){
    modList <- vector(length = 4, mode = "list")
    names(modList) <- c("out1.ens", "out1.stack", "out2.ens", "out2.stack")
    
    if(!exists("validSet")){
      samp1 <- gatherData(grade = grade, conn = conn, DV = "grad_ind")
      devControl <- DEWScontrol(mode=mode)
      DEWSList <- makeDEWSList(samp1, MODE = devControl$mode); rm(samp1)
      
      for(i in 1:length(modList)){
        tmp <- try(do.call(ROCtest, 
                                    args = list(mod = eval(parse(text=names(modList[i]))), 
                                                testdata = DEWSList$validdata)))
        modList[[i]] <- try(tmp@auc)
      }
      bestMod <- names(which.max(modList))      
      bestThresh <- findBestThresh(eval(parse(text=bestMod)), DEWSList)
    } else{
      for(i in 1:length(modList)){
        tmp <- try(do.call(ROCtest, 
                                    args = list(mod = eval(parse(text=names(modList[i]))), 
                                                testdata = validSet$validdata)))
        modList[[i]] <- try(tmp@auc)
      }
      bestMod <- names(which.max(modList))  
      bestThresh <- findBestThresh(eval(parse(text=bestMod)), validSet)
    }
  }
  
  varNames <- names(out1.ens$models[[1]]$trainingData)
  varNames <- varNames[!varNames %in% ".outcome"] 
  
  estCohortScaled <- preProcess.pred(preProcess = varScales, 
                                     predCohort = estCohort, varNames = varNames)
  
  bigPred <- predict(eval(parse(text=bestMod)), 
                     newdata = estCohortScaled, se=TRUE)
  out <- cbind(bigPred, estCohortScaled)
  out$bestThresh <- bestThresh
  out$STUDENT_KEY <- row.names(out)
  if(cache == TRUE){
    fileName <- paste0("cache/predictions/GRADE_", grade, "_", stub, "_" , year,
                       ".rda")
    save(out, varScales, file = fileName)
  } else if(cache == FALSE){
    return(out) 
  }
}


#' Find the best threshold for a model
#' @description Blah blah blah.
#' @param mod
#' @param dataList
#' @return A numeric representing the optimal probability threshold cutoff.
#' @export
findBestThresh <- function(mod, dataList){
  newPreds <- as.data.frame(NA)
  for(i in names(dataList)){
    if(!is.null(dataList[[i]]$preds)){
      newPreds <- rbind.fill(newPreds, dataList[[i]]$preds)
    } else{
      
    }
  }
  newPreds <- newPreds[-1, -1]
  if(class(mod) == "caretEnsemble"){
    bigpreds2 <- predict(mod, keepNA = TRUE, 
                         newdata = newPreds)    
  } else if(class(mod) == "caretStack"){
    bigpreds2 <- predict(mod, type = "prob", 
                         newdata = newPreds)
    bigpreds2 <- bigpreds2[, 1]
  }
  
  newClass <- NA
  for(i in names(dataList)){
    if(!is.null(dataList[[i]]$class)){
      newClass <- c(newClass, dataList[[i]]$class)
    } else{
      
    }
  }
  newClass <- newClass[-1]
  newClass <- factor(ifelse(newClass == 1, "Grad", "Non.Grad"))
  myroc <- roc(newClass ~ bigpreds2, levels = c("Grad", "Non.Grad"), 
               algorithm = 2)
  prop <- length(newClass[newClass == "Non.Grad"]) / length(newClass)
  mycoord <- coords(myroc, x = "best", best.method = "closest.topleft", 
                    best.weights = c(1, prop))
  BASETHRESH_2 <- mycoord[[1]]
  return(BASETHRESH_2)
}
