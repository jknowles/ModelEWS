# ################################################################################
# # Impute preliminary data
# #
# #
# ################################################################################
# 
# ################################################################################
# ##
# ## Load packages
# ##
# ################################################################################
# library(doParallel)
# library(caret)
# library(pROC)
# #library(R.utils)
# #devtools::install_github("jknowles/EWStools")
# library(EWStools)
# #devtools::install_github("jknowles/caretEnsemble")
# library(caretEnsemble)
# library(eeptools)
# ################################################################################
# ##
# ## Set Parameters
# ##
# ################################################################################
# 
# GRADE <- 8
# NMODELS <- 8
# CORES <- 16
# LENGTH <- 8
# source("R/input_data_cleaning.R")
# source("R/transformTrainingData.R")
# #  newnamesLAG <- newnamesLAG[-1]
# AttR <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = attF, varnames = varnamesA, 
#                     preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "att_rate")
# 
# DisD <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = disF, varnames = varnamesDis, 
#                    preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "disdays")
# 
# 
# PossA <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = PattF, varnames = varnamesPA, 
#                    preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "poss_att_days")
# 
# # Need to oversample here
# MobD <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = mobDF, varnames = varnamesMobd, 
#                    preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "mobility_flag")
# #Need to oversample here
# 
# # MobS <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = mobSF, varnames = varnamesMobs, 
# #                    preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "mobility_sch")
# 
# impute <- list(attrate = AttR, discipline = DisD, possatt = PossA, 
#                mobd = MobD)
# 
# rm(AttR, DisD, PossA, MobD)
# gc()
# ################################################################################
# ##
# ## Set Test Parameters
# ##
# ################################################################################
# 
# #MODE <- "PROD"
# MODE <- "DEV"
# # myOS <- Sys.info()['sysname']
# 
# # CV settings
# folds <- ifelse(MODE == "DEV", 3, 5)
# reps  <- ifelse(MODE == "DEV", 1, 3)
# 
# # Train methods for caret package to estimate the test error
# if(MODE == "PROD"){
#   ctrl <- trainControl(method='repeatedcv', number=folds, repeats = reps,
#                               savePredictions = TRUE)
# } else if(MODE == "DEV"){
#   ctrl <- trainControl(method='cv', number=folds, savePredictions = TRUE)
# }
# 
# # Set the tune length for the train function
# # algorithm parameters to evaluate 
# LENGTH <- ifelse(MODE == "PROD", 6, 4)
# 
# # Show the user what they have selected
# print("The following settings have been chosen:")
# print(paste0("Running in ", myOS))
# print(paste0("Mode: ", MODE))
# print(paste0("CPU Cores: ", CORES))
# 
# #
# mods <- read.csv("data/imputeModels.csv", stringsAsFactors=FALSE)
# # Read in candidate models from a list and subset
# candidatemods <- mods$method..[mods$EWS.=="Yes"]
# 
# 
# ModelFitsDis <- modSearch(methods = candidatemods, 
#                        datatype = c("train", "test"), 
#                        traindata = impute$discipline$traindata, 
#                        testdata = impute$discipline$testdata,
#                        modelKeep = FALSE, 
#                        length = LENGTH, fitControl = ctrl, 
#                        metric = "RMSE", cores = CORES)
# 
# 
# 
# Sys.sleep(20)
# 
# 
# ModelFitsAtt <- modSearch(methods = candidatemods, 
#                        datatype = c("train", "test"), 
#                        traindata = impute$attrate$traindata, 
#                        testdata = impute$attrate$testdata,
#                        modelKeep = FALSE, 
#                        length = LENGTH, fitControl = ctrl, 
#                        metric = "RMSE", cores = CORES)
# 
# Sys.sleep(20)
# 
# ModelFitsPossAtt <- modSearch(methods = candidatemods, 
#                           datatype = c("train", "test"), 
#                           traindata = impute$possatt$traindata, 
#                           testdata = impute$possatt$testdata,
#                           modelKeep = FALSE, 
#                           length = LENGTH, fitControl = ctrl, 
#                           metric = "RMSE", cores = CORES)
# 
# Sys.sleep(20)
# 
# # Let's go ROC here
# mods <- read.csv("data/ewsModels.csv", stringsAsFactors=FALSE)
# # Read in candidate models from a list and subset
# candidatemods <- mods$method..[mods$EWS.=="Yes"]
# 
# 
# # Train methods for caret package to estimate the test error
# if(MODE == "PROD"){
#   ctrl <- trainControl(method='cv', number=5, 
#                              savePredictions = TRUE, 
#                              classProbs=TRUE, summaryFunction = fourStatsSummary)
# } else if(MODE == "DEV"){
#   ctrl <- trainControl(method='cv', number=3, savePredictions = TRUE, 
#                              classProbs=TRUE, summaryFunction = fourStatsSummary)
# }
# 
# 
# 
# impute$mobd$traindata$class <- ifelse(impute$mobd$traindata$class > 2, "Move", "NoMove")
# impute$mobd$testdata$class <- ifelse(impute$mobd$testdata$class > 2, "Move", "NoMove")
# impute$mobd$testdata$class <- factor(impute$mobd$testdata$class)
# impute$mobd$traindata$class <- factor(impute$mobd$traindata$class)
# impute$mobd$validdata$class <- ifelse(impute$mobd$validdata$class > 2, "Move", "NoMove")
# impute$mobd$validdata$class <- factor(impute$mobd$validdata$class)
# 
# # upsample due to severe class imbalance here
# 
# NEWTRAIN <- upSample(impute$mobd$traindata$preds, impute$mobd$traindata$class, 
#                      list = TRUE)
# names(NEWTRAIN) <- c("preds", "class")
# 
# NEWTEST <- upSample(impute$mobd$testdata$preds, impute$mobd$testdata$class, 
#                      list = TRUE)
# names(NEWTEST) <- c("preds", "class")
# 
# ModelFitsMob <- modSearch(methods = candidatemods, 
#                               datatype = c("train", "test"), 
#                               traindata = NEWTRAIN, 
#                               testdata = NEWTEST,
#                               modelKeep = FALSE, maximize = FALSE,
#                               length = LENGTH, fitControl = ctrl, 
#                               metric = "Dist", cores = CORES)
# 
# 
# fn <-paste0("cache/imputation/Grade", GRADE, "ImputationStats.rda")
# 
# 
# save(ModelFitsAtt, ModelFitsDis, ModelFitsMob, ModelFitsPossAtt, 
#      file = fn)
# 
# rm(list = ls())
