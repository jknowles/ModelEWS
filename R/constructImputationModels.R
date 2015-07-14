# #constructImputeModels
# 
# # Attendance
# 
# library(EWStools)
# library(doParallel)
# library(caretEnsemble)
# GRADE <- 5
# NMODELS <- 8
# CORES <- 16
# LENGTH <- 8
# 
# ################################################################################
# ##
# ## Load data
# ##
# ################################################################################
# 
# # Load results of EWS_Model_Search
# 
# eval(parse(text = paste0("load(file = 'cache/imputation/Grade", GRADE, "ImputationStats.rda')")))
# 
# # load training data
# source("R/input_data_cleaning.R")
# source("R/transformTrainingData.R")
# 
# IMPUTEVAR <- c("ATT", "DIS", "PossAtt", "MobD")
# 
# for(i in IMPUTEVAR){
#   if(i == "ATT"){
#     dat <- gatherData(grade = GRADE, newnames = newnamesLAG[-1], dataformula = attF, 
#                       preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "att_rate")
#     ModelFits <- ModelFitsAtt
#     
#   } else if(i == "DIS"){
#     dat <-  gatherData(grade = GRADE, newnames = newnamesLAG[-1], dataformula = disF, 
#                        preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "disdays")
#     ModelFits <- ModelFitsDis
#     
#   } else if(i == "PossAtt"){
#     dat <- gatherData(grade = GRADE, newnames = newnamesLAG[-1], dataformula = PattF, varnames = varnamesPA, 
#                       preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "poss_att_days")
#     ModelFits <- ModelFitsPossAtt
#   } else if(i == "MobD"){
#     # Need to oversample here
#     dat <- gatherData(grade = GRADE, newnames = newnamesLAG[-1], dataformula = mobDF, varnames = varnamesMobd, 
#                        preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "mobility_flag")
#     ModelFits <- ModelFitsMob
#     #Need to oversample here
#   }
# 
#   
# if(i != "MobD"){
#   teststats <- ModelFits[ModelFits$grp == "test",]
#   hiRMSE <- unique(teststats$RMSE)[order(-unique(teststats$RMSE))][1:NMODELS]
#   loRMSE <- unique(teststats$RMSE)[order(unique(teststats$RMSE))][1:NMODELS]
#   bestMethod <- unique(ModelFits$method[ModelFits$RMSE %in% loRMSE])
#   # Calculate speed vs.RMSE
#   modelMethods <- ModelFits[ModelFits$grp == "test", 
#                             c(1,2,5,6,7)][!duplicated(ModelFits[ModelFits$grp == "test", 
#                                                                 c(1,2,5,6,7)]),]
#   
#   modelMethods$efficiency <- round(10/(modelMethods$elapsedTime + modelMethods$RMSE^2), digits = 2)
#   bestMethod2 <- modelMethods$method[order(-modelMethods$efficiency)][1:NMODELS]
#   
#   fitControl2 <- trainControl(method='cv', number=5, 
#                               savePredictions = TRUE)
#   # drop icr and knn, 
#   # can't handle missing data
#   bestMethod2 <- bestMethod2[bestMethod2 != "knn"]
#   bestMethod2 <- bestMethod2[bestMethod2 != "kknn"]
#   bestMethod2 <- bestMethod2[bestMethod2 != "icr"]
#   cl <- makeCluster(CORES) 
#   registerDoParallel(cl)
#   
#   out1 <- buildModels(methodList = bestMethod2, control = fitControl2, 
#                       x = omitLinearCombos(dat$testdata$preds), 
#                       y = dat$testdata$class,  
#                       tuneLength = LENGTH, metric = "RMSE")
#   lapply(out1, class)
#   stopCluster(cl)
#   eval(parse(text= paste0("ensemble", i, "<- caretEnsemble(out1); rm(out1)")))
#   eval(parse(text = paste0("scaleDat", i, "<- dat$scale; rm(dat)")))
# } else{
#   # Commented out now because no time left to test so many models
# #   teststats <- ModelFits[ModelFits$grp == "test",]
# #   hiDIST <- unique(teststats$DIST)[order(unique(teststats$DIST))][1:NMODELS]
# #   loDIST <- unique(teststats$DIST)[order(-unique(teststats$DIST))][1:NMODELS]
# #   bestMethod <- unique(ModelFits$method[ModelFits$DIST %in% loDIST])
# #   
# #   # Calculate speed vs.RMSE
# #   modelMethods <- ModelFits[ModelFits$grp == "test", 
# #                             c(1,2,5,6,7)][!duplicated(ModelFits[ModelFits$grp == "test", 
# #                                                                 c(1,2,5,6,7)]),]
# #   
# #   modelMethods$efficiency <- round(10/(modelMethods$elapsedTime + modelMethods$DIST^2), digits = 2)
# #   bestMethod2 <- modelMethods$method[order(-modelMethods$efficiency)][1:NMODELS]
# #   
# # 
# #   
# #   # drop icr and knn, 
# #   # can't handle missing data
# #   bestMethod2 <- bestMethod2[bestMethod2 != "knn"]
# #   bestMethod2 <- bestMethod2[bestMethod2 != "kknn"]
# #   bestMethod2 <- bestMethod2[bestMethod2 != "icr"]
#   
#   
#   tmp.pred <- rbind(dat$testdata$preds, dat$traindata$preds)
#   tmp.class <- c(dat$testdata$class, dat$traindata$class)
#   tmp.class <- ifelse(tmp.class > 2, "Move", "NoMove")
#   tmp.class <- factor(tmp.class)
#   
#   NEWTRAIN <- downSample(tmp.pred, tmp.class,list = TRUE)                         
#   names(NEWTRAIN) <- c("preds", "class")
#   NEWTRAIN$preds <- omitLinearCombos(NEWTRAIN$preds)
#   rm(tmp.pred, tmp.class)
#   
#   bestMethod2 <- c("glm", "cforest", "ctree2", "glmboost", "bagEarth")
#   ctrl <- trainControl(method='cv', number=5, 
#                               savePredictions = TRUE, 
#                               classProbs=TRUE, summaryFunction = twoClassSummary)
#   cl <- makeCluster(CORES) 
#   registerDoParallel(cl)
#   
#   
#   out1 <- buildModels(methodList = bestMethod2, control = ctrl, 
#                       x = NEWTRAIN$preds, 
#                       y = NEWTRAIN$class,  
#                       tuneLength = LENGTH, metric = "ROC")
#   
#   lapply(out1, class)
#   stopCluster(cl)
#   
#   eval(parse(text= paste0("ensemble", i, "<- caretEnsemble(out1); rm(out1)")))
#   eval(parse(text = paste0("scaleDat", i, "<- dat$scale; rm(dat)")))
# }
# }
#   
# 
# eval(parse(text = paste0("save(ensembleATT, ensembleDIS, ensembleMobD, 
#                         ensemblePossAtt, scaleDatATT, scaleDatDIS, scaleDatMobD, 
#                         scaleDatPossAtt,
#                         file = 'cache/imputation/models/GRADE_", GRADE, 
#                          "EnsembleModels.rda', 
#                          compress = 'bzip2')")))
# 
# 
# #   
# #   ModelFits2 <- modSearch(methods = bestMethod2, datatype=c("train", "test"),
# #                           traindata = dat$traindata,
# #                           testdata = dat$validdata, modelKeep = FALSE, 
# #                           length = LENGTH, fitControl = fitControl2, 
# #                           metric = "RMSE", cores = CORES)
# 
# #################################################################################
# #################################################################################
# #################################################################################
# # IMPUTEVAR <- "PossAtt"
# # 
# # if(IMPUTEVAR == "ATT"){
# #   dat <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = attF, varnames = varnamesA, 
# #                     preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "att_rate")
# #   ModelFits <- ModelFitsAtt
# #   
# # } else if(IMPUTEVAR == "DIS"){
# #   dat <-  gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = disF, varnames = varnamesDis, 
# #                      preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "disdays")
# #   ModelFits <- ModelFitsDis
# #   
# # } else if(IMPUTEVAR == "PossAtt"){
# #   dat <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = PattF, varnames = varnamesPA, 
# #                     preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "poss_att_days")
# #   ModelFits <- ModelFitsPossAtt
# # } else if(IMPUTEVAR == "MobD"){
# #   dat <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = mobDF, varnames = varnamesMobd, 
# #                     preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "mobility_dist")
# #   ModelFits <- ModelFitsMobD
# #   
# # } else if(IMPUTEVAR == "MobS"){
# #   dat <- gatherData(grade = GRADE, newnames = newnamesLAG, dataformula = mobSF, varnames = varnamesMobs, 
# #                     preProcess = TRUE, MODE = "Dev", PRELIM = TRUE, DV = "mobility_sch")
# #   ModelFits <- ModelFitsMobS
# # }
# # 
# # 
# # 
# # # Successes
# # modSuc <- unique(ModelFits$method[!is.na(ModelFits$RMSE)])
# # 
# # print(modSuc)
# # 
# # # Fails
# # modFail <-unique(ModelFits$method[is.na(ModelFits$RMSE)])
# # 
# # print(modFail)
# # 
# # # Top 5
# # teststats <- ModelFits[ModelFits$grp == "test",]
# # hiRMSE <- unique(teststats$RMSE)[order(-unique(teststats$RMSE))][1:NMODELS]
# # loRMSE <- unique(teststats$RMSE)[order(unique(teststats$RMSE))][1:NMODELS]
# # #Some stats on these
# # bestMethod <- unique(ModelFits$method[ModelFits$RMSE %in% loRMSE])
# # 
# # # Calculate speed vs. auc
# # modelMethods <- ModelFits[ModelFits$grp == "test", 
# #                           c(1,2,5,6,7)][!duplicated(ModelFits[ModelFits$grp == "test", 
# #                                                                  c(1,2,5,6,7)]),]
# # 
# # modelMethods$efficiency <- round(10/(modelMethods$elapsedTime + modelMethods$RMSE^2), digits = 2)
# # 
# # # Get top N model methods
# # bestMethod <-modelMethods$method[order(-modelMethods$efficiency)][1:NMODELS]
# # 
# # ############################# 
# # # Re-test top contenders
# # #
# # #############################
# # 
# # fitControl2 <- trainControl(method='cv', number=5, 
# #                             savePredictions = TRUE)
# # 
# # ModelFits2 <- modSearch(methods = bestMethod, datatype=c("train", "test"),
# #                         traindata = dat$traindata,
# #                         testdata = dat$validdata, modelKeep = FALSE, 
# #                         length = LENGTH, fitControl = fitControl2, 
# #                         metric = "RMSE", cores = CORES)
# # 
# # cl <- makeCluster(CORES) 
# # registerDoParallel(cl)
# # library(caretEnsemble)
# # 
# # out1 <- buildModels(methodList = bestMethod, control = fitControl2, 
# #                     x = omitLinearCombos(dat$testdata$preds), 
# #                     y = dat$testdata$class,  
# #                     tuneLength = LENGTH, metric = "RMSE")
# # 
# # lapply(out1, class)
# # stopCluster(cl)
# # 
# # out1.ens <- caretEnsemble(out1)
# # 
# # #predMatrix <- makePredObsMatrix(out)
# # summary(out1.ens)
# # preds1 <- predict(out1.ens); rm(out1)
# # 
# # # Inspect accuracy
# # 
# # fort <- cbind(dat$testdata$class, preds1)
# # names(fort) <- c("y", "yhat", "yhat.se")
# # 
# # quantile(fort$y-fort$yhat, 0.9)[[1]] / (range(fort$y)[2] - range(fort$y)[1])
# # quantile(fort$y-fort$yhat, 0.1)[[1]] / (range(fort$y)[2] - range(fort$y)[1])
# # # 
# # # 
# # # qplot(y, yhat, data = fort, alpha = I(0.2))
# # # qplot(y, y-yhat, data=fort, alpha = I(0.2))
# # 
# # 
# # 
# # rm(out1, out2, out2.ens)
# # rm(myroc, prop, mycoord, tmp, bigclass, bigpreds, ModelFits2)
# # rm(badAUC, bestMethod, modFail, modSuc, myF, newnames, topAUC, varnames, buildNames, gatherData, CORES, LENGTH, 
# #    NMODELS, teststats, ModelFits, modelMethods, cl)
# # 
# # 
# # 
# # eval(parse(text = paste0("save(file = 'cache/imputation/models/GRADE_", GRADE, "EnsembleModels.rda', 
# #                          compress = 'bzip2')")))
