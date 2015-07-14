# 
# ############################################################
# ## Set up environment
# ############################################################
# 
# GRADE <- 6
# 
# library(caretEnsemble)
# library(EWStools)
# library(ggplot2)
# library(eeptools)
# ############################################################
# ## LOAD MODELS
# ############################################################
# 
# eval(parse(text = paste0("load('cache/models/GRADE_", GRADE, "EnsembleModels.rda')")))
# 
# 
# 
# #Var imp
# predimport <- varImp(out1.ens)
# predimport$var <- row.names(predimport)
# qplot(reorder(var, wght), wght, data = predimport[predimport$wght > 5, ], 
#       geom = 'bar', stat='identity') + theme_dpi() + coord_flip()
# 
# 
# qplot(reorder(var, wght), wght, data = predimport, 
#       geom = 'bar', stat='identity') + theme_dpi() + coord_flip() + 
#   labs(x = "Variable Weight", y = "Variable", 
#        title = "Predictor Importance for Grade 6 EWS Model")
# 
# 
# ## Validate models
# 
# mypredsa <- predict(out1.ens$models[[5]], newdata = full$validdata$preds, type = "prob")
# confusionMatrix(mypredsa, reference = full$validdata$class)
# 
# mypreds <- predict(out1.ens, keepNA=TRUE, newdata = full$validdata$preds)
# 
# validROC <- roc(full$validdata$class ~ mypreds)
# thresh <- coords.roc(validROC, x = "best", best.method = "closest.topleft", 
#                      best.weights = c(1, .13))
# 
# 
# predClass <- ifelse(mypreds >= thresh[[1]], "Non.Grad", "Grad")
# table(predClass)
# 
# caret::confusionMatrix(predClass, reference = full$validdata$class)
# 
# var.imp <- varImp(bestmod)
# 
# qplot(row.names(var.imp$importance),var.imp$importance[,1], geom='bar', stat='identity') +coord_flip()
# 
# ## 
