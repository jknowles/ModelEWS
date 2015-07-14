# ################################################################################
# # Title: Impute Data
# # Date: 7/31/2013
# # DESCRIPTION: Script to build the preliminary file by imputing ISES values in 
# #              order to provide predictions at the beginning of the school year. 
# # Parameterize this for the case where data is preliminary or final
# # Allow the year to change
# ################################################################################
# source("R/prediction_data_cleaning.R")
# 
# prelim <- TRUE
# 
# if(prelim == TRUE){
#   years <- c(2013) # 2013 to be added later
#   for(y in years){
# for(g in 6:8){
#   source("R/prediction_data_cleaning.R")
#   eval(parse(text=paste0("load('cache/test/testsetG", g,"Y", y,".rda')")))
#   eval(parse(text=paste0("load('cache/models/g", g,"imputemodelsAR.rda')")))
#   eval(parse(text=paste0("load('cache/models/g", g,"imputemodelsDD.rda')")))
#   eval(parse(text=paste0("load('cache/models/g", g,"imputemodelsPA.rda')")))
#   eval(parse(text=paste0("load('cache/models/g", g,"model.rda')")))
#   eval(parse(text=paste0("predictions$disab_code <- recode_extra_factor_level(
#                          predictions$disab_code, 'disab_code', 
#                          level=attrate_g", g,"$xlevels$disab_code,
#                          attrate_g", g,")")))
#   eval(parse(text=paste0("predictions$schg", g-1,"<- recode_extra_factor_level(
#                          predictions$schg",g-1,", 'schg", g-1,"', 
#                          level=attrate_g", g,"$xlevels$schg", g-1,",
#                          attrate_g", g,")")))
#   eval(parse(text=paste0("predictions$schg", g, " <- recode_extra_factor_level(
#                          predictions$schg",g,", 'schg", g,"', 
#                          level=final_model_g", g,"$xlevels$schg", g,",
#                          final_model_g", g,")")))
#   eval(parse(text=paste0("att_yhats <- predict(attrate_g", g, 
#                          ",newdata=predictions, type='response')")))
#   eval(parse(text=paste0("possatt_yhats <- predict(poss_att_days_g", g, 
#                          ",newdata=predictions, type='response')")))
#   eval(parse(text=paste0("dis_yhats <- predict(disdays_g", g, 
#                          ",newdata=predictions, type='response')")))
#   eval(parse(text=paste0("predictions$att_rate_g", g, ".ACT <- 
#                          predictions$att_rate_g", g, "; 
#                          predictions$att_rate_g", g, "<- att_yhats")))
#   eval(parse(text=paste0("predictions$poss_att_daysg", g, ".ACT <- 
#                          predictions$poss_att_daysg", g, "; 
#                          predictions$poss_att_daysg", g, "<- possatt_yhats")))
#   eval(parse(text=paste0("predictions$disdays_g", g, ".ACT <- 
#                          predictions$disdays_g", g, "; 
#                          predictions$disdays_g", g, "<- dis_yhats")))
#   eval(parse(text=paste0("predictions$disflag_g", g," <- discat(", g,")")))
#   # temporary hack, use prior mobility
#   eval(parse(text=paste0("predictions$mobility_sch_yr", g,"<- 
#                          predictions$mobility_sch_yr", g-1)))
#   # drop missing data
#   eval(parse(text=paste0("predictions <- 
#                          predictions[!is.na(predictions$att_rate_g",g,") ,]")))
#   eval(parse(text=paste0("yhats <- predict(final_model_g", g, ", 
#                          newdata=predictions, type='response', se.fit=TRUE)")))
#   predictions$yhat <- yhats$fit
#   predictions$yhatError <- yhats$se.fit
#   rm(yhats, dis_yhats, possatt_yhats, att_yhats); gc()
#   eval(parse(text=paste0("rm(attrate_g", g, ", poss_att_days_g", g, ", disdays_g", g, 
#                          ", final_model_g", g,")")))
#   gc()
#   eval(parse(text=paste0("save(predictions, 
#                          file='cache/predictions/predictionsG", g, "Y",y, y-1999,"p.rda', 
#                          compress='gzip')")))
#   rm(predictions); gc()
# }
# }
# } else if(prelim !=TRUE){
#   source("R/prediction_data_cleaning.R")
#   grades <- c(6, 7, 8)
#   years <- c(2011) # 2012 to be added later
#   for(y in years){
#   for(g in grades){
#     eval(parse(text=paste0("load('cache/test/testsetG", g, "Y", y,".rda')")))
#     eval(parse(text=paste0("load('cache/models/g", g, "model.rda')")))
#     eval(parse(text=paste0("final_model <- final_model_g", g)))
#     predictions$disab_flag <- impute_missing_factor_level(predictions$disab_flag, 
#                                                           "disab_flag", 
#                                                           level = final_model$xlevels$disab_flag,  
#                                                           final_model)
#     eval(parse(text=paste0("predictions$schg",g,"<- impute_missing_factor_level(predictions$schg",g,", 
#                                                    'schg",g,"',
#                                                    level = final_model$xlevels$schg",g,",
#                                                    final_model)")))
#     test <- predict(final_model, newdata = predictions, 
#                     se = TRUE, type="response")
#     
#     predictions$yhat <- test$fit
#     predictions$yhatError <- test$se.fit
#     rm(test)
#     eval(parse(text=paste0("save(predictions, 
#                          file='cache/predictions/predictionsG",g,"Y", y, y-1999,".rda', 
#                          compress='gzip')")))
#     
#   }
#   }
# }
