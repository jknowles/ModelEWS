#------------------------------------------------------------------------------
# WSN Lookup and Post-prediction Processing for export
# Jared E. Knowles
# 07-01-2013
# Edited in 2014
# Edited in 2015
#------------------------------------------------------------------------------

#' Uncenter
#' @description A function to uncenter binary variables that have been centered 
#' using the arm package
#' @param x a vector with two distinct values that was previously a binary variable 
#'
#' @return a vector, length x, with two distinct values, 0 and 1
#' @export
#' @examples
#' x <- rbinom(100, 1, prob = .3)
#' scaleX <- x - mean(x)
#' identical(x, as.integer(uncenter(scaleX)))
uncenter <- function(x){
  if(length(unique(x)) != 2){
    warning("Use only on variables with two distinct values")
  }
  vals <- unique(x)
  constant <- abs(vals[vals <= 0])
  x <- x + constant
  return(x)
}

#' Reverse pre-processing done by caret to a dataset
#'
#' @param preProc a preProcess object from the caret package
#' @param data a data.frame with the same elements as the preProcess object
#'
#' @return a data.frame
#' @details This only reverses scaling and centering done by preProcess currently. 
#' It cannot undo PCA transformations, it cannot undo imputation, exponential transformations, 
#' or any other method
#' @export
unPreProc <- function(preProc, data){
  stopifnot(class(preProc) == "preProcess")
  stopifnot(class(data) == "data.frame")
  for(i in names(preProc$mean)){
    tmp <- data[, i] * preProc$std[[i]] + preProc$mean[[i]]
    data[, i] <- tmp
  }
  return(data)  
}

#' Postprocessing of DEWS predictions
#'
#' @param predCohort a data.frame produced by the prediction function
#' @param preProcess a preProcess object used to preProcess predictions
#'
#' @return a data.frame with variables rescaled to be on their original 
#' interpretable scales
#' @export
postProcess.pred <- function(predCohort, preProcess){
  stopifnot(class(predCohort) == "data.frame")
  stopifnot(class(preProcess) == "preProcess")
  binVars <- findBinary(predCohort)
  # drop bestThresh
  binVars <- binVars[!binVars %in% "bestThresh"]
  allVars <- names(predCohort)
  conVars <- names(preProcess$mean)
  predCohort[, conVars] <- unPreProc(preProcess, predCohort[, conVars])
  predCohort[, binVars] <- apply(predCohort[, binVars], 2, uncenter)
  return(predCohort)
}

# load("cache/predictions/GRADE_5_EnsembleModels_APRIL_2013-2014.rda")

#' A function to export the results of model predictions to a specific format
#'
#' @param stub path to filename where the predictions in raw form are stored
#' @param grade a numeric for the grade of the prediction filename
#' @param year a character representing the year in the filename of predictions
#' @param conn a connection object to reach the database
#' @param prelim a logical indicating if this is the final or preliminary DEWS 
#' run for the year
#' @param cache a logical indicating if results should be stored to disk
#' @return a data.frame with key data elements, rescaled to be interpretable, 
#' as well as the final predictions
#' @export
exportPreds <- function(stub = "EnsembleModels_APRIL", grade = 5, year = '2013-2014', 
                        conn, prelim = FALSE, cache = TRUE){
  fileName <- paste0("cache/predictions/GRADE_", grade, "_", stub, "_" , year,
                     ".rda")
  load(fileName)
  out <- postProcess.pred(predCohort = out, preProcess = varScales)
  keyTable <- pullWSN(conn)
  out <- merge(out, keyTable[, 2:3], by = "STUDENT_KEY")
  
  rm(keyTable)
  FLAGTHRESH <- out$bestThresh[1]
  out$yhat <- out$pred
  
  if(median(out$se) > 0.15){
    out$yhatError <- out$se^2
  } else{
    out$yhatError <- out$se
  }
  # Overall predictions
  out$flag_low <- out$yhat - out$yhatError
  out$flag_high <- out$yhat + out$yhatError
  out$yhat <- ifelse(out$yhat > 1, 1, out$yhat)
  out$flag_high <- ifelse(out$flag_high > 1, 1, out$flag_high)
  out$flag_low <- ifelse(out$flag_low < 0, 0, out$flag_low)
  
  out$flag <- 'false'
  out$flag <- ifelse(out$yhat & out$flag_low >= FLAGTHRESH,
                      "High", "None")
  out$flag <- ifelse(out$flag_high >= FLAGTHRESH & out$flag_low <= FLAGTHRESH,
                      "Moderate", out$flag)
  out$flag <- ifelse(out$flag_high <= FLAGTHRESH,
                      "Low", out$flag)
  out$flag[is.na(out$yhat)] <- "Missing"
  
  out$assess <- out$MATH_SS + out$READ_SS
  
  cis <- tapply(out$assess, list(out$flag), function(x){(ci(x, 0.75))}, simplify=TRUE)
  
  thresh <- expand.grid(group = c("High", "Moderate", "Low"), 
                        min = rep(NA), med = rep(NA),
                        max = rep(NA) )
  
  thresh[thresh$group=="High", c(2:4)] <- cis$High
  thresh[thresh$group=="Moderate", c(2:4)] <- cis$Moderate
  thresh[thresh$group=="Low", c(2:4)] <- cis$Low
  
  rankM <- ecdf(out$MATH_SS)
  rankR <- ecdf(out$READ_SS)
  rankA <- ecdf(out$ATT_RATE)
  out$mathrank <- trunc(rankM(out$MATH_SS)*100)
  out$readrank <- trunc(rankR(out$READ_SS)*100)
  out$attrank <- trunc(rankA(out$ATT_RATE)*100)
  
  out$daysmissed <- 180 - round(180*out$ATT_RATE, 1)
  out$daysmissed[out$daysmissed < 0] <- 0
  # Hack, need to round to nearest 0.5
  out$daysmissed <- round(out$daysmissed, 1)
  
  out$assessments <- NA
  out$assessments[out$assess < thresh[thresh$group=="Moderate", "min"]] <- "High"
  out$assessments[out$assess >= thresh[thresh$group=="Moderate", "min"] & 
                     out$assess <= thresh[thresh$group=="Moderate", "max"]] <- 
    "Moderate"
  out$assessments[out$assess > thresh[thresh$group=="Moderate", "max"]] <- "Low"
  out$assessments[is.na(out$READ_SS) | is.na(out$MATH_SS)] <- "Missing"
  out$attendance <- NA
  out$attendance[out$daysmissed <= 12] <- "Low"
  out$attendance[out$daysmissed > 12 & out$daysmissed <= 20] <- "Moderate"
  out$attendance[out$daysmissed > 20] <- "High"
  out$attendance[is.na(out$daysmissed)] <- "Missing"
  
  if("mobility_flag" %in% names(out)){
    out$mobility <- "Low"
    out$mobility[out$mobility_flag == "Yes"] <- "Moderate"
    out$mobility_flag <- NULL
  } else{
    out$mobility <- "Low"
    out$mobility[out$SCHOOL_COUNT < 2 & out$ENROLLMENTS_COUNT < 2] <- "Low"
    out$mobility[out$SCHOOL_COUNT >= 2 & out$ENROLLMENTS_COUNT < 2] <- "Low"
    out$mobility[out$SCHOOL_COUNT >= 2 & out$ENROLLMENTS_COUNT >= 2] <- "Moderate"
    out$mobility[out$SCHOOL_COUNT >= 3 & out$ENROLLMENTS_COUNT >= 3] <- "High"
  }
  
  
  out$discipline <- "Low"
  out$discipline[out$incCount > min(out$incCount) | 
                    out$daysRemoved > min(out$daysRemoved)] <- "Moderate"
  out$discipline[out$incCount > as.numeric(names(table(out$incCount)[2]))] <- "High"
  #   
  vars <- c("WSN", "STUDENT_KEY", "yhat", "yhatError", "flag", 
            "mathrank", "readrank", "attrank", "assessments", "daysmissed", 
            "ATT_RATE", "MATH_SS", "READ_SS",
            "attendance", "mobility", "discipline", 
            "daysRemoved", "SCHOOL_COUNT", 
            "ENROLLMENTS_COUNT","incCount", "DAYS_POSS")
  
  export <- out[, vars]
  #export <- export[, vars]
  export$DATE <- Sys.time()
  export$MODEL <- "Full"
  export$MODEL_DATA <- ifelse(prelim==TRUE, "Preliminary", "Final")
  MODEL_NAME <- ifelse(prelim == TRUE, paste0("Grade ", grade, " Preliminary"), 
                       paste0("Grade ", grade, " Final"))
  export$MODEL_NAME <- MODEL_NAME
  export$STUDENT_RISK_SEVERITY_SCORE <- export$yhat; export$yhat <- NULL
  export$STUDENT_RISK_SEVERITY_PRECISION <- export$yhatError; export$yhatError <- NULL
  export$STUDENT_RISK_OUTCOME <- export$flag; export$flag <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Att <- export$daysmissed; export$daysmissed <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Att2 <- export$attrank; export$attrank <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Att3 <- export$ATT_RATE * 100; export$ATT_RATE <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Att4 <- export$DAYS_POSS; export$DAYS_POSS <- NULL
  export$STUDENT_RISK_REPORT_TEXTAtt <- export$attendance; export$attendance <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Assess1 <- export$READ_SS; export$READ_SS <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Assess2 <- export$MATH_SS; export$MATH_SS <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Assess3 <- export$mathrank; export$mathrank <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Assess4 <- export$readrank; export$readrank <- NULL
  export$STUDENT_RISK_REPORT_TEXTAssess <- export$assessments; export$assessments <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Disc <- exp(export$daysRemoved) - .01; export$daysRemoved <- NULL
  if(prelim==TRUE){
    export$STUDENT_RISK_MEASURE_VALUE_Disc2 <- NA; export$incCount <- NULL
    export$STUDENT_RISK_MEASURE_VALUE_Disc3 <- NA
  } else if(prelim!=TRUE){
    export$STUDENT_RISK_MEASURE_VALUE_Disc2 <- exp(export$incCount) - .1; export$incCount <- NULL
    export$STUDENT_RISK_MEASURE_VALUE_Disc3 <- NA
  }
  export$STUDENT_RISK_REPORT_TEXTDisc <- export$discipline; export$discipline <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Mob <- export$ENROLLMENTS_COUNT; export$ENROLLMENTS_COUNT <- NULL
  export$STUDENT_RISK_MEASURE_VALUE_Mob2 <- export$SCHOOL_COUNT; export$SCHOOL_COUNT <- NULL
  export$STUDENT_RISK_REPORT_TEXTMob <- export$mobility; export$mobility <- NULL
  export$GRADE_LEVEL <- grade
  export$SCHOOL_YEAR <- year
  if(cache == TRUE){
    eval(parse(text=paste0("write.csv(export, na='', quote=FALSE, 
                       file='export/", year ,
                           "/EdVantage DEWS Export Grade ", grade,
                           " ",Sys.Date(),".csv')")))
  } else {
    return(export)
  }
}

validateExports <- function(year){
  etlData <- read.csv("export/2013-2014/EdVantage DEWS Export Grade 7 2015-05-31.csv")
}