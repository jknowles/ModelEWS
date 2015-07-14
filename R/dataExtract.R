#------------------------------------------------------------------------------
# Examples of data extraction functions
# These functions are entirely specific to the DPI data system and to the specific 
# problem of dropout early warning systems. These functions should be completely 
# rewritten or heavily modified in other implementation and are shown here simply 
# as an example of how to set up the data acquisition code
#-------------------------------------------------------------------------------


#' Pull down assessment data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student assessment data and merge keys
#' @details This function is meant to pull a single year and single grade of 
#' assessment data for students and to do the necessary data cleaning and reshaping 
#' to create data with a single row per student per year per grade and with columns 
#' representing assessment results on mathematics and reading. This function is 
#' called as part of a larger function which assembles all data on a cohort for 
#' processing in a model search.
#' @export
pullWSAS <- function(cohortYear, grade, conn){  
  cohortYear2 <- paste(substr(cohortYear, 1, 4), substr(cohortYear, 8, 9), sep = "-")
  readQuery <- paste0("SELECT STUDENT_KEY, SCHOOL_YEAR, GRADE,
                TEST_SCALED_SCORE, TEST_RAW_SCORE, WSAS_FAY_DISTRICT, 
                WSAS_FAY_SCHOOL, SCHOOL_KEY, SCHOOL_DISTRICT_CODE
                FROM 
               K12INTEL_REPORTING.MTBL_WSAS WHERE SCHOOL_YEAR  = '", 
                cohortYear2,"' AND GRADE = '", grade,"' AND 
                TEST_SUBJECT = 'Reading'")
  readWSAS <- sqlQuery(conn, readQuery)
  readWSAS$SCHOOL_YEAR <- cohortYear
  
  names(readWSAS) <- c("STUDENT_KEY", "SCHOOL_YEAR", "GRADE", "READ_SS", 
                       "READ_RAW", "READ_FAY_D", "READ_FAY_S", "SCHOOL_KEY", 
                       "SCHOOL_DISTRICT_CODE")
  mathQuery <- paste0("SELECT STUDENT_KEY, TEST_SCALED_SCORE, TEST_RAW_SCORE, 
                WSAS_FAY_DISTRICT, WSAS_FAY_SCHOOL
                FROM 
               K12INTEL_REPORTING.MTBL_WSAS WHERE SCHOOL_YEAR  = '", 
                      cohortYear2,"' AND GRADE = '", grade,"' AND 
                TEST_SUBJECT = 'Mathematics'")
  mathWSAS <- sqlQuery(conn, mathQuery)
  names(mathWSAS) <- c("STUDENT_KEY", "MATH_SS", "MATH_RAW", "MATH_FAY_D", 
                       "MATH_FAY_S")
  out <- merge(readWSAS, mathWSAS)
  out$SCHOOL_YEAR <- NULL
  out$GRADE <- NULL
  out$SCHOOL_KEY_WSAS <- out$SCHOOL_KEY; out$SCHOOL_KEY <- NULL
  out$DISTRICT_CODE_WSAS <- out$SCHOOL_DISTRICT_CODE; out$SCHOOL_DISTRICT_CODE <- NULL
  return(out)
}

#' Pull down demographic data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student demographic data and merge keys
#' @details This function is meant to pull a single year and single grade of 
#' demographic data for students and to do the necessary data cleaning and reshaping 
#' to create data with a single row per student per year per grade and with columns 
#' representing demographic data for model building. This function is 
#' called as part of a larger function which assembles all data on a cohort for 
#' processing in a model search. Specifically in this case it includes race, 
#' school, student disability status, student economic disadvantage status, 
#' student ELL status, student retention indicator, student attendance, 
#' student gender, and school district
#' @export
pullDemog <- function(cohortYear, grade, conn){
  demogQuery <- paste0("SELECT A.STUDENT_ANNUAL_ATTRIBS_KEY, 
                A.STUDENT_KEY, A.SCHOOL_KEY, A.STUDENT_RACE, 
                A.STUDENT_FOODSERVICE_ELIG_CODE, A.STUDENT_SPECIAL_ED_INDICATOR, 
                A.STUDENT_ESL_CLASSIFICATION, A.STUDENT_LEP_INDICATOR, 
                A.STUDENT_REPEATER_INDICATOR, 
                A.STUDENT_ANNUAL_ENROLLMENT_DAYS, A.STUDENT_ANNUAL_ABSENCES, 
                A.STUDENT_ANNUAL_GRADE_CODE, B.STUDENT_GENDER_CODE, 
                A.DISTRICT_CODE, A.SCHOOL_YEAR FROM 
               K12INTEL_DW.DTBL_STUDENT_ANNUAL_ATTRIBS A, 
                K12INTEL_DW.DTBL_STUDENT_ATTRIBS B
          WHERE A.STUDENT_ATTRIB_KEY = B.STUDENT_ATTRIB_KEY AND 
                         A.SCHOOL_YEAR  = '", 
                       cohortYear,"' AND A.STUDENT_ANNUAL_GRADE_CODE = '", grade, 
                       "'")
  studentDemog <- sqlQuery(conn, demogQuery)
  return(studentDemog)
}


#' Calculate expected graduation year
#'
#' @param baseyear a character representing the school year in a given grade for 
#' a given cohort of students in the form 'YYYY-YY' or 'YYYY-YYYY'
#' @param grade a character or numeric representing the grade level
#'
#' @return a school year in the format 'YYYY-YY' representing the expected graduation 
#' year for the cohort
#' @details This expected graduation is purely based on the number of school years 
#' necessary under normal progression to reach grade 12 and the school year that 
#' grade 12 will coincide with. 
#' @export
expectedGrad <- function(baseyear, grade){
  baseNum <- substr(baseyear, 1, 4); baseNum <- as.numeric(baseNum)
  gradeNum <- as.numeric(grade)
  gradNum <- baseNum + (12 - gradeNum)
  gradYear <- paste(gradNum, gradNum+1, sep = "-")
  return(gradYear)
}

#' Pull down graduation data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student graduation data and merge keys
#' @details This function returns the graduation outcome for a cohort of students 
#' in given grade and year. The graduation outcomes are recoded based on DPI 
#' specific exit codes. There is also logic in this function to eliminate 
#' duplicated records and to parse early graduation.
#' @export
pullGrad <- function(cohortYear, grade, conn){
  gradYear <- expectedGrad(baseyear = cohortYear, grade = grade)
  # Find all cohort members with a 4 year regular diploma
  gradQuery <- paste0("SELECT A.STUDENT_DIPLOMA_TYPE, 
          A.STUDENT_KEY, A.SCHOOL_YEAR FROM 
          K12INTEL_DW.DTBL_STUDENT_ANNUAL_ATTRIBS A 
          WHERE STUDENT_DIPLOMA_TYPE = 'Regular Diploma' AND 
          A.STUDENT_KEY in (SELECT STUDENT_KEY FROM 
          K12INTEL_DW.DTBL_STUDENT_ANNUAL_ATTRIBS WHERE SCHOOL_YEAR = '", 
                      cohortYear,"' AND STUDENT_ANNUAL_GRADE_CODE = '", grade, "')")
  
  studentGrad <- sqlQuery(conn, gradQuery)
  studentGrad$yearNum <- as.numeric(substr(studentGrad$SCHOOL_YEAR, 1, 4))
  gradYear <- as.numeric(substr(gradYear, 1, 4))
  # Code if they received their diploma on or before the rest of their cohort
  studentGrad$grad_ind <- ifelse(studentGrad$yearNum > gradYear, "Late", "on-time")
  # Find duplicates, students with multiple graduation records
  dupeKeys <- studentGrad$STUDENT_KEY[duplicated(studentGrad$STUDENT_KEY)]
  dupeKeys <- unique(dupeKeys)
  dupes <- studentGrad[studentGrad$STUDENT_KEY %in% dupeKeys, ]
  studentGrad <- studentGrad[!studentGrad$STUDENT_KEY %in% dupeKeys, ]
  # Resolve duplicates
  dupes <- dupes %>%
    group_by(STUDENT_KEY) %>%
    summarize(STUDENT_DIPLOMA_TYPE = "Regular Diploma", 
              SCHOOL_YEAR = eeptools::statamode(SCHOOL_YEAR), 
              yearNum = max(yearNum),
              grad_ind = ifelse("on-time" %in% grad_ind, "on-time", "Late"))
  dupes$duplicated <- "Yes"
  studentGrad$duplicated <- "No"
  dupes <- dupes[, names(studentGrad)]
  studentGrad <- rbind(studentGrad, dupes)
  rm(dupes, dupeKeys)
  names(studentGrad)[3] <- "GradYear"
  knownGrads <- studentGrad[, c("STUDENT_KEY", "grad_ind", "GradYear", "duplicated")]
  #   return(studentGrad)
  # Now let's find out what happened to all non-diploma students
  # Need to identify if they should be retained in sample, or excluded because their 
  # outcome is unknown
  gradQuery2 <- paste0("SELECT A.STUDENT_KEY, A.WITHDRAW_REASON, 
        A.WITHDRAW_REASON_CODE, A.WITHDRAW_DATE,Year(WITHDRAW_DATE) as Year,
        beginenroll.DATE_VALUE as BEGIN_ENROLLDATE, 
        endenroll.DATE_VALUE as END_ENROLLDATE 
        FROM K12INTEL_DW.FTBL_ENROLLMENTS A 
            inner join k12intel_dw.DTBL_CALENDAR_DATES beginenroll on 
              a.CAL_DATE_KEY_BEGIN_ENROLL = beginenroll.CALENDAR_DATE_KEY 
            inner join k12intel_dw.DTBL_CALENDAR_DATES endenroll on 
              a.CAL_DATE_KEY_END_ENROLL = endenroll.CALENDAR_DATE_KEY where 
      A.STUDENT_KEY in (SELECT STUDENT_KEY FROM 
              K12INTEL_DW.DTBL_STUDENT_ANNUAL_ATTRIBS WHERE 
      SCHOOL_YEAR = '", cohortYear,"' AND STUDENT_ANNUAL_GRADE_CODE = '", grade, 
                       "')")

  studentGrad <- sqlQuery(conn, gradQuery2)
  
  # let's cleanup now
  studentGrad <- merge(studentGrad, knownGrads, by = "STUDENT_KEY", all.x=TRUE)
  studentGrad$grad_ind[is.na(studentGrad$grad_ind)] <- "Non-grad"
  
  rm(knownGrads)
  
  maxDates <- studentGrad %>%
    group_by(STUDENT_KEY) %>%
    summarize(lastEnd = max(END_ENROLLDATE))
  
  maxDates$maxObsFlag <- "Yes"
  
  studentGrad <- merge(studentGrad, maxDates, 
                       by.x=c("STUDENT_KEY", "END_ENROLLDATE"), 
                       by.y=c("STUDENT_KEY", "lastEnd"), all.x=TRUE)
  
  studentGrad$maxObsFlag[is.na(studentGrad$maxObsFlag)] <- "No"
  rm(maxDates)
  grads <- subset(studentGrad, grad_ind!="Non-grad" & maxObsFlag == "Yes")
  nongrads <- subset(studentGrad, grad_ind == "Non-grad")
  nongrads_tmp <- nongrads[nongrads$maxObsFlag == "Yes" & nongrads$Year > gradYear,]
  
  nongrads <- nongrads[!nongrads$STUDENT_KEY %in% c(unique(nongrads_tmp$STUDENT_KEY), 
                                                    unique(grads$STUDENT_KEY)),]
  
  nongrads$grad_ind <- "Non-grad"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "DE"] <- "exitWIpublicK12"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "TNC"] <- "exitWIpublicK12"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "TOS"] <- "exitWIpublicK12"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "OOS"] <- "exitWIpublicK12"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "HSC"] <- "Late"
  nongrads$grad_ind[nongrads$maxObsFlag == "Yes" & 
                   nongrads$WITHDRAW_REASON_CODE == "PCC"] <- "Late"
  nongrads <- nongrads[nongrads$maxObsFlag == "Yes",]
  rm(studentGrad)
  studentGrad <- rbind(grads, nongrads_tmp)
  studentGrad <- rbind(studentGrad, nongrads)
  rm(nongrads, grads, nongrads_tmp)
  studentGrad <- studentGrad[!is.na(studentGrad$STUDENT_KEY),]
  studentGrad <- studentGrad[, c("STUDENT_KEY", "grad_ind", "duplicated")]
  return(studentGrad)
}


#' Pull down discipline data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student discipline data and merge keys
#' @details This function returns the discipline records for a cohort of students 
#' in given grade and year. Students with no discipline record are not returned.
#' @export
pullDiscipline <- function(cohortYear, grade, conn){
#   cols <- colnames(sqlFetch(ds[[4]], "K12INTEL_STAGING_DPIODS.DISC_REMOVAL_FACT", max=1))
  cohortYear2 <- paste(substr(cohortYear, 1, 4), substr(cohortYear, 8, 9), sep = "-")
  removQuery <-  paste0("SELECT A.LDS_STUDENT_KEY,
              A.PRIMARY_INCIDENT_TYPE, B.DAYS_REMOVED, 
              B.REMOVAL_TYPE FROM 
              K12INTEL_STAGING_DPIODS.DISC_INCIDENT_FACT A, 
              K12INTEL_STAGING_DPIODS.DISC_REMOVAL_FACT B 
              WHERE A.DISC_INCIDENT_FACT_KEY = B.DISC_INCIDENT_FACT_KEY 
              AND A.SCHOOL_YEAR = '", cohortYear2, "' AND A.GRADE_LEVEL_DISC = '", 
               grade, "' AND NOT (B.REMOVAL_TYPE = 'IS')")
  stuDisc <- sqlQuery(conn, removQuery)
  stuDisc$SCHOOL_YEAR <- cohortYear
  stuDisc <- stuDisc[stuDisc$REMOVAL_TYPE != "SP",]
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% as.character()
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "SR", "SchoolRule", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "OF", "Weapon", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "HG", "Weapon", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "SG", "Weapon", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "DW", "Weapon", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "DG", "Drug", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "AL", "Drug", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "AS", "Assault", .)
  stuDisc$PRIMARY_INCIDENT_TYPE %<>% ifelse(. == "AE", "Assault", .)
  stuDisc <- stuDisc %>%
    group_by(LDS_STUDENT_KEY, SCHOOL_YEAR) %>% 
    summarise(incCount = length(LDS_STUDENT_KEY),
              incType = paste(unique(PRIMARY_INCIDENT_TYPE), collapse = "-"), 
              daysRemoved = sum(DAYS_REMOVED),
              removalType = ifelse("EX" %in% REMOVAL_TYPE, "EX", "OS")
              )
  
  crossRefQuery <- paste0("SELECT b.LDS_STUDENT_KEY,case when 
  c.STUDENT_STATUS = 'Deleted' then -2 else a.STUDENT_KEY end STUDENT_KEY,a.WSN
  FROM K12INTEL_KEYMAP.KM_STUDENTS_DPI a WITH(NOLOCK)
  inner join K12INTEL_STAGING_DPIODS.STUDENT_KEY_WSN_XREF b WITH(NOLOCK)
  on a.WSN = b.WSN
  inner join K12INTEL_DW.DTBL_STUDENTS c WITH(NOLOCK)
  on a.STUDENT_KEY = c.STUDENT_KEY
  inner join K12INTEL_STAGING_DPIODS.DISC_INCIDENT_FACT inc
  on inc.LDS_STUDENT_KEY = b.LDS_STUDENT_KEY
  WHERE b.CURRENT_WSN_IND = 'Y'") 
  keyTable <- sqlQuery(conn, crossRefQuery)
  keyTable <- keyTable[!duplicated(keyTable),]
  stuDisc <- merge(stuDisc, keyTable, by = "LDS_STUDENT_KEY")
  stuDisc$LDS_STUDENT_KEY <- NULL
  stuDisc$WSN <- NULL
  stuDisc$SCHOOL_YEAR <- NULL
  return(stuDisc)
}


#' Pull down mobility data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student mobility data and merge keys
#' @details This function returns the number of schools and number of districts 
#' attended by a student #' in given grade and year for all students. 
#' @export
pullMobility <- function(cohortYear, grade, conn){
  mobQuery <- paste0("select enrl.STUDENT_KEY,schdates.LOCAL_SCHOOL_YEAR,
  count(distinct enrl.district_code) as distinct_districts,
  count(distinct enrl.SCHOOL_KEY) as distinct_schools,
  count(*) as number_enrollments
  from 
  k12intel_dw.FTBL_ENROLLMENTS enrl 
  INNER JOIN k12intel_dw.dtbl_school_dates schdates WITH (NOLOCK) 
  ON (enrl.SCHOOL_DATES_KEY_BEGIN_ENROLL = schdates.school_dates_key)
  WHERE
  schdates.LOCAL_SCHOOL_YEAR = '", cohortYear, "' AND
  enrl.STUDENT_KEY in (SELECT STUDENT_KEY FROM 
          K12INTEL_DW.DTBL_STUDENT_ANNUAL_ATTRIBS WHERE SCHOOL_YEAR = '", 
                     cohortYear,"' AND STUDENT_ANNUAL_GRADE_CODE = '", grade, "') 
  group by enrl.STUDENT_KEY,schdates.LOCAL_SCHOOL_YEAR
  order by schdates.LOCAL_SCHOOL_YEAR")

  stuMob <- sqlQuery(conn, mobQuery)
  names(stuMob) <- c("STUDENT_KEY", "SCHOOL_YEAR", "DISTRICT_COUNT", "SCHOOL_COUNT", 
                     "ENROLLMENTS_COUNT")
  stuMob$SCHOOL_YEAR <- NULL
  return(stuMob)
}

#' Create a cohort set of students with full data
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return Returns a data.frame with student mobility data and merge keys
#' @details This function calls pullDemog, pullWSAS, pullDiscipline, pullMobility 
#' and combines the results based on the unique student key to create a single 
#' student-year-grade record which includes all of the information on a student
#' by row including assessment scores, attendance, demographics, mobility, and 
#' discipline results. Returns incomplete records for students missing data 
#' in any element, but uses the results of pullDemog as the full list of students.
#' @export
pullCohort <- function(cohortYear, grade, conn, type = "train"){
  if("train" %in% type){
    stuGrad <- pullGrad(cohortYear, grade, conn)
  } else {
    stuGrad <- data.frame("STUDENT_KEY" = NA, grad_ind = NA)
  }
  stuDem <- pullDemog(cohortYear, grade, conn)
  stuTest <- pullWSAS(cohortYear, grade, conn)
  stuDisc <- try(pullDiscipline(cohortYear, grade, conn))
  stuMob <- pullMobility(cohortYear, grade, conn)
  outDat <- merge(stuDem, stuGrad, by = c("STUDENT_KEY"), all.x=TRUE)
  outDat <- merge(outDat, stuTest, by = c("STUDENT_KEY"), all.x=TRUE)
  if(class(stuDisc) == "data.frame"){
    outDat <- merge(outDat, stuDisc, by = c("STUDENT_KEY"), all.x=TRUE)
    outDat$incType[is.na(outDat$incType)] <- "None"
    outDat$daysRemoved[is.na(outDat$daysRemoved)] <- 0
    outDat$removalType[is.na(outDat$removalType)] <- "None"
    outDat$incCount[is.na(outDat$incCount)] <- 0
  } else {
    warning("Discipline data not available for 2005-2006 students.")
    stuDisc <- data.frame(STUDENT_KEY= unique(outDat$STUDENT_KEY), incCount = NA, 
                          incType = NA, daysRemoved = NA, removalType = NA)
    outDat <- merge(outDat, stuDisc, by = c("STUDENT_KEY"), all.x=TRUE)
  }
  outDat <- merge(outDat, stuMob, by = c("STUDENT_KEY"), all.x=TRUE)
#   rm(stuDem, stuGrad, stuTest, stuDisc, stuMob)
  if(!"train" %in% type){
    outDat$grad_ind <- NULL
  }
  return(outDat)
}

#-------------------------------------------------------------------------------
# Collapse cohort attributes
# cohortDF = data.frame produced by getCohort()

#' Calculate school level peer attributes for a cohort dataset
#' @param cohortDF a dataframe produced by the pullCohort function
#' @return a data.frame with nrows = number of schools
#' @details This function calculates an array of peer level demographics for a 
#' cohort of students in a given grade-year. These include means and standard 
#' deviations of test scores, suspension and expulsion rates, attendance rates, 
#' and percent non-white, percent SwD, and percent economically disadvantaged
#' @export
peerStats <- function(cohortDF){
  peerStats <- cohortDF %>% 
    group_by(SCHOOL_KEY) %>%
    summarize(size = length(STUDENT_KEY), 
              mean_read = mean(READ_SS, na.rm=TRUE), 
              sd_read = sd(READ_SS, na.rm=TRUE), 
              mean_math = mean(MATH_SS, na.rm=TRUE), 
              sd_math = sd(MATH_SS, na.rm=TRUE), 
              suspPeers = length(incCount[!is.na(incCount) & incCount > 0]), 
              mean_attend = sum(STUDENT_ANNUAL_ABSENCES, na.rm=TRUE) / 
                sum(STUDENT_ANNUAL_ENROLLMENT_DAYS, na.rm=TRUE), 
              sd_attend = sd((STUDENT_ANNUAL_ENROLLMENT_DAYS - STUDENT_ANNUAL_ABSENCES) / 
                               STUDENT_ANNUAL_ENROLLMENT_DAYS, na.rm=TRUE), 
              nonWhite = length(STUDENT_RACE[STUDENT_RACE!="White"]), 
              SwDper = length(STUDENT_SPECIAL_ED_INDICATOR[STUDENT_SPECIAL_ED_INDICATOR == "Yes"]),
              FRLper = length(STUDENT_FOODSERVICE_ELIG_CODE[STUDENT_FOODSERVICE_ELIG_CODE != "N"])
    )
  peerStats$FRLper <- peerStats$FRLper / peerStats$size
  peerStats$SwDper <- peerStats$SwDper / peerStats$size
  peerStats$nonWhitePer <- peerStats$nonWhite / peerStats$size
  peerStats$nonWhite <- NULL
  peerStats$suspPeersPer <- peerStats$suspPeers / peerStats$size
  peerStats$mean_attend <- 1 - peerStats$mean_attend
  names(peerStats) <- paste("cohort",names(peerStats), sep ="_")
  return(peerStats)
}


#-------------------------------------------------------------------------------
# Pull WSN crossRef

#' Extract a student WSN for crosswalk and matching purposes
#' @param conn a connection object with the data connections necessary to access 
#' the database
#' @return a data.frame with all student LDS_KEYS and WSNs for merging purposes
pullWSN <- function(conn){
  crossRefQuery <- paste0("SELECT b.LDS_STUDENT_KEY,case when 
                        c.STUDENT_STATUS = 'Deleted' then -2 else a.STUDENT_KEY end STUDENT_KEY,a.WSN
                        FROM K12INTEL_KEYMAP.KM_STUDENTS_DPI a WITH(NOLOCK)
                        inner join K12INTEL_STAGING_DPIODS.STUDENT_KEY_WSN_XREF b WITH(NOLOCK)
                        on a.WSN = b.WSN
                        inner join K12INTEL_DW.DTBL_STUDENTS c WITH(NOLOCK)
                        on a.STUDENT_KEY = c.STUDENT_KEY") 
  keyTable <- sqlQuery(conn, crossRefQuery)
  return(keyTable)
}


#' Create a training dataset for a single cohort
#'
#' @param cohortYear a character string of the format YYYY-YYYY
#' @param grade a character representing the grade level in the format 10 or 0X
#' @param conn a connection object with the data connections necessary to access 
#' the database
#'
#' @return a data.frame with all the data for a single cohort organized with 
#' each row representing a single student, and each column a measure for that 
#' student in the grade-school-year selected
#' @export
assembleCohort <- function(cohortYear, grade, conn, type = "train"){
  tmpCoh <- pullCohort(cohortYear, grade, conn, type)
  peerCoh <- peerStats(tmpCoh)
  out <- merge(tmpCoh, peerCoh, by.x = c("SCHOOL_KEY"), 
               by.y = c("cohort_SCHOOL_KEY"), all.x=TRUE)
  return(out)
}
