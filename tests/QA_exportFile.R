#################################################################################
#################################################################################
## DEWS Export Test Script
#################################################################################
#################################################################################

## If missingness becomes allowed, then this data will need to be augmented
## Convert counts to percentages of rows

# Set up environment
nacheck <- function(x){
  return(length(x[is.na(x)]))
}

# setwd("D:/Rshare/PROJECTS/DEWS")

YEAR <- "2013-2014"
TYPE <- "source"
# Get data

getFile <- function(year, stub = "/export/"){
  filelist <- list.files(file.path(paste0(getwd(), stub, year)), full.names=TRUE)
  return(filelist)
}

if(TYPE == "source"){
  export <- do.call("rbind", lapply(getFile(year = YEAR), read.csv, header = TRUE)) 
} else if(type == "db"){
  print("Not yet, add SQL queries here")
}


context("Correct Dimensions")
test_that("Rows are correct", {
  expect_that(nrow(export), is_more_than(240000))
})

test_that("Columns are correct", {
  expect_equal(ncol(export), 29)
})

test_that("Model dimensions are correct", {
  expect_that(table(export$MODEL_NAME)[[1]], is_more_than(59000))
  expect_that(table(export$MODEL_NAME)[[2]], is_more_than(59000))
  expect_that(table(export$MODEL_NAME)[[3]], is_more_than(59000))
  expect_that(table(export$MODEL_NAME)[[4]], is_more_than(59000))
})

etlnames <- c("X", "WSN", "STUDENT_KEY", "DATE", "MODEL", "MODEL_DATA", 
              "MODEL_NAME", 
              "STUDENT_RISK_SEVERITY_SCORE", "STUDENT_RISK_SEVERITY_PRECISION", 
              "STUDENT_RISK_OUTCOME", "STUDENT_RISK_MEASURE_VALUE_Att", 
              "STUDENT_RISK_MEASURE_VALUE_Att2", "STUDENT_RISK_MEASURE_VALUE_Att3", 
              "STUDENT_RISK_MEASURE_VALUE_Att4", "STUDENT_RISK_REPORT_TEXTAtt", 
              "STUDENT_RISK_MEASURE_VALUE_Assess1", 
              "STUDENT_RISK_MEASURE_VALUE_Assess2", 
              "STUDENT_RISK_MEASURE_VALUE_Assess3", 
              "STUDENT_RISK_MEASURE_VALUE_Assess4", 
              "STUDENT_RISK_REPORT_TEXTAssess", 
              "STUDENT_RISK_MEASURE_VALUE_Disc", "STUDENT_RISK_MEASURE_VALUE_Disc2", 
              "STUDENT_RISK_MEASURE_VALUE_Disc3", "STUDENT_RISK_REPORT_TEXTDisc", 
              "STUDENT_RISK_MEASURE_VALUE_Mob", 
              "STUDENT_RISK_MEASURE_VALUE_Mob2", "STUDENT_RISK_REPORT_TEXTMob", 
              "GRADE_LEVEL", "SCHOOL_YEAR")

test_that("Columns are in right order with right names", {
  expect_true(identical(names(export), etlnames))
  expect_true(all(names(export) %in% etlnames))
  expect_true(all(etlnames %in% names(export)))
})

context("Overall Risk")

test_that("Overall risk dimensions are correct", {
  expect_more_than(table(export$STUDENT_RISK_OUTCOME)[["High"]], 15000)
  expect_more_than(table(export$STUDENT_RISK_OUTCOME)[["Low"]], 100000)
  expect_more_than(table(export$STUDENT_RISK_OUTCOME)[["Moderate"]], 15000)
  expect_less_than(table(export$STUDENT_RISK_OUTCOME)[["High"]], 40000)
  expect_less_than(table(export$STUDENT_RISK_OUTCOME)[["Low"]], 200000)
  expect_less_than(table(export$STUDENT_RISK_OUTCOME)[["Moderate"]], 50000)
  expect_less_than(nacheck(export$STUDENT_RISK_SEVERITY_SCORE), 100)
  expect_more_than(max(export$STUDENT_RISK_SEVERITY_SCORE, na.rm=T), .9)
  expect_less_than(min(export$STUDENT_RISK_SEVERITY_SCORE, na.rm=T), .25)
})

context("Attendance Risk")

test_that("Overall attendance dimensions are correct", {
  expect_that(table(export$STUDENT_RISK_REPORT_TEXTAtt)[[1]], is_more_than(10000))
  expect_that(table(export$STUDENT_RISK_REPORT_TEXTAtt)[[2]], is_more_than(100000))
  expect_that(table(export$STUDENT_RISK_REPORT_TEXTAtt)[[3]], 
              is_more_than(table(export$STUDENT_RISK_REPORT_TEXTAtt)[[1]]))
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Att), 500)
})


test_that("Attendance sub dimensions are correct", { 
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Att, na.rm=T), 0)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Att, na.rm=T), 180)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Att2, na.rm=T), 0)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Att2, na.rm=T),
               mean(median(export$STUDENT_RISK_MEASURE_VALUE_Att2, na.rm=T)))
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Att2, na.rm=T), 100)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Att3, na.rm=T), 0)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Att3, na.rm=T), 100)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Att4, na.rm=T), 1)
  expect_that(max(export$STUDENT_RISK_MEASURE_VALUE_Att4, na.rm=T), is_more_than(180))
  expect_that(median(export$STUDENT_RISK_MEASURE_VALUE_Att4, na.rm=T), is_more_than(160))
 })

context("Assessment Risk")

test_that("Overall assessment dimensions are correct", {
  expect_equal(dim(table(export$STUDENT_RISK_REPORT_TEXTAssess)), 3)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["High"]], 10000)
  expect_less_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["High"]], 40000)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["Low"]], 100000)
  expect_less_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["Low"]], 125000)
  expect_less_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["Moderate"]], 100000)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTAssess)[["Moderate"]], 30000)
})

test_that("Assessment sub dimensions are correct", { 
  expect_equal(nacheck(export$STUDENT_RISK_REPORT_TEXTAssess), 0)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Assess1), 6000)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Assess2), 6000)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Assess3), 6000)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Assess4), 6000)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Assess1), 790)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Assess1), 290)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Assess2), 730)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Assess2), 270)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Assess3), 0)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Assess3), 100)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Assess3), 50)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Assess4), 0)
  expect_equal(max(export$STUDENT_RISK_MEASURE_VALUE_Assess4), 100)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Assess4), 50)
})

context("Discipline Risk")
# Fix here
test_that("Overall discipline dimensions are correct", {
  expect_equal(dim(table(export$STUDENT_RISK_REPORT_TEXTDisc)), 3)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTDisc)[["High"]], 4900)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTDisc)[["Low"]], 150000)
  expect_less_than(table(export$STUDENT_RISK_REPORT_TEXTDisc)[["Moderate"]], 9000)
  expect_equal(nacheck(export$STUDENT_RISK_REPORT_TEXTDisc), 0)
})

test_that("Discipline sub dimensions are correct", { 
  expect_equal(nacheck(export$STUDENT_RISK_REPORT_TEXTDisc), 0)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Disc), 6000)
  expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Disc2), 6000)
#   expect_less_than(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 6000)
  expect_more_than(max(export$STUDENT_RISK_MEASURE_VALUE_Disc), 120)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Disc), 0)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Disc), 0)
  expect_more_than(max(export$STUDENT_RISK_MEASURE_VALUE_Disc2), 5)
  expect_less_than(max(export$STUDENT_RISK_MEASURE_VALUE_Disc2), 180)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Disc2), 0)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Disc2), 0)
#   expect_more_than(max(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 120)
#   expect_less_than(max(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 200)
#   expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 0)
#   expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 0)
#   expect_less_than(mean(export$STUDENT_RISK_MEASURE_VALUE_Disc3), 
#               mean(mean(export$STUDENT_RISK_MEASURE_VALUE_Disc2)))
  expect_more_than(mean(export$STUDENT_RISK_MEASURE_VALUE_Disc), 
              mean(mean(export$STUDENT_RISK_MEASURE_VALUE_Disc2)))
})

context("Mobility Risk")

test_that("Overall mobility dimensions are correct", {
  expect_equal(dim(table(export$STUDENT_RISK_REPORT_TEXTMob)), 3)
  expect_less_than(table(export$STUDENT_RISK_REPORT_TEXTMob)[["High"]], 2000)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTMob)[["Low"]], 150000)
  expect_more_than(table(export$STUDENT_RISK_REPORT_TEXTMob)[["Moderate"]], 1000)
  expect_equal(nacheck(export$STUDENT_RISK_REPORT_TEXTMob), 0)
})

test_that("Mobility sub dimensions are correct", { 
  expect_equal(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Mob), 0)
  expect_equal(nacheck(export$STUDENT_RISK_MEASURE_VALUE_Mob2), 0)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Mob), 1)
  expect_equal(min(export$STUDENT_RISK_MEASURE_VALUE_Mob2), 1)
  expect_more_than(max(export$STUDENT_RISK_MEASURE_VALUE_Mob), 4)
  expect_more_than(max(export$STUDENT_RISK_MEASURE_VALUE_Mob2), 2)
  expect_that(mean(export$STUDENT_RISK_MEASURE_VALUE_Mob), 
              is_more_than(mean(export$STUDENT_RISK_MEASURE_VALUE_Mob2)))
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Mob), 1)
  expect_equal(median(export$STUDENT_RISK_MEASURE_VALUE_Mob2), 1)
})

