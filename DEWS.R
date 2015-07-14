##########################################
# DPI Early Warning System
# Version 0.1
# 01-31-2013
# Author: Jared Knowles
# See dependencies.txt for information about setting up the system
##########################################

cat("Welcome")
print("Connect to LDS")
ds <- packrat::with_extlib("dpiR", checkCreds)

dewsBootstrap()

modelSearch(grade = 5, mode = "PROD", fileName = "cache/CARETMODELFITS_APR_2015")
modelSearch(grade = 6, mode = "PROD", fileName = "cache/CARETMODELFITS_APR_2015")
modelSearch(grade = 7, mode = "PROD", fileName = "cache/CARETMODELFITS_APR_2015")
modelSearch(grade = 8, mode = "PROD", fileName = "cache/CARETMODELFITS_APR_2015")

library(caretEnsemble)

constructModels(grade = 5, mode = "PROD", modelResultsName = "cache/CARETMODELFITS_APR_2015", 
                conn = ds[[4]], nmodels = 8)
constructModels(grade = 6, mode = "PROD", modelResultsName = "cache/CARETMODELFITS_APR_2015", 
                conn = ds[[4]], nmodels = 8)
constructModels(grade = 7, mode = "PROD", modelResultsName = "cache/CARETMODELFITS_APR_2015", 
                conn = ds[[4]], nmodels = 5)
constructModels(grade = 8, mode = "PROD", modelResultsName = "cache/CARETMODELFITS_APR_2015", 
                conn = ds[[4]], nmodels = 5)

library(EWStools)
rawPreds(stub = "EnsembleModels_APRIL", grade = 5, year = '2013-2014', 
         conn = ds[[4]], cache = TRUE)
rawPreds(stub = "EnsembleModels_APRIL", grade = 6, year = '2013-2014', 
         conn = ds[[4]], cache = TRUE)
rawPreds(stub = "EnsembleModels_APRIL", grade = 7, year = '2013-2014', 
         conn = ds[[4]], cache = TRUE)
rawPreds(stub = "EnsembleModels_APRIL", grade = 8, year = '2013-2014', 
         conn = ds[[4]], cache = TRUE)


exportPreds(stub = "EnsembleModels_APRIL", grade = 5, year = '2013-2014', 
            conn = ds[[4]], cache = TRUE)
exportPreds(stub = "EnsembleModels_APRIL", grade = 6, year = '2013-2014', 
            conn = ds[[4]], cache = TRUE)
exportPreds(stub = "EnsembleModels_APRIL", grade = 7, year = '2013-2014', 
            conn = ds[[4]], cache = TRUE)
exportPreds(stub = "EnsembleModels_APRIL", grade = 8, year = '2013-2014', 
                   conn = ds[[4]], cache = TRUE)

library(testthat)
test_file("R/QA_exportFile.R")


###########################################################################
## 
##  GET DEWS STATS
##
##
# ########################################################################


# Lines of code

# library(R.utils)
# 
# codebase <- countLines("R/pkgs.R") +
#             countLines("R/LDS_data_pull.R") +
#             countLines("R/LDS_data_pull_predictions.R") +
#             countLines("R/imputation_routines.R") +
#             countLines("R/lds_connect.R") +
#             countLines("R/prediction_data_cleaning.R") +
#             countLines("R/pre_export_checks.R") +
#             countLines("R/postprediction.R") +
#             countLines("R/postprediction_prep.R") +
#             countLines("R/modelsearch.R") +
#             countLines("R/model_testing_functions.R") +
#             countLines("R/model_build.R") + 
#             countLines("inst/tests/test-data.R") +
#             countLines("inst/tests/test-pkgs.R") +
#             countLines("inst/tests/test-cleandata.R") +
#             countLines("inst/tests/test-cleanpredictiondata.R") +
#             countLines("inst/tests/buildverificationdata.R")
# 
# 


