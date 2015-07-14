# ################################################################################
# # Package dependencies for DEWS
# ################################################################################

#' Install packages in a list, but do not load them
#' @param package character, a package name
#' @param load logical, should the package be loaded after installing? default is FALSE
#' @return nothing
#' @export
installPackages <- function(package, load = FALSE) {
  if(is.element(package, installed.packages()[,1])) {
    print(paste0("Package ", package, " already installed"))
    
  } else {
    install.packages(package)
  }
  if(load == TRUE){
    print(paste0("Loading package: ", package))
    library(package, character.only = TRUE)
  }
}


#' Function to prepare the environment for model training and model building
#' @return nothing
#' @description Function loads all required packages in the correct order and 
#' installs all packages that need to be installed. This minimizes namespace conflicts.
#' @details First the package reads all the packages necessary out of the list of 
#' models ready to be trained stored in inst/data/ewsModels.csv. If the model list 
#' changes and this function is re-run, it will now install all necessary packages.
#' @export
dewsBootstrap <- function(){
  library(devtools)
  devtools::install_github("jknowles/EWStools")
  library(RODBC)
  library(caret)
  library(EWStools)
  library(plyr)
  library(dplyr); library(magrittr)
  
  EWS <- read.csv("inst/data/ewsModels.csv")
  
  pkglist <- unique(EWS$Package[EWS$EWS == "Yes"]); rm(EWS)
  pkglist <- pkglist[pkglist != "stats"] # sanitize list to remove base package
  pkglist <- as.character(pkglist)
  
  "%w/o%" <- function(x, y) x[!x %in% y] #--  x without y
  
  print("Searching for caret package dependencies...")
  #toInstall <- pkglist[pkglist %w/o% installed.packages()[,1]]
  toInstall <- pkglist; rm(pkglist)
  print("If this is the first run, then this may install several packages")
  Sys.sleep(3)
  print(paste0("I need to install or load", length(toInstall), " analysis packages for caret to train the models correctly."))
  Sys.sleep(3)
  
  for(i in toInstall){
    installPackages(i)
  }
  rm(i, toInstall)
  print("Namespace prepared")
}

packratPkg <- function(){
  library(devtools)
  library(SDDA)
  library(mda)
  library(glmnet)
  library(hda)
  library(caTools)
  library(stepPlr)
  library(klaR)
  library(sda)
  library(sparseLDA)
  library(party)
  library(earth)
  library(gbm)
  library(mboost)
  library(RSNNS)
  library(pls)
  library(rpart)
  library(spls)
  library(ipred)
  library(kohonen)
  
  
}
