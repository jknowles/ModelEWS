# #' Check credentials and open a channel. \emph{You probably don't need to call
# #' this function directly, it is called by the \code{pull*} functions.}
# #' 
# #' \code{checkCreds} is an enhanced wrapper to open a channel using RODBC's
# #' \link[RODBC]{odbcConnect} using your credentials that are stored in
# #' \code{"~/.dpi_datacreds"} (see \link{makeCreds} for information on creating
# #' this file for yourself)
# #' 
# #' @param dsn The name of the ODBC database containing the data of interest. 
# #'   (Check the 'System DSN' tab in the ODBC Data Source Administrator, for 
# #'   these)
# #'   
# #' @return a channel object
# #' 
# #' @importFrom RODBC odbcConnect
# #' 
# #' @export
# checkCreds <- function(dsn = NULL) {
#   #Get Credentials
#   if (file.exists("~/.dpi_datacreds")==FALSE) 
#     stop("First you need to create your data credential file using makeDataCreds()")
#   creds <- scan("~/.dpi_datacreds", character(0), sep="\n", comment.char="#", quiet=TRUE)
#   if(missing(dsn)){
#     L <- length(creds[nchar(creds) > 2])
#     creds <- creds[nchar(creds) > 2]
#     connList <- vector(L, mode = "list")
#     for(i in 1:L){
#       dsn <- scan(text=creds[i], what=character(0), quiet=TRUE)[1]
#       login <- scan(text=creds[i], what=character(0), quiet=TRUE)[2]
#       paswd <- scan(text=creds[i], what=character(0), quiet=TRUE)[3]
#       #open & return channel
#       channel <- RODBC::odbcConnect(dsn, uid = login, pwd = paswd, believeNRows=FALSE)
#       connList[[i]] <- channel
#       names(connList)[i] <- dsn
#     }
#     return(connList)
#   } else {
#     if (length(creds) > 1){
#       creds <- creds[grep(dsn, creds, fixed=TRUE)]
#     }
#     login <- scan(text=creds, what=character(0), quiet=TRUE)[2]
#     paswd <- scan(text=creds, what=character(0), quiet=TRUE)[3]
#     #open & return channel
#     channel <- RODBC::odbcConnect(dsn, uid = login, pwd = paswd, believeNRows=FALSE)
#     return(channel)
#   }
# }
# 
# #' Makes a properly formatted credential file
# #' 
# #' \code{makeCreds} creates the file used by the \code{pull*} functions to
# #' supply the SQL database with your credentials. Credentials for multiple
# #' databases can be submitted as vectors for each of \code{dsn}, \code{login},
# #' and \code{password}. When submitting multiple credentials make sure that 
# #' the order is correct.
# #' 
# #' @param dsn The name (or vector of names) of the ODBC database containing the
# #'   data of interest. If your login and password are identical for all
# #'   databases, use 'ALL' (Check the 'System DSN' tab in the ODBC Data Source
# #'   Administrator, for these).
# #'   
# #' @param login The login (or vector of logins) to the dsn.
# #' 
# #' @param password The password (or vector of passwords) to the dsn.
# #' 
# #' @param replace A logical scalar specifying whether or not \code{makeCreds}
# #'   should overwrite an existing credential file. If a credential file exists
# #'   and \code{replace}=FALSE then \code{makeCreds} exits with an error.
# #'   
# #' @return Nothing. A message is printed if writing the file was successfull.
# #' 
# #' @export
# makeCreds <- function(dsn, login, password, replace=FALSE) {
#   if (file.exists("~/.dpi_datacreds")==TRUE & replace==FALSE)
#     stop("You already have a credential file. To replace the existing file specify 'replace=TRUE'.")
#   if (sd(c(length(dsn), length(login), length(password)))!=0)
#     stop("The vectors for dsn, login and password are not all the same length.  Check your work :)")
#   header <- c(
#     paste("# .dpi_datacreds for", as.character(Sys.info()["user"])), 
#     paste("# created on", date()),
#     paste("# "),
#     paste("# The format of the credential list *must* be as follows:"),
#     paste("# DataSourceName login password"),
#     paste("# separated by space(s) *AND* with and end-of-line character"),
#     paste("# at the end of each line"),
#     paste("# "),
#     paste("# If you have the same login and password across all databases,"),
#     paste("# you can use 'ALL' for the DataSourceName."),
#     paste("# "),
#     paste("# Feel free to add comments as long as the line begins with '#'"),
#     paste(" "),
#     paste(" ")
#   )
#   footer <- c(
#     paste(" "),
#     paste(" "),
#     paste("# Happy Data Wrangling!"),
#     paste(" ")
#   )
#   writeLines(c(header, paste(dsn, login, password), footer), 
#              con="~/.dpi_datacreds")
#   cat(paste("Credential file successfully written to ~/.dpi_datacreds"))
# }
# 
# #'Lists column names of specified SQL table
# #'
# #'\code{pullColNames} returns the column names of the specified SQL table (using
# #'your credentials, see \code{\link{checkCreds}}) and returns them as a vector.
# #'
# #'@param dsn The name of the ODBC database containing the data of interest. 
# #'  (Check the 'System DSN' tab in the ODBC Data Source Administrator, for 
# #'  these)
# #'  
# #' @param sql.table The name of the specific table whose column names you want to
# #'  see.
# #'  
# #' @return A character vector of column names.
# #'  
# #' @importFrom RODBC sqlFetch
# #' @importFrom RODBC odbcClose   
# #' 
# #' @export
# pullColNames <- function(dsn, sql.table) {
#   #Obtain sql.query results
#   channel <- checkCreds(dsn)
#   colNames <- colnames(RODBC::sqlFetch(channel, sql.table, max=1))
#   RODBC::odbcClose(channel)
#   #return vector of colnames
#   return(colNames)
# }
# # 