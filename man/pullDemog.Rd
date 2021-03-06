% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/dataExtract.R
\name{pullDemog}
\alias{pullDemog}
\title{Pull down demographic data}
\usage{
pullDemog(cohortYear, grade, conn)
}
\arguments{
\item{cohortYear}{a character string of the format YYYY-YYYY}

\item{grade}{a character representing the grade level in the format 10 or 0X}

\item{conn}{a connection object with the data connections necessary to access
the database}
}
\value{
Returns a data.frame with student demographic data and merge keys
}
\description{
Pull down demographic data
}
\details{
This function is meant to pull a single year and single grade of
demographic data for students and to do the necessary data cleaning and reshaping
to create data with a single row per student per year per grade and with columns
representing demographic data for model building. This function is
called as part of a larger function which assembles all data on a cohort for
processing in a model search. Specifically in this case it includes race,
school, student disability status, student economic disadvantage status,
student ELL status, student retention indicator, student attendance,
student gender, and school district
}

