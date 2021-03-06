% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/addStandDiff.R
\name{addStandDiff}
\alias{addStandDiff}
\title{A function to calculate standardized differences between xi and the rolling mean of xi-1 to xi-n
for given variables in a data frame.
It calculates the difference between xi and the rolling mean. At the end it divides
the calculated difference by the standard deviation and adds the results to dt}
\usage{
addStandDiff(dt, nDays, groupVars = "calf", vars, dateColumn = "date",
  sdMethod = "cumulative", group = FALSE)
}
\arguments{
\item{dt}{data frame which includes the variables for which to calculate the standardized differences}

\item{nDays}{number of days to use for rolling mean and rolling sd}

\item{groupVars}{name of grouping variable ind dt if calculation should be done groupwise
(e.g. for each calf individual).Must be Character. Default = `"calf"`}

\item{vars}{character vector of variable names for which to calculate the standardized differences}

\item{dateColumn}{name (character) of date column which is used to arrange data
Default = `"date"`}

\item{sdMethod}{parameter to choose how standard deviation (used for standardizing) is calculated:
`"cumulative"` - calculates standard deviation from first value in vector (x[1]) to x[i] and
updates it for every x[i]
`"rolling"` - calculates running standard deviation within the same window as the rolling mean (-> nDays)}
}
\value{
Returns differnce to rolling mean `diffToRollMean`, rolling sd `rollSD` and
standardized differnece `standardDiff` for all given variables and adds them to the data frame
}
\description{
A function to calculate standardized differences between xi and the rolling mean of xi-1 to xi-n
for given variables in a data frame.
It calculates the difference between xi and the rolling mean. At the end it divides
the calculated difference by the standard deviation and adds the results to dt
}
\examples{

}
