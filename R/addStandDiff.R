#' A function to calculate standardized differences between xi and the rolling mean of xi-1 to xi-n
#' for given variables in a data frame.
#' It calculates the difference between xi and the rolling mean. At the end it divides
#' the calculated difference by the standard deviation and adds the results to dt
#'
#' @param dt data frame which includes the variables for which to calculate the standardized differences
#' @param vars character vector of variable names for which to calculate the standardized differences
#' @param nDays number of days to use for rolling mean and rolling sd
#' @param dateColumn name (character) of date column which is used to arrange data
#' Default = `"date"`
#' @param groupVars name of grouping variable ind dt if calculation should be done groupwise
#' (e.g. for each calf individual).Must be Character. Default = `"calf"`
#' @param sdMethod parameter to choose how standard deviation (used for standardizing) is calculated:
#' `"cumulative"` - calculates standard deviation from first value in vector (x[1]) to x[i] and
#' updates it for every x[i]
#' `"rolling"` - calculates running standard deviation within the same window as the rolling mean (-> nDays)
#'
#'
#' @return Returns differnce to rolling mean `diffToRollMean`, rolling sd `rollSD` and
#' standardized differnece `standardDiff` for all given variables and adds them to the data frame
#' @import tidyverse
#' @import RcppRoll
#' @examples
#'
#' @export


addStandDiff <- function(dt, nDays, groupVars = "calf", vars, dateColumn = "date", sdMethod = "cumulative") {

  library(RcppRoll)
  library(tidyverse)

  #TODO
  # - check if variables all numeric / integer
  # - how to handle missing days in data table
  # -

  # check if all vars in data frame
  if (!all(vars %in% names(dt))) {
    stop(paste0("Variable '", vars[which.min(vars %in% names(dt))], "' not in data frame"))
  }

  # check if groupVars and dateColumn in data frame
  if (!all(groupVars %in% names(dt))) {
    stop(paste0("Variable '", groupVars[which.min(groupVars %in% names(dt))], "' not in data frame"))
  }

  #check if right sdMethod provided
  if (!sdMethod %in% c("cumulative", "rolling")) {
    stop("Wrong sdMethod, please choose 'cumulative' or 'rolling'" )
    }


  # define custom functions for difference to rolling mean and standard deviation
  diffToRollMean <- function(x, nDays) {
    require(RcppRoll)
    x - RcppRoll::roll_mean(x, na.rm = T, n = nDays,
                  align = "right", fill = NA)}

  customRollSD <- function(x, nDays) roll_sd(x, na.rm = T, n = nDays, align = "right", fill = NA)

  standDiffToRollMean <- function(x, nDays, sdMethod = sdMethod) {
    if (sdMethod == "rolling") { diffToRollMean(x, nDays) / customRollSD(x, nDays)}
    if (sdMethod == "cumulative") { diffToRollMean(x, nDays) / sqrt(cumVar(x))}
    }

  #Calculate diffToRollMean, customRollSD, standardized Difference and add it to dt
  dt <- dt %>%
    arrange_(dateColumn) %>%
    group_by_at(vars(one_of(groupVars))) %>%
    mutate_at(.funs = funs(diffToRollMean = diffToRollMean(., nDays),
                   rollSD = customRollSD(., nDays),
                   cumSD = sqrt(cumVar(.)),
                   standardDiff = standDiffToRollMean(., nDays, sdMethod)),
              .vars = vars(vars))

  return(dt)
}


