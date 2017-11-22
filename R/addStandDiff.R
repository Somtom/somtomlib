#' A function to calculate standardized differences between xi and the rolling mean of xi-1 to xi-n
#' for given variables in a data frame.
#' It calculates the rolling standard deviation and the rolling mean for xi-1 to xi-n.
#' After that it calculates the difference between xi and the rolling mean. At the end it divides
#' the calculated difference by the standard deviation and adds the results to dt
#'
#' @param dt data frame which includes the variables for which to calculate the standardized differences
#' @param vars character vector of variable names for which to calculate the standardized differences
#' @param nDays number of days to use for rolling mean and rolling sd
#' @param dateColumn name (character) of date column which is used to arrange data
#' Default = `"date"`
#' @param groupVars name of grouping variable ind dt if calculation should be done groupwise
#' (e.g. for each calf individual).Must be Character. Default = `"calf"`
#'
#'
#' @return Returns differnce to rolling mean `diffToRollMean`, rolling sd `rollSD` and
#' standardized differnece `standardDiff` for all given variables and adds them to the data frame
#' @import tidyverse
#' @import RcppRoll
#' @examples
#'
#' @export


addStandDiff <- function(dt, nDays, groupVars = "calf", vars, dateColumn = "date") {
  require(RcppRoll)
  require(tidyverse)

  #TODO
  # - check if variables all numeric / integer
  # - how to handle missing days in data table
  # -

  # check if all vars in data frame
  if (!all(vars %in% names(dt))) {
    stop(paste0("Variable '", vars[which.min(vars %in% names(dt))], "' not in data frame"))
  }


  # define custom functions for difference to rolling mean and standard deviation
  diffToRollMean <- function(x, nDays) {
    x - roll_mean(x, na.rm = T, n = nDays,
                  align = "right", fill = NA)}

  customRollSD <- function(x, nDays) roll_sd(x, na.rm = T, n = nDays, align = "right", fill = NA)

  standDiffToRollMean <- function(x, nDays) {diffToRollMean(x, nDays) / customRollSD(x, nDays)}

  #Calculate diffToRollMean, customRollSD, standardized Difference and add it to dt
  dt <- dt %>%
    arrange_(dateColumn) %>%
    group_by_at(vars(one_of(groupVars))) %>%
    mutate_at(.funs = funs(diffToRollMean = diffToRollMean(., nDays),
                   rollSD = customRollSD(., nDays),
                   standardDiff = standDiffToRollMean(., nDays)),
              .vars = vars(vars))

  return(dt)
}


