#' A function to plot histograms for all given variables in a dataset and arrange them in a grid
#' 
#' @param dt Data.frame containing the variables to plot
#' @param vars Variables to use for the histogram grid
#' @param ncols Number of columns in histogram grid. Default: 2
#' @param binwidth Custom binwidth for the plots. 
#'                 Currently this parameter is used for every historgram
#' @return Plots a grid of histograms
#' @import ggplot2
#' @import gridExtra
#' @examples 
#' 
#' 
#' @export

histogramGrid <- function(dt, vars, ncols = 2, binwidth = NULL) {
  require(ggplot2)
  require(gridExtra)
  
  
  #check if variable names in provided datatable
  if (min(vars %in% names(dt)) != 1) {
    stop(paste(vars[which(!(vars %in% names(dt)))], "not a variable of dt"))
  }
  
  # #check if binwidth is same length as vars
  # if (!is.null(binwidth) & length(binwidth) != length(vars)) {
  #   stop("length of binwidth is not equal to length of vars")
  # }
  
  #check weather variables continious or categorial
  #####.....#####
  
  plots <- lapply(vars, function(var) {
    p <- ggplot(data = dt) +
      geom_histogram(aes(x = dt[var]), binwidth = binwidth) +
      xlab(var)
  })
  
  
  do.call(grid.arrange, c(plots, ncol = ncols))
}



