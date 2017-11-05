#' A function to plot missing values in a dataset using ggplot
#' 
#' @param x data.frame or vector which should be used for the missing value plot
#' @return visualises missing values for each variable in the dataset
#' @import reshape2
#' @import ggplot2
#' @import dplyr
#' @examples 
#' x <- data.frame(a = rep(1,103), b = c(rep(2,53), rep(NA,20), rep(2, 30)), c = rep(NA, 103))
#' ggplot_missing(x)
#' 
#' @export


ggplot_missing <- function(x){
  require(reshape2)
  require(ggplot2)
  require(dplyr)
  
  if(is.vector(x)) { x <- data.frame(vector = x)}
  
  x %>% 
    is.na %>%
    
    
    melt %>%
    ggplot(data = .,
           aes(x = Var2,
               y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_grey(name = "",
                    labels = c("Present","Missing")) +
    theme_minimal() + 
    theme(axis.text.x  = element_text(angle=45, vjust=0.5)) + 
    labs(x = "Variables in Dataset",
         y = "Rows / observations")
}
