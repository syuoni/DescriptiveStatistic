#' Get the descriptive statistics for a data frame 
#' 
#' @name descriptive_stats
#' @param df data frame
#' @param na.rm remove NA if TRUE
#' 
#' @return a matrix of descriptive statistics
#'   \item{observations}{number of samples}
#'   \item{mean}{mean value}
#'   \item{std.dev}{standard deviation}
#'   \item{min}{minimun value}
#'   \item{max}{maximun value}
#' 
#' @export 

descriptive.stats <- function(df, na.rm=FALSE){
  # If a function is only to be called by another function, it could be written as a inner function. 
  descriptive.stats.variable <- function(x, na.rm=FALSE){
    if(na.rm){
      x <- x[!is.na(x)]
    }
    n <- length(x)
    if(is.numeric(x)){
      mean <- mean(x)
      sd <- sd(x)
      min <- min(x)
      max <- max(x)
    }else{
      mean <- NA
      sd <- NA
      min <- NA
      max <- NA
    }
    return(c(observations=n, mean=mean, std.dev=sd, min=min, max=max))
  }
  
  return(t(sapply(df, descriptive.stats.variable, na.rm=na.rm)))
}



