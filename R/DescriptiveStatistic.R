#' @title descriptive statistic
#' @author syuoni
#' 
#' @name descriptive_statistic
#' @param df data frame to describe
#' 
#' @return a matrix of descriptive statistic
#' 
#' @export 

descriptive.stats <- function(df, na.rm=FALSE){
  return(t(sapply(df, descriptive.stats.variable, na.rm=na.rm)))
}

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

