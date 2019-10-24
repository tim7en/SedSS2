# checks for negatives
is.negative <- function(x) {
  options(warn = -1)
  
  expr <- tryCatch({
    x <- try(as.numeric(x))
    if ((is.numeric(x) || is.integer(x))) {
      return(length(which(na.omit(x) < 0)))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
  
  options(warn = 1)
}

# checks for zeros
is.zero <- function(x) {
  options(warn = -1)
  
  expr <- tryCatch({
    x <- try(as.numeric(x))
    if ((is.numeric(x) || is.integer(x))) {
      return(length(which(x == 0)))
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
  
  options(warn = 1)
}