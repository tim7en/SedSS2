#function converts elements with mixed signs according to user defined options
convert <- function(dat, neg_glob, zero_const = 0.001) {
  #mat <- dat[,-c(1,2)]
  dat[dat == 0] <- zero_const #replace 0 with a constant that user defined
  negatives <- colnames(dat)[-c(1, 2)][sapply(dat[, -c(1, 2)], function(x) min(x)) <= 0]
  positives <- colnames(dat)[-c(1, 2)][sapply(dat[, -c(1, 2)], function(x) max(x)) >= 0]
  mixed <- negatives[which(negatives %in% positives)]
  
  negatives_only <- negatives[which(!negatives %in% mixed)]
  dat[,c(negatives_only)]<- (dat[,c(negatives_only)] * -1)
  #print ('inside of the function, before if')
  if (length(mixed) == 0) {
    #print ('returning dat')
    return (dat)
  }

  #print ('passed first if')
  if (length(mixed) == 1) {
    
    x <- dat[, c(mixed)]
    dat_origin <- x
    formulas <- strsplit(neg_glob, ",")
    convert <- eval(parse(text = formulas[[1]][1]))
    dat[, c(mixed)] <- convert
    return(dat)
  } else if (length(mixed) > 1) {
    if (length(unlist(strsplit(neg_glob, ","))) > 2) {
      formulas <- unlist(strsplit(neg_glob, ";"))
      if (length(formulas) == length(mixed)) {
        l <- sapply(formulas, strsplit, split = ",")
        flag <- 0
        for (i in seq(1, length(l))) {
          x <- dat[, c(mixed[i])]
          dat_origin <- x
          convert <- eval(parse(text = l[[i]][1]))
          dat[, c(mixed[i])] <- convert
        }
        return(dat)
      } else {
        NULL
      }
    }
  } else {
    NULL
  }
}
