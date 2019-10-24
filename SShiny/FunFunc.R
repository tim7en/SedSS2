FunFunc <- function() {
  inputTrain <- NULL
  inputValidate <- NULL
  # dat_transform <- function(x) {
  #   if (all(x < 0)) {
  #     x <- x * (-1)
  #   } else if (any(x <= 0)) {
  #     const <- abs(min(x))
  #     formulas <- strsplit(negGlob, ',')
  #     x <- eval(parse(text = formulas[[1]][1]))
  #     x
  #   } else {
  #     x
  #   }
  # }

  for (i2 in seq(1, length(uniSource))) {
    dat <- x[which(x[, 2] == uniSource[i2]), ]
    print ('running convert')
    dat <- convert (dat, negGlob,zeroConstant)
    print ('finished convert')
    
    train_index <- sample(1:nrow(dat), nrow(dat) * sourceSplitProportion)
    training_dat <- dat[train_index, ]
    validate_dat <- dat[-train_index, ]
    inputTrain <- rbind(inputTrain, training_dat)
    inputValidate <- rbind(inputValidate, validate_dat)
  }
  
  datas <- getSubsetmean(inputTrain[, -1])
  
  DFA <- DFA[(which(colnames(DFA) %in% colnames(datas)))]
  DFA <- DFA[, colSums(DFA != 0) > 0]
  target <- target[, which(names(target) %in% colnames(DFA))]
  datas <- datas[, which(colnames(datas) %in% colnames(DFA))]
  dat <- inputValidate [, -c(1, 2)]
  dat <- dat[, which(names(dat) %in% colnames(DFA))]
  matchNames <- match(colnames(dat), colnames(target))
  dat <- rbind(dat, target[matchNames])
  #dat <- apply(dat, 2, dat_transform)
  dat <- data.matrix(dat)
  target <- dat[nrow(dat), ]
  rownames(dat) <- c(as.character(inputValidate[, 1]), as.character(targetD[i, 1]))
  d <- UseUnMixing(target, datas, DFA, method = "Nelder-Mead")
  d <- round(d, 4)
  d <- c(d, targetD[i, 1])
  names(d) <- NULL
  names(d) <- c(rownames(datas), "GOF", "target")
  return(d)
}