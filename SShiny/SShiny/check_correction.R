check_correction <- function(datas, trg, dff, corFor) {
  
  dff$sign <- 0
  
  x <- as.data.frame(datas)
  x[, 3:ncol(x)] <- apply(x[, 3:ncol(x)], 2, as.numeric)
  l <- NULL
  coefsOut <- list()
  k <- 1
  j <- 1

  # for (i in seq (1, nrow (formulas))){
  for (i in seq(1, dim(dff)[1])) { # for each formula in the table
    f <- dff[i, ] # extract formula
    datas <- x
    datas <- datas[which(datas[, 2] == as.character(f[[7]])), ] # extract class

    datas[, 3:ncol(datas)] <- apply(datas[, 3:ncol(datas)], 2, as.numeric) # make sure numeric
    vals <- strsplit(as.character(f[[5]]), "~") # split formula to get conversion of the initial concentration for src

    element <- eval((parse(text = vals[[1]][1]))) # convert src element concentration

    if (length(corFor) > 1) {
      var1 <- datas[, corFor[1]] # get source data (x)
      var2 <- datas[, corFor[2]] # get source data (y)

      # will be helpful in the future
      fit <- lm(eval((parse(text = as.character(f[[5]]))))) # evaluate equation
      # get conversions needed
      vals_n <- vals[[1]][2]
      vals_n <- strsplit(vals_n, " ")
      size_f <- vals_n[[1]][grep("var1", vals_n[[1]])]
      toc_f <- vals_n[[1]][grep("var2", vals_n[[1]])]

      # convert source data, size
      if (length(var1) == 0) {
      } else {
        sourceSize <- eval(parse(text = size_f))
      }

      # convert source data, toc
      if (length(var2) == 0) {} else {
        sourceToc <- eval(parse(text = toc_f))
      }

      var1 <- as.numeric(trg[corFor[1]]) # use values of only first target
      var2 <- as.numeric(trg[corFor[2]])

      targetSize <- eval(parse(text = size_f)) # convert them
      targetToc <- eval(parse(text = toc_f)) # convert values

      # Drop intercept if there any from coefficients
      coefs <- coefficients(fit)[names(coefficients(fit)) != "(Intercept)"]
      coefs <- as.numeric(coefs)
      Yi <- element
      Si <- sourceSize
      Ti <- sourceToc
      Sj <- targetSize
      Tj <- targetToc
    } else {
      var1 <- datas[, corFor] # get source data (x)

      # will be helpful in the future
      fit <- lm(eval((parse(text = as.character(f[[5]]))))) # evaluate equation
      # get conversions needed
      vals_n <- vals[[1]][2]
      vals_n <- strsplit(vals_n, " ")
      size_f <- vals_n[[1]][grep("var1", vals_n[[1]])]

      sourceSize <- eval(parse(text = size_f))
      var1 <- as.numeric(trg[corFor]) # use values of only first target
      targetSize <- eval(parse(text = size_f)) # convert them
      # Drop intercept if there any from coefficients
      coefs <- coefficients(fit)[names(coefficients(fit)) != "(Intercept)"]
      coefs <- list(as.numeric(coefs))

      Yi <- element
      Si <- sourceSize
      Ti <- NULL
      Sj <- targetSize
      Tj <- NULL
    }


    if (length(coefs) == 3) {
      Ss <- coefs[1] # Size slope
      Ts <- coefs[2] # Toc slope
      TjSj <- coefs[3] # Interaction slope
      Corrected <- (Yi - (((Si - Sj) * Ss) + ((Ti - Tj) * Ts) + (((Si * Ti) - (Sj * Tj)) * TjSj)))
    } else if (length(coefs) == 2) {
      if (length(size_f) == 0) { # toc and interaction
        Ts <- coefs[1]
        TjSj <- coefs[2]
        Corrected <- (Yi - (((Ti - Tj) * Ts)) + (((Si * Ti) - (Sj * Tj)) * TjSj))
      }
      else if (length(toc_f) == 0) { # size and interaction
        Ss <- coefs[1]
        TjSj <- coefs[2]
        Corrected <- (Yi - (((Si - Sj) * Ss)) + (((Si * Ti) - (Sj * Tj)) * TjSj))
      } else { # size and toc only
        Ss <- coefs[1]
        Ts <- coefs[2]
        Corrected <- (Yi - (((Si - Sj) * Ss) + ((Ti - Tj) * Ts)))
      }
    } else if (length(coefs) == 1) {
      if (length(size_f) == 0) {
        Ts <- coefs[[1]]
        Corrected <- (Yi - ((Ti - Tj) * Ts))
      } # toc
      else {
        Ss <- coefs[[1]]
        Corrected <- (Yi - ((Si - Sj) * Ss))
      }
    }
    
    options(warn = -1)
    
    if (any(is.na(as.numeric(Corrected))) && all(Yi>=0)) {
      
    } else if (any(Corrected <= 0) && all(Yi>=0)) {
      
    } else if (is.nan(any(as.numeric(Corrected)))){
    
    } else if (is.infinite(any(as.numeric(Corrected)))){
    
    } else if (all(as.numeric(Corrected)>=0) && all(Yi>=0)){
      dff$sign[i] <- 1
    } else if (any (Yi <=0) && any(as.numeric(Corrected)<=0) && is.numeric(as.numeric(Corrected))){
      #dff$sign[i] <- 1
    } else {
      
    }
    
    options(warn = 1)
  }
  
  return (dff[which(dff$sign>0),-(ncol(dff))])
}
