mylist_glob
dfa_glob
target_glob
samplerange <- 0.8

mainoutput <- NULL
mainvalidation <- NULL

for (j in seq(1, length(mylist_glob))) {

  # extract first table from the list
  datas <- mylist_glob[[j]]

  # split the table by proportions from mylist_glob
  train_index <- sample(1:nrow(datas), nrow(datas) * samplerange)

  # sample data by rows
  train_data <- datas[train_index, ]
  target_data <- datas[-train_index, ]
  validation_data <- datas[-train_index, ]

  # bind to global validation set
  mainvalidation <- rbind(mainvalidation, validation_data)


  # rename source types of target_data
  target_data[, 2] <- paste0("target_data", seq(1, nrow(target_data)))

  target_data <- convert(target_data, neg_glob, zero_const)

  # run mixing model against each drop
  dat <- convert(train_data, neg_glob, zero_const)

  datas <- getSubsetmean(dat[, -1])
  DFA <- dfa_glob
  target <- target_data

  DFA <- DFA[(which(colnames(DFA) %in% colnames(datas)))]
  DFA <- DFA[, colSums(DFA != 0) > 0]
  target <- target[, which(names(target) %in% colnames(DFA))]
  datas <- datas[, which(colnames(datas) %in% colnames(DFA))]
  target <- as.matrix(target)

  myoutput <- NULL

  for (i in seq(1, nrow(target))) {
    d <- UseUnMixing(target[i, ], datas, DFA[j, ], method = "Nelder-Mead")
    myoutput <- rbind(myoutput, d)
  }
  mainoutput <- rbind(mainoutput, myoutput)
}

mainoutput <- round (mainoutput, 3)
mainoutput <- data.frame (mainoutput)

#add source type
mainoutput$SourceType <- mainvalidation[,2]

mydat <- mainoutput [,-c(5,6)]

mydat <- t (mydat)
colnames(mydat) <- mainoutput$SourceType

#correlationplot
corrplot (mydat)


# longer with popup warning window for each element dropped (takes longer time)
