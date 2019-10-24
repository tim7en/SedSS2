library (energy)
target <- inverse_neg_glob(target, neg_glob, c(mixed))
target[c(negatives)] <- target[c(negatives)]*-1

targetafterBracket <- NULL
drops <- NULL
for (i in seq (1, nrow(target))){
  bracketTest <- bracketT(correctedDataFrames[[i]], target[i,], 0.1)
  targetafterBracket <- rbind (targetafterBracket, bracketTest[[1]])
  drops <- rbind (drops, bracketTest[[2]])
}

multinormRes <- NULL
for (i in seq (1, length (correctedDataFrames))){
  #correctedDataFrames[[i]] <- convert (correctedDataFrames[[i]], neg_glob, zero_const = 0.001)
  mymultiNormEnergyTest <- mvnorm.etest(as.matrix(correctedDataFrames[[i]][,-c(1,2)]), R = 199)
  multinormRes <- c(multinormRes, mymultiNormEnergyTest$p.value)
}

stepwiseDFA(correctedDataFrames)
