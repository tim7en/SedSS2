# function to output summary table of a data frame
data_summary <- function(x) {
  expr <- tryCatch({
    tabSummary <- NULL
    tabSummary <- rbind(tabSummary, sapply(x, function(x) class(x)))
    tabSummary <- rbind(tabSummary, sapply(x, function(x) sum(is.na(x))))
    tabSummary <- rbind(tabSummary, sapply(x, function(x) nlevels(x)))
    tabSummary <- rbind(tabSummary, apply(x, 2, is.negative))
    tabSummary <- rbind(tabSummary, apply(x, 2, is.zero))
    rownames(tabSummary) <- c("Type", "NAs", "Unique", "Negative", "Zeros")
  }, error = function(e) {
    return(NULL)
  })
  return(tabSummary)
}