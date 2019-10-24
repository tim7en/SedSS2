x <- dat_glob


filter_regtable <- function(x) {
  unique_targets <- unique(x$target)
  unique_sources <- unique(x$source)
  unique_elements <- unique(x$element)

  OUTPUT <- NULL
  # for all targets
  for (iter1 in seq(1, length(unique_targets))) {
    s <- which(x$target %in% unique_targets[iter1]) # subset
    y <- x[s, ]

    # for all sources
    for (iter2 in seq(1, length(unique_sources))) {
      s2 <- which(y$source %in% unique_sources[iter2]) # subset source
      y2 <- y[s2, ]

      # for all elements
      for (iter3 in seq(1, length(unique_elements))) {
        s3 <- which(y2$element %in% unique_elements[iter3]) # subset element
        y3 <- y2[s3, ]

        sub_simple <- y3[grepl("+", y3$stepV, fixed = T), ] # subset simple regression (multiple terms)
        sub_multipl <- y3[grepl("*", y3$stepV, fixed = T), ] # subset interaction regression (multiple terms)

        sub_single <- y3[which(!rownames(y3) %in% rownames(sub_simple)), ] # subset (single term regression)
        sub_single <- sub_single[which(!rownames(sub_single) %in% rownames(sub_multipl)), ] # subset (single term regres)


        # filter without interaction, multiple terms
        varr <- sub_simple %>% group_by(target, source, element) # group_by(source, element) #group_by (target, source, element)
        f <- varr %>% dplyr::arrange(-desc(residualsSD), desc(Cooks), .by_group = TRUE)
        f <- f %>%
          dplyr::group_by(target, source, element) %>%
          dplyr::mutate(rank = rank(-desc(residualsSD), ties.method = "first"))
        f <- f %>% group_by(target, source, element) %>% filter(row_number() <= 10)
        f <- f[, -which(names(f) %in% c("formula", "grade", "Cooks", "residualsSD", "p-eq", "p-resi", "p.eq", "p.resi"))]
        f1 <- as.data.frame(f)


        # filter with interaction, multiple terms
        varr <- sub_multipl %>% group_by(target, source, element) # group_by(source, element) #group_by (target, source, element)
        f <- varr %>% dplyr::arrange(-desc(residualsSD), desc(Cooks), .by_group = TRUE)
        f <- f %>%
          dplyr::group_by(target, source, element) %>%
          dplyr::mutate(rank = rank(-desc(residualsSD), ties.method = "first"))
        f <- f %>% group_by(target, source, element) %>% filter(row_number() <= 10)
        f <- f[, -which(names(f) %in% c("formula", "grade", "Cooks", "residualsSD", "p-eq", "p-resi", "p.eq", "p.resi"))]
        f2 <- as.data.frame(f)

        # filter single terms
        varr <- sub_single %>% group_by(target, source, element) # group_by(source, element) #group_by (target, source, element)
        f <- varr %>% dplyr::arrange(-desc(residualsSD), desc(Cooks), .by_group = TRUE)
        f <- f %>%
          dplyr::group_by(target, source, element) %>%
          dplyr::mutate(rank = rank(-desc(residualsSD), ties.method = "first"))
        f <- f %>% group_by(target, source, element) %>% filter(row_number() <= 10)
        f <- f[, -which(names(f) %in% c("formula", "grade", "Cooks", "residualsSD", "p-eq", "p-resi", "p.eq", "p.resi"))]
        f3 <- as.data.frame(f)


        # extract only rank 1
        f1 <- f1[which(f1$rank == 1), ]
        f2 <- f2[which(f1$rank == 1), ]
        f3 <- f3[which(f1$rank == 1), ]

        # combine them in one table

        OUTPUT <- rbind(OUTPUT, f1, f2, f3)
        OUTPUT <- data.frame(na.omit(OUTPUT))
      }
    }
  }
  return(OUTPUT)
}
