#install.packages("fuzzySim") doesnt work, so here's the function; serves to convert from presence only to presence-absence

splist2presabs <- function(data, sites.col, sp.col, keep.n = FALSE) {
  # version 1.1 (7 May 2013)
  # data: a matrix or data frame with your localities and species (each in a different column)
  # sites.col: the name or index number of the column containing the localities
  # sp.col: the name or index number of the column containing the species names or codes
  # keep.n: logical, whether to get in the resulting table the number of times each species appears in each locality; if false (the default), only the presence (1) or absence (0) are recorded
  
  stopifnot(
    length(sites.col) == 1,
    length(sp.col) == 1,
    sites.col != sp.col,
    sites.col %in% 1 : ncol(data) | sites.col %in% names(data),
    sp.col %in% 1 : ncol(data) | sp.col %in% names(data),
    is.logical(keep.n)
  )
  
  presabs <- table(data[ , c(sites.col, sp.col)])
  presabs <- as.data.frame(unclass(presabs))
  if (!keep.n)  presabs[presabs > 1] <- 1
  presabs <- data.frame(row.names(presabs), presabs)
  names(presabs)[1] <- names(subset(data, select = sites.col))
  rownames(presabs) <- NULL
  return(presabs)
}  # end splist2presabs function



