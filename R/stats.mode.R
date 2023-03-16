
#' Function to compute the mode of a vector
#' 
#' @param vec Input vector for which the mode should be computed
#' @return Mode of the input vector, i.e., its most frequent value
#' @export
csslr.stats.mode <- function(vec, na.rm=FALSE) {
  if (na.rm == TRUE) {
    vec <- vec[!is.na(vec)]
  }

  vecUnique <- unique(vec)
  theMode <- vecUnique[which.max(tabulate(match(vec, vecUnique)))]

  return(theMode)
}
