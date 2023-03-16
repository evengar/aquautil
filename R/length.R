#' @export
length.trios <- function(trios){
  length(trios$depth)
}

#' @export
dim.trios <- function(trios){
  dim(trios$up)
}