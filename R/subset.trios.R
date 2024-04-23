#' Subset a trios object to certain depths
#'
#' @param trios A trios object
#' @param depths the depths to subset to
#'
#' @return A trios object containing only the specified depths
#' @export
subset_depth <- function(trios, depths){
  subset_index <- which(trios$depth %in% depths)
  trios$depth <- trios$depth[subset_index]
  trios$air <- trios$air[,subset_index]
  trios$up <- trios$up[,subset_index]
  trios$down <- trios$down[,subset_index]
  
  return(trios)
}

#' Subset a trios object to wavelengths and depths
#' @inheritParams subset_to_wavelength
#' @param depths the depths to subset to
#' @return a trios object containing the specified wavelengths and depths
#' @export
subset.trios <- function(trios, lower = NULL, upper = NULL, depths = NULL){
  if (!is.null(depths)){
    trios <- subset_depth(trios, depths)
  }
  if (!is.null(lower) & !is.null(upper)){
    trios <- subset_to_wavelength(trios, lower, upper)
  }
  return(trios)
}