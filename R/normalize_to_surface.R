#' Normalize down- and upwelling irradiance to surface irradiance
#'
#' @param trios A trios object
#'
#' @return A normalized trios object
#' @export
normalize_to_surface <- function(trios, relative_to_surface = TRUE){
  
  # also normalize air sensor to prevent normalizing multiple times
  trios$air <- trios$air / trios$air
  trios$up <- trios$up / trios$air
  trios$down <- trios$down / trios$air
  
  if (relative_to_surface){
    trios$up <- trios$up / trios$up[,1]
    trios$down <- trios$down / trios$down[,1]
  }
  
  return(trios)
}