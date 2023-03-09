#' Normalize down- and upwelling irradiance to surface irradiance
#'
#' @param trios A trios object
#'
#' @return A normalized trios object
#' @export
normalize_to_surface <- function(trios){
  
  # also normalize air sensor to prevent normalizing multiple times
  trios$air <- trios$air / trios$air
  
  trios$up <- trios$up / trios$air
  trios$down <- trios$down / trios$air
  return(trios)
}