#' @export
trios_get_reflectance <- function(trios){

  
  p.refl <- trios[["up"]] / trios[["down"]] # calculate spectral reflectance
  
  return(p.refl)
}



#' @export
trios_plot_reflectance <- function(trios){

  p.refl <- trios_get_reflectance(trios)
  
  
  plot(x = trios$w, y = p.refl[,1], type="h", lwd=4, col=trios_palette(length(trios$w)))
  abline(v=665, lty=2)
}
