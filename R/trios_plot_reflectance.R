#' @export
trios_get_reflectance <- function(trios){

  
  p.refl <- trios[["up"]] / trios[["down"]] # calculate spectral reflectance
  
  return(p.refl.par)
}



#' @export
trios_plot_reflectance <- function(trios){

  p.refl <- trios_get_reflectance(trios)
  
  
  Rainbow <- colorRampPalette(c("violet", "blue1", "green", "yellow", "orange", "red", "darkred"))
  plot(x = trios$w, y = p.refl[,1], type="h", lwd=4, col=Rainbow(length(trios$w)))
  abline(v=665, lty=2)
}
