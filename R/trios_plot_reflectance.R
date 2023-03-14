#' @export
trios_get_reflectance <- function(trios, par_region=TRUE){
  if (par_region){
    subset_to_wavelength(trios, 400, 700)
  }
  
  p.refl <- trios[["up"]] / trios[["down"]] # calculate spectral reflectance
  
  return(p.refl.par)
}



#' @export
trios_plot_reflectance <- function(trios){
  w <- as.numeric(rownames(trios[[1]]))
  w.par <- w[(w > 400) & (w < 700)]
  p.refl.par <- trios_get_reflectance(trios)
  
  
  Rainbow <- colorRampPalette(c("violet", "blue1", "green", "yellow", "orange", "red", "darkred"))
  plot(x = w.par, y = p.refl.par[,1], type="h", lwd=4, col=Rainbow(length(w.par)))
  abline(v=665, lty=2)
}
