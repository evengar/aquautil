#' @export
get_reflectance <- function(trios){

  
  p.refl <- trios[["up"]] / trios[["down"]] # calculate spectral reflectance
  
  return(p.refl)
}



#' @export
plot_reflectance <- function(trios, ...){

  p.refl <- get_reflectance(trios)
  
  
  plot(x = trios$w, y = p.refl[,1], type="h", lwd=4, col=trios_palette(length(trios$w)),
       main = "Surface reflectance",
       ...)
  abline(v=665, lty=2)
}
