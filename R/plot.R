#' Make a summary plot of a trios object
#'
#' @param trios A trios object
#'
#' @export
#'
#' @examples
#' 
#' plot(trios_march)
#' plot(trios_september)
#' 
plot.trios <- function(trios){
  par(mfrow = c(2,2))
  
  plot_depth_attenuation(trios)
  plot_downwelling_irradiance(trios)
  plot_attenuation_coefficient(trios)
  plot_reflectance(trios)
  
  par(mfrow = c(1,1))
}