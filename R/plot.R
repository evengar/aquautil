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
plot.trios <- function(trios, title = NULL){
  par(mfrow = c(2,2))
  
  plot_depth_attenuation(trios)
  plot_downwelling_irradiance(trios)
  plot_attenuation_coefficient(trios)
  plot_reflectance(trios)
  
  if(!is.null(title)){
    mtext(title, side = 3, line = -2, outer = TRUE)
  }
  
  par(mfrow = c(1,1))
}