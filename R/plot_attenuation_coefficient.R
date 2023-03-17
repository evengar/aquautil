#' @export
plot_attenuation_coefficient <- function(trios){

  attenuation_df <- attenuation_coefficient(trios)
  plot(attenuation ~ w, data = attenuation_df,
       main = "Attenuation coefficient",
       xlab = "Wavelength",
       ylab = "Attenuation coefficient")
}