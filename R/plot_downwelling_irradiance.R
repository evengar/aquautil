#' @export
plot_downwelling_irradiance <- function(trios, ...){
  matplot(trios$w, 
          trios$down, 
          type = "l",
          lty=1, 
          col=topo.colors(length(trios)), 
          lwd=3, 
          main="Downwelling irradiance", 
          xlab="Wavelength (nm)", 
          ylab="Power (µW / m2 / nm)",
          ...)
}