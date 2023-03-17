#' @export
plot_depth_attenuation <- function(trios, relative_to_surface = TRUE, ...){
  
  trios <- normalize_to_surface(subset_to_wavelength(trios), 
                                relative_to_surface = relative_to_surface)
  matplot(trios$depth, 
          t(trios$down), 
          type = "l", 
          log="y", 
          lty=1, 
          col=trios_palette(length(trios$w)),
          main = "Attenuation by depth",
          xlab="Depth (m)", 
          ylab="Power (µW / m2 / nm)",
          ...)
  
}