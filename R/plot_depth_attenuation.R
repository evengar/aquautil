#' @export
plot_depth_attenuation <- function(trios){
  # trios_df <- subset(as.data.frame(trios), sensor == "up")
  trios <- normalize_to_surface(subset_to_wavelength(trios))
  matplot(trios$depth, 
          t(trios$up), 
          type = "l", 
          log="y", 
          lty=1, 
          col=trios_palette(length(trios$w)),
          xlab="Depth(m)", 
          ylab="Power (µW / m2 / nm)")
  
}