#' @export
attenuation_coefficient <- function(trios){
  
  if (length(trios) == 2){
    return(log(trios$up[,2]) - log(trios$up[,1]))
  }
  
  df <- trios_to_df(trios)
  
  df <- subset(df, sensor == "up")
  
  df$fw <- factor(df$w) # Need to have wavelength as factor
  m <- lme4::lmer(log(irradiance) ~ depth + (depth | fw), data=df)
  
  # Slope for each wavelength = fixed effect + random effect
  # Negative slope is attenuation coefficient as function of wavelength
  
  wl <- as.numeric(rownames(ranef(m)$fw))
  -(fixef(m)[2] + ranef(m)$fw[, 2])
}