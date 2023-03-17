#' @export
attenuation_coefficient <- function(trios){
  
  # special case when only 2 depths are recorded
  if (length(trios) == 2){
    range <- diff(trios$depth)
    difference <- log(trios$up[,1]) - log(trios$up[,2])
    # convert slope to per meter
    return(difference / range)
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