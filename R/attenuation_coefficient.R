#' @export
attenuation_coefficient <- function(trios){
  
  # special case when only 2 depths are recorded
  if (length(trios) == 2){
    range <- diff(trios$depth)
    difference <- log(trios$up[,1]) - log(trios$up[,2])
    # convert slope to per meter
    return(data.frame(w = trios$w, attenuation = difference / range))
  }
    
  df <- as.data.frame(trios)
  
  # omit 0 values because we use log later
  df <- na.omit(subset(df, sensor == "up" & irradiance > 0))

  
  df$fw <- factor(df$w) # Need to have wavelength as factor
  m <- lme4::lmer(log(irradiance) ~ depth + (depth | fw), data=df)
  
  # Slope for each wavelength = fixed effect + random effect
  # Negative slope is attenuation coefficient as function of wavelength
  
  wl <- as.numeric(rownames(lme4::ranef(m)$fw))
  attenuation <- -(lme4::fixef(m)[2] + lme4::ranef(m)$fw[, 2])
  
  data.frame(w = wl, attenuation = attenuation)
}