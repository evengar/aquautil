attenuation_coefficient <- function(trios){
  
  
  
  df$fw <- factor(df$w) # Need to have wavelength as factor
  summary(m <- lmer(log(values) ~ z + (z | fw), data=df))
  
  plot(log(df$values), predict(m)) # Good fit :)
  abline(c(0, 1), lty=2)
  
  # Slope for each wavelength = fixed effect + random effect
  # Negative slope is attenuation coefficient as function of wavelength
  
  wl <- as.numeric(rownames(ranef(m)$fw))
  att <- -(fixef(m)[2] + ranef(m)$fw[, 2])
}