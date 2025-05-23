#' Sync the wavelengths of Trios sensors and make into Trios object
#'
#' @inheritParams read_trios
#' 
#' @inherit read_trios return
#' @export

trios_sync_sensors <- function(trios_long,
                               sensor_air = "8175",
                               sensor_up = "8078",
                               sensor_down = "501A",
                               sensor_down_alt = "817c"
                                 ){

  d.air  <- subset(trios_long, sensor == sensor_air)
  d.up   <- subset(trios_long, sensor == sensor_up)
  d.down <- subset(trios_long, sensor == sensor_down | sensor == sensor_down_alt)
  
  depths <- unique(d.air$depth)
  pressures <- unique(d.air$pressure2)
  #browser()
  # Make separate wavelength by depth matrices for the 3 sensors 
  # (air reference, water downwelling, and water upwelling)
  
  p.air <- tapply(d.air$power, list(wavelength=d.air$wavelength, depth=d.air$depth), sum)
  w.air <- as.numeric(rownames(p.air))
  # matplot(w.air, p.air, type="l", lty=1, col=topo.colors(10), lwd=3, main="Air", 
  #         xlab="Wavelength (nm)", ylab="Power (?W / m2 / nm)", xlim=c(350, 750))
  # 
  p.down <- tapply(d.down$power, list(wavelength=d.down$wavelength, depth=d.down$depth), sum)
  w.down <- as.numeric(rownames(p.down))
  # matplot(w.down, p.down, type="l", lty=1, col=topo.colors(10), lwd=3, main="Downwelling",
  #         xlab="Wavelength (nm)", ylab="Power (?W / m2 / nm)", xlim=c(350, 750))
  # 
  p.up <- tapply(d.up$power, list(wavelength=d.up$wavelength, depth=d.up$depth), sum)
  w.up <- as.numeric(rownames(p.up))
  # matplot(w.up, p.up, type="l", lty=1, col=topo.colors(10), lwd=3, main="Upwelling",
  #         xlab="Wavelength (nm)", ylab="Power (?W / m2 / nm)", xlim=c(350, 750))
  # 
  # Interpolate all spectra to the same wavelengths
  
  w <- w.air
  for (i in (1:length(depths))) {
    p.up[, i] <- approx(w.up, p.up[, i], w)$y
    p.down[, i] <- approx(w.down, p.down[, i], w)$y
  }

  dimnames(p.air) <- dimnames(p.up) <- dimnames(p.down) <- NULL  
  
  
  trios <- list(air = p.air, up = p.up, down = p.down, w = w, depth = depths, p = pressures)
  
  structure(trios, class = c("trios", "list"))
}
