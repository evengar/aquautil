#' @export

trios_process <- function(path, depths){
  
  f <- dir(path)
  # remove any calibration files that may be present
  f <- f[!grepl("CAL", f)]
  
  # sensor ID comes after "SAM_" or "SAMIP_"
  sensor <- factor(gsub(".*SAM(IP)?_(.{4})_.*", "\\2", f))
  # sensor_table = table(sensor)
  # stopifnot(all(sensor_table[1]==sensor_table[2:length(sensor_table)]))

  # Get date and time by pattern matching
  dt_string <- gsub(".*_([0-9]{4}-[0-9]{2}-[0-9]{2}_[^_]*)_.*", "\\1", f)
  time <- as.POSIXct(strptime(dt_string, "%Y-%m-%d_%H-%M-%S"))
  
  ff <- data.frame(f, sensor, time)
  ff <- ff[order(sensor, time), ]
  ff$depth <- rep(depths, 3)
  
  # Aggregate all files into one big table
  trios <- NULL
  for (i in (1:length(f))) {
    # Concatenate path + file name
    fi <- paste(path, ff$f[i], sep = "/")
    
    # Read all lines as strings (no tabs in file)
    di <- read.table(fi, sep = "\t", as.is = TRUE)$V1
    # Get pressure from file
    p <- NA
    pressure_pos <- grep("Pressure", di)
    if (length(pressure_pos) > 0){
      p <- strsplit(di[pressure_pos], " = ")[[1]][2]
      #browser()
    }
    
    
    # Use occurrences of "DATA" to find
    # beginning and end of data part of the file
    k <- grep("DATA", di)
    di <- di[(k[1]+2):(k[2]-1)]
    
    #browser()
    
    # Split data strings and extract wavelength and power
    mi <- matrix(unlist(strsplit(di, " ")), nrow = 5)
    wi <- as.numeric(mi[2,])
    Pi <- as.numeric(mi[3,])
    
    # Add to aggregated data table
    trios <- rbind(
      trios,
      data.frame(
        sensor = ff$sensor[i], 
        depth = ff$depth[i], 
        wavelength = wi,
        power = Pi,
        pressure = p)
                   )
  }
  
  stopifnot("Increasing depth does not correspond to increasing pressure. Are the depths given in the wrong order?" = 
    all(order(na.omit(unique(trios$depth))) == order(na.omit(unique(trios$pressure)))))
  
  pressures <- as.numeric(na.omit(unique(trios$pressure)))
  pressures_inorder <- pressures[order(pressures)]
  
  depths_inorder <- depths[order(depths)]
  depth_pressure <- data.frame(depth = depths_inorder, pressure2 = pressures_inorder)
  #browser()
  
  trios_merged <- merge(trios, depth_pressure)
  
  return(trios_merged)
}