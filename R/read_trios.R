#' Read a set of Trios files
#' 
#' @param path Path to parent folder of the Trios data
#'
#' @param depths Sample depths for Trios measurements. This needs to have the same number of elements as there are files per sensor.
#' @param sensor_air Name of the air sensor
#' @param sensor_up Name of the upwards facing sensor
#' @param sensor_down Name of the downward facing sensor
#' @param sensor_down_alt Alternative name of downward facing sensor, in case there are multiple
#' 
#' @return A trios object, containing sensors "air", "up" and "down", and wavelengths "w" and depth "depth"
#'
#' @export
read_trios <- function(path, 
                       depths,
                       sensor_air = "8175",
                       sensor_up = "8078",
                       sensor_down = "501A",
                       sensor_down_alt = "817c"){
  
  trios_long <- trios_process(path, depths)
  return(trios_sync_sensors(trios_long, depths))
}