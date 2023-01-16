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