#' Title
#'
#' @param path 
#' @param depths 
#' @param sensor_air 
#' @param sensor_up 
#' @param sensor_down 
#' @param sensor_down_alt 
#'
#' @return
#' @export
#'
#' @examples
read_trios <- function(path, 
                       depths,
                       sensor_air = "8175",
                       sensor_up = "8078",
                       sensor_down = "501A",
                       sensor_down_alt = "817c"){
  
  trios_long <- trios_process(path, depths)
  return(trios_sync_sensors(trios_long, depths))
}