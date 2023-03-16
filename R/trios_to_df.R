#' Convert a trios object to a data frame in long format
#'
#' @param trios 
#'
#' @return A data frame in long form with columns for depth, wavelength, sensor type and irradiance value
#' @export
#'
trios_to_df <- function(trios){
  
  with(trios, data.frame(depth=rep(depth, each=length(w)), 
                         w=rep(w, length(depth)),
                         sensor = factor(rep(c("up", "down", "air"), each = length(w) * length(depth))),
                         irradiance = c(stack(as.data.frame(up))[,1],
                                         stack(as.data.frame(down))[,1],
                                         stack(as.data.frame(air))[,1])
                       )
     )
}