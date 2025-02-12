#' Get volume for pooling in equal concentrations
#' 
#'
#' @param plates A data frame in long format, with concentrations for each well named conc_ng_ul,
#' typically the output of fluorescence_to_conc. Can contain multiple plates in a single data frame.
#' 
#' @param max_vol The maximum volume to use from a single sample. For example the total volume of your sample - 5.
#' @param blank_threshold Maximum concentration for something to be considered a "blank", which will be pooled with a fixed volume
#' @param blank_vol Fixed volume to use for samples with concentrations below blank_threshold
#'
#' @return The input data frame with an added column pool_vol
#' @export

calc_pool_vol <- function(plates, max_vol, blank_threshold, blank_vol = 10){
  min_conc <- min(plates$conc_ng_ul[plates$conc_ng_ul > blank_threshold])
  target_ng = min_conc*max_vol
  plates %>%
    mutate(
      pool_vol = ifelse(conc_ng_ul > blank_threshold,
                        target_ng/conc_ng_ul,
                        blank_vol)
    )
}