#' Plot well-plates with a fill-value for each well
#'
#' @param plates A data frame in long format, with wells and either concentrations or pool volumes,
#' typically the output of fluorescence_to_conc or calc_pool_vol. If the data frame contains a column
#' "plate", the resulting plot will be faceted by plate.
#' 
#' @param fill_var The variable to use for fill and labeling, e.g. conc_ng_ul or pool_vol
#' @param round_dec Number of decimals to use for rounding of the labels
#'
#' @return A ggplot object, plotting the plate with its physical layout,
#'  and geom_tile and geom_text for the variable of interest.
#' @export
 
plot_plate <- function(plate, fill_var = conc_ng_ul, round_dec = 2){
  fill_var = enquo(fill_var)
  
  baseplot <- ggplot(pool, aes(as.factor(as.numeric(column)), fct_rev(row)))  +
    geom_tile(aes(fill = !!fill_var)) +
    geom_text(aes(label = round(!!fill_var, round_dec)), col = "red") +
    scale_fill_viridis_c()
  
  if("plate" %in% names(plate)){
    return(baseplot + facet_wrap(~plate, nrow = length(unique(plate$plate))))
  }
  return(baseplot)
}