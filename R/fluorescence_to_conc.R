#' Convert fluorescence values from a plate to concentrations of your target substance based on a series of standards.
#' 
#' Made specifically for quantifying DNA with picogreen, but can probably
#' also be used for other plate reader applications
#'
#' @param plate A data.frame formatted as a well plate, with names A:H in the first column, 
#' and the subsequent 12 columns containing fluorescence reads. The rest of the columns will be ignored. This is compatible with a single
#' plate from the plate reader at AQUA read from an excel file.
#' @param std Either a data frames with two columns named fluorescence and conc_ng_ul, or a
#' named vector containing concentrations and the names of the wells containing the respective concentrations.
#' @param sample_volume The volume of sample added to each well 
#' @param well_volume The total volume of the well
#'
#' @return A data frame in long format with concentrations for each well
#' @export
fluorescence_to_conc <- function(plate, std, sample_volume, well_volume = 200){
  
  names(plate)[1] <- "row"
  
  if(!(all(plate$row == LETTERS[1:8]))){
    stop("First column has to contain row names A:H")
  }
  
  if(ncol(plate) > 13){
    plate <- plate[-(14:ncol(plate))]
  }
  

  
  plate_long <- plate %>%
    pivot_longer(-row, names_to = "column", values_to = "fluorescence") %>%
    mutate(well = paste0(row, column)) %>%
    na.omit()
  
  if(is.numeric(std) & !is.null(names(std))){
    std_table <- data.frame(
      well = names(std),
      conc_ng_ul = unname(std)
    )
    
    std <- left_join(std_table, plate_long, by = "well")
  }
  
  plate_long <- plate_long %>%
    mutate(fluorescence = fluorescence - min(std$fluorescence))
  std$fluorescence <- std$fluorescence - min(std$fluorescence)
  
  std_mod <- lm(conc_ng_ul ~ (-1) + fluorescence, data = std)
  conc_in_well <- predict(std_mod, newdata = plate_long)
  conc <- conc_in_well * well_volume / sample_volume
  conc_data <- bind_cols(plate_long, conc_ng_ul = conc) %>%
    arrange(as.numeric(column), row)
  return(conc_data)
}