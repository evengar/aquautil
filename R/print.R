#' @export
print.trios <- function(trios){
  cat(paste0("Trios object with ", length(trios), "depths:"), "\n")
  cat(trios$depth, "meters", "\n")
  cat("", "\n")
  
  cat(paste0("and ", length(trios$w), " wavelengths, ranging from"), "\n")
  cat(paste0(round(range(trios$w)[1], 1), " to ", round(range(trios$w)[2], 2), " nm"))
  
  invisible(trios)
}