#' @export
trios_palette <- function(n){
  Rainbow <- colorRampPalette(c("violet", "blue1", "green", "yellow", "orange", "red", "darkred"))
  Rainbow(n)
}