#' Subset a trios object to certain wavelengths
#'
#' @param trios A trios object
#' @param lower Lower wavelength (non-inclusive)
#' @param upper Upper wavelength (non-inclusive)
#'
#' @return A trios object containing only values between the specified wavelengths
#' @export
#'
subset_to_wavelength <- function(trios, lower = 400, upper = 700){
  w <- trios$w
  w_par <- w[(w > lower) & (w < upper)]

  trios$up <- trios$up[w %in% w_par, ]
  trios$down <- trios$down[w %in% w_par, ]
  trios$air <- trios$air[w %in% w_par, ]
  trios$w <- w_par
  return(trios)
}
