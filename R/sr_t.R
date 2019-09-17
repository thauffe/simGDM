#' @title Total island species richness
#'
#' @description This function calculates the total island species richness
#' (Equation 12 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Nat_t Native richness
#' @param End_t Endemic richness
#'
#' @return Total island species richness
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export sr_t
sr_t <- function(Nat_t, End_t){
  Res <- Nat_t + End_t
  return(Res)
}
