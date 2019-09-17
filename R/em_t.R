#' @title Number of emigration events
#'
#' @description This function calculates the number of emigration events
#' (Equation 5 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param End_tm1 Endemic richness of the previous time step
#' @param Emi Probability of recolonization of the source area per time step
#' @param Iso_t A descriptor of island isolation in arbitrary units
#'
#' @return Number of emigration events
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export em_t
em_t <- function(End_tm1, Emi, Iso_t){
  Res <- End_tm1 * (Emi / Iso_t)
  if (Res > End_tm1){ # There cannot be more emigrants than endemic species
    Res <- End_tm1
  }
  return(Res)
}
