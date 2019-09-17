#' @title Number of emigration events
#'
#' @description This function calculates the number of emigration events
#' (Equation 5 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Ana The per-species probability of anagenesis per time step
#' @param Nat_tm1 Number of natives of the previous time step
#' @param NewArrivals_t Number of newly immigrating species
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
