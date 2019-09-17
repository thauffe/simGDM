#' @title Native richness
#'
#' @description This function calculates the native richness
#' (Equation 11 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param ExtinctProb_t Probability for a species to go extinct in a time step
#' @param Nat_tm1 Species richness of the island of the previous time step
#' @param NewColonizers_t Number of new successfully colonizing species
#' @param A_t Number of species evolved via anagenesis
#' @param Em_t Number of emigration events
#'
#' @return Native richness
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export nat_t
nat_t <- function(ExtinctProb_t, Nat_tm1, NewColonizers_t, A_t, Em_t){
  Res <- (1 - ExtinctProb_t) * Nat_tm1 + NewColonizers_t - A_t + Em_t
  return(Res)
}
