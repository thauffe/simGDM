#' @title Endemic richness
#'
#' @description This function calculates the endemic richness
#' (Equation 12 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param ExtinctProb_t Probability for a species to go extinct in a time step
#' @param End_tm1 Endemic richness of the previous time step
#' @param A_t Number of species evolved via anagenesis
#' @param NaS_t Number of new non-adaptive speciation events
#' @param AS_t Number of new adaptive speciation events
#'
#' @return Endemic richness
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export end_t
end_t <- function(ExtinctProb_t, End_tm1, A_t, Em_t, NaS_t, AS_t){
  Res <- (1 - ExtinctProb_t) * End_tm1 + A_t + NaS_t + AS_t - Em_t
  return(Res)
}
