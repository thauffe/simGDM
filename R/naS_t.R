#' @title Number of new non-adaptive speciation events
#'
#' @description This function calculates the number of new non-adaptive speciation events
#' (Equation 6 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Sr_tm1 Species richness of the island of the previous time step
#' @param Topography_t Topography
#' @param Vs Probability of a vicariance event when topography equals 1
#'
#' @return Number of new non-adaptive speciation events
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export naS_t
naS_t <- function(Sr_tm1, Topography_t, Vs){
  Res <- Sr_tm1 * Topography_t * Vs
  return(Res)
}
