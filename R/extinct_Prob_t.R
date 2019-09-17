#' @title Probability for a species to go extinct in a time step
#'
#' @description This function calculates the probability for a species to go extinct in a time step
#' (Equation 8 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param N_t Empty niches
#' @param E0 The per-species probability of extinction when richness equals Kmax
#'
#' @return Probability for a species to go extinct in a time step
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
#' Lake expansion increases equilibrium diversity via the target effect of island biogeography
#'
#' @export extinctProb_t
extinctProb_t <- function(N_t, E0){
  # Allow infinite niche space
  if(is.finite(N_t)){
    if(N_t < 0){
      N_t <- 0
    }
    Res <- (1 - N_t) * E0
  }
  else{
    Res <- E0
  }
  return(Res)
}
