#' @title Number of new adaptive speciation events
#'
#' @description This function calculates the number of new adaptive speciation events
#' (Equation 7 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Sr_tm1 Species richness of the island of the previous time step
#' @param N_t Empty niches
#' @param S0 The per species probability of adaptive speciation at maximum empty niche space
#'
#' @return Number of new adaptive speciation events
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
#' Lake expansion increases equilibrium diversity via the target effect of island biogeography.
#'
#' @export aS_t
aS_t <- function(Sr_tm1, N_t, S0){
  # Failback like in Borregaard
  if(N_t < 0){
    N_T <- 0
  }
  # Allow infinite niche space
  if(is.finite(N_t)){
    Res <- Sr_tm1 * N_t * S0
  }
  else{
    Res <- Sr_tm1 * S0
  }
  return(Res)
}
