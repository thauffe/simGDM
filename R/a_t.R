#' @title Number of species evolved via anagenesis
#'
#' @description This function calculates the number of species evolved via anagenesis
#' (Equation 4 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Ana The per-species probability of anagenesis per time step
#' @param Nat_tm1 Number of natives of the previous time step
#' @param NewArrivals_t Number of newly immigrating species
#' @param C0 The per-species probability of successful colonization at full empty niche space
#' @param N_t Empty niches
#'
#' @return Number of species evolved via anagenesis
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
#' @export a_t
a_t <- function(Ana, Nat_tm1, NewArrivals_t, C0, N_t){
  if(is.finite(N_t)){
    Res <- Ana * ( Nat_tm1 - NewArrivals_t * (1 - C0 * N_t))
  }
  else{
    Res <- Ana * (Nat_tm1 - NewArrivals_t * (1 - C0))
  }
  # Failback of Borregarrd script
  if (Res < 0){
    Res <- 0
  }
  return(Res)
}
