#' @title Number of new successfully colonizing species
#'
#' @description This function calculates the number of new successfully colonizing species
#' (Equation 3 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param NewArrivals_t Number of newly immigrating species
#' @param C0 The per-species probability of successful colonization at full empty niche space
#' @param N_t Empty niches
#'
#' @return Number of new successfully colonizing species
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export newArrivals_t
newColonizers_t <- function(NewArrivals_t, C0, N_t){
  if(is.finite(N_t)){
    Res <- NewArrivals_t * C0 * N_t
  }
  else{
    Res <- NewArrivals_t * C0
  }
  return(Res)
}
