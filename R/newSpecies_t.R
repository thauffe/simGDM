#' @title Probability that an immigrant species is not already on the island
#'
#' @description This function calculates the probability	that an immigrant species is not already on the island
#' (Equation 1 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Nat_tm1 Island natives of the previous time step
#' @param Ms Species richness	of the mainland	source pool
#' @param Msa Power exponent controlling the abundance distribution of the source
#'
#' @return Probability that an immigrant species is not already on the island
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M.K., T.J. Matthews and R.J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export newSpecies_t

newSpecies_t <- function(Nat_tm1, Ms, Msa){
  Res <- 1 - (Nat_tm1 / Ms)^Msa
  # Failback of Borregarrd script
  if(is.na(Res) | Res < 0){
    Res <- 0
  }
  return(Res)
}
