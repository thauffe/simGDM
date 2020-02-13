#' @title Number of newly immigrating species
#'
#' @description This function calculates the number of newly immigrating species
#' (Equation 2 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Imm The probability of one species arriving from the mainland per time step
#' @param Ms Species richness	of the mainland	source pool
#' @param Iso_t A descriptor of island isolation in arbitrary units
#' @param NewSpecies_t Probability that an immigrant species is not already on the island
#' @param Area_t Island area at moment t, to simulate the target effect (not included in Borregaard 2016)
#' @param Elevation_t Island elevation at moment t, to simulate the target effect (not included in Borregaard 2016)
#'
#' @return Number of newly immigrating species
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M.K., T.J. Matthews and R.J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
#' Lake expansion increases equilibrium diversity via the target effect of island biogeography
#'
#' @export newArrivals_t

newArrivals_t <- function(Imm, Ms, Iso_t, NewSpecies_t, Area_t = 1, Elevation_t = 1){
  Res <- ( (Imm * Ms) / Iso_t ) * NewSpecies_t * Area_t * Elevation_t
  return(Res)
}
