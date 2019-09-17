#' @title Carrying capacity of the island
#'
#' @description This function calculates the carrying capacity of the island
#' (Equation 9 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Ms Species richness	of the mainland	source pool
#' @param Elevation_t Elevation
#' @param Topography_t Topography
#' @param Area_t Area
#' @param Z The exponent of the species-area relationship between the island and the source area
#' @param Ma The realized size of the source area that provides new species
#'
#' @return Carrying capacity of the island
#'
#' @author Torsten Hauffe
#'
#' @seealso \code{\link{calc_diversity}}
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export kmax_t
kmax_t <- function(Ms, Elevation_t, Topography_t, Area_t, Z, Ma){
  Res <- Ms * ( (Elevation_t + Topography_t + Area_t) / (3 * Ma) )^Z
  return(Res)
}
