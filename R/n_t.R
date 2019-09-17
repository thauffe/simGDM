#' @title Number of empty niches
#'
#' @description This function calculates the number of empty niches
#' (Equation 10 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Kmax_t Carrying capacity
#' @param Sr_t Total island species richness
#' @param Ms Species richness	of the mainland	source pool
#'
#' @return Number of empty niches
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#' @export n_t
n_t <- function(Kmax_t, Sr_t, Ms){
  if(Sr_t > Kmax_t){
    Res <- 0
  }
  else{
    Res <- (Kmax_t - Sr_t) / Ms
  }
  return(Res)
}
