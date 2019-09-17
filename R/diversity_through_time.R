#' @title Island diversity and events through time
#'
#' @description This function calculates the the number of colonization, speciation, extinction,
#' and emmigration events through the trajectory of island evolution. This results in the total diversity
#' of endemics and non-endemics at each moment in time.
#'
#' @param Elevation Vector of island elevation through time
#' @param Topography Vector of island topography through time
#' @param Area Vector of island area through time
#' @param Vs Probability of a vicariance event when topography equals 1
#' @param S0 The per species probability of adaptive speciation at maximum empty niche space
#' @param E0 The per-species probability of extinction when richness equals Kmax
#' @param Ms Species richness	of the mainland	source pool
#' @param Iso_t A descriptor of island isolation in arbitrary units
#' @param Imm The probability of one species arriving from the mainland per time step
#' @param C0 The per-species probability of successful colonization at full empty niche space
#' @param Ana The per-species probability of anagenesis per time step
#' @param Emi Probability of recolonization of the source area per time step
#' @param Z The exponent of the species-area relationship between the island and the source area
#' @param Ma The realized size of the source area that provides new species
#' @param Msa Power exponent controlling the abundance distribution of the source
#' @param DivDep TRUE (default) Diversity dependence acting on immigration, extinction and in-situ speciation
#' @param TargetEffect FALSE (default) No effect of island area on the immigration probability
#'
#' @return The output is a dataframe containing diversity and rates per time step.
#' \item{Richness}{ Native species richness}
#' \item{NonEndemics}{ Non-endemic species richness}
#' \item{Endemics}{ Endemic species richness}
#' \item{Kmax}{ Carrying capacity}
#' \item{EndemicsClado}{ Endemic species evolved via in-situ cladogenesis}
#' \item{EndemicsAna}{ Endemic species evolved via in-situ anagenesis}
#' \item{Immigrants}{ Species immigrating at that time step to the island}
#' \item{NewViaClado}{ Species evolved via in-situ cladogenesis at that time step}
#' \item{NewViaNonadaptClado}{ Species evolved via non-adaptive in-situ cladogenesis at that time step}
#' \item{NewViaAdaptClado}{ Species evolved via adaptive in-situ cladogenesis at that time step}
#' \item{NewViaAna}{ Species evolved via in-situ anagenesis at that time step}
#' \item{Extinctions}{ Species going extinct at that time step}
#' \item{Emigrants}{ Species emigrating from the island to the mainland}
#'
#' @author Torsten Hauffe
#'
#' @references Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
#' The general dynamic model: towards a unified theory of island biogeography?
#' Global Ecology and Biogeography, 25(7), 805-816.
#'
#'@examples
#' TimeSteps <- 4000
#' X <- 1:TimeSteps
#' Island <- 1/( 1+exp(-(0.0001 + 0.004*X[1:3000])) )
#' Island <- Island - min(Island)
#' Island <- Island / max(Island)
#' Island <- Island / 2
#' Island <- c(Island, Island[length(Island)] + Island[1:1000])
#' Clado <- 0.00007
#' Dtt <- diversity_through_time(Elevation = Island,
#'                               Topography = Island,
#'                               Area = Island,
#'                               Vs = 0.9 * Clado,
#'                               S0 = 0.1 * Clado,
#'                               E0 = 0.00095,
#'                               Ms = 300,
#'                               Iso_t = 0.3,
#'                               Imm = 0.00019,
#'                               C0 = 1,
#'                               Ana = 0.00034,
#'                               Emi = 0,
#'                               Z = 0.25,
#'                               Ma = 200,
#'                               Msa = 0.3,
#'                               DivDep = FALSE,
#'                               TargetEffect = TRUE)
#'
#' @export diversity_through_time

diversity_through_time <- function(Elevation,
                                   Topography,
                                   Area,
                                   Vs,
                                   S0,
                                   E0,
                                   Ms,
                                   Iso_t,
                                   Imm,
                                   C0,
                                   Ana,
                                   Emi,
                                   Z,
                                   Ma,
                                   Msa,
                                   DivDep = TRUE,
                                   TargetEffect = FALSE){
  S <- as.data.frame(matrix(NA_real_, nrow = length(Elevation), ncol = 13))
  colnames(S) <- c("Richness", "NonEndemics", "Endemics", "Kmax", "EndemicsClado", "EndemicsAna",
                   "Immigrants", "NewViaClado", "NewViaNonadaptClado", "NewViaAdaptClado",
                   "NewViaAna", "Extinctions", "Emigrants")
  S[1, ] <- 0
  for(i in 2:length(Elevation)){
    S[i, ] <-  calc_diversity(Elevation_t = Elevation[i-1],
                              Topography_t = Topography[i-1],
                              Area_t = Area[i-1],
                              Nat_tm1 = S[i-1, 2],
                              End_tm1 = S[i-1, 3],
                              Clado_tm1 = S[i-1, 5],
                              Ana_tm1 = S[i-1, 6],
                              Vs = Vs,
                              S0 = S0,
                              E0 = E0,
                              Ms = Ms,
                              Iso_t = Iso_t,
                              Imm = Imm,
                              C0 = C0,
                              Ana = Ana,
                              Emi = Emi,
                              Z = Z,
                              Ma = Ma,
                              Msa = Msa,
                              DivDep = DivDep,
                              TargetEffect = TargetEffect)
  }
  return(S)
}



