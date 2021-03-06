#' @title Island diversity and events through time
#'
#' @description This function calculates the the number of colonization,
#' speciation, extinction, and emmigration events through the trajectory
#' of island evolution. This results in the total diversity of endemics
#' and non-endemics at each moment in time.
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
#' @param EnvirFilt FALSE (default) No effect of island elevation as proxy for habitat diversity on the immigration probability
#'
#' @details Using the parameters in Table 1 of Borregard et al. (2016) with the
#' \code{\link{Island}} dataset results in slightly different diversity trajectories
#' than Borregard's Figure 4a. The example below recreates this figure as closely as possible,
#' but uses a different size of the source area (Ma) and probability of anagenesis (Ana).
#' Reasons for the differences could be the manually digitalized properties of the
#' island itself because Figure S3 (Borregard et al. 2016) containes no y-axis scale.
#' Moreover, the original R script of Borregard et al. (2016) hard-codes many parameters.
#'
#' Rates can be plotted in units of events per time (as in Borregard et al. 2016)
#' or in units of events per lineage per time (as in many phylogenetic studies).
#' This largely removes the hump-shaped extinction trajectory.
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
#' Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
#' Lake expansion increases equilibrium diversity via the target effect of island biogeography
#'
#'@examples
#' # Reproduce Figure 4a (Borregaard et al., 2016)
#' data(Island)
#'
#' Dtt <- diversity_through_time(Elevation = Island$Elevation,
#'                               Topography = Island$Topography,
#'                               Area = Island$Area,
#'                               Vs = 0.0005,
#'                               S0 = 0.0025,
#'                               E0 = 0.001,
#'                               Ms = 500,
#'                               Iso_t = 0.3,
#'                               Imm = 0.002,
#'                               C0 = 0.5,
#'                               Ana = 0.0003,
#'                               Emi = 0.0002,
#'                               Z = 0.25,
#'                               Ma = 200,
#'                               Msa = 0.3,
#'                               DivDep = TRUE,
#'                               TargetEffect = FALSE,
#'                               EnvirFilt = FALSE)
#'
#' ColRich <- rgb(170, 99, 42, maxColorValue = 255)
#' ColNonEnd <- rgb(249, 195, 91, maxColorValue = 255)
#' ColEnd <- rgb(191, 11, 189, maxColorValue = 255)
#' ColEx <- rgb(211, 0, 0, maxColorValue = 255)
#' ColAna <- rgb(255, 144, 235, maxColorValue = 255)
#' ColClado <- rgb(64, 197, 253, maxColorValue = 255)
#'
#' # Diversities
#' par(las = 1, mar = c(4, 6, 0.1, 0.5))
#' plot(1:nrow(Dtt), Dtt[, "Kmax"], type = "l", col = "black",
#'      xlim = c(-1, 5000), ylim = c(0, 160),
#'      xaxs = "i", yaxs = "i",
#'      xaxt = "n", xlab = "Time (Ma)",
#'      ylab = "Species")
#' axis(side = 1, at = c(0, 1000, 2000, 3000, 4000, 5000), labels = c(5, 4, 3, 2, 1, 0))
#' lines(1:nrow(Dtt), Dtt[, "Richness"], type = "l", col = ColRich)
#' lines(1:nrow(Dtt), Dtt[, "NonEndemics"], type = "l", col = ColNonEnd)
#' lines(1:nrow(Dtt), Dtt[, "Endemics"], type = "l", col = ColEnd)
#' legend("topright",
#'        legend = c("Carrying capacity", "Total species richness", "Non-endemics", "Endemics"),
#'        col = c("black", ColRich, ColNonEnd, ColEnd), lty = 1, bty = "n", cex = 0.7)
#'
#' # Rates
#' plot(1:nrow(Dtt),  Dtt[, "Immigrants"], type = "l", col = ColNonEnd,
#'      xlim = c(-1, 5000), ylim = c(0, 0.15),
#'      xaxs = "i", yaxs = "i",
#'      xaxt = "n", xlab = "Time (Ma)",
#'      ylab = expression(atop(paste("Rate"), "(events"%.%"time step"^-1*")")))
#' axis(side = 1, at = c(0, 1000, 2000, 3000, 4000, 5000), labels = c(5, 4, 3, 2, 1, 0))
#' lines(1:nrow(Dtt), Dtt[, "Extinctions"], col = ColEx)
#' lines(1:nrow(Dtt), Dtt[, "NewViaAna"], col = ColAna)
#' lines(1:nrow(Dtt), Dtt[, "NewViaClado"], type = "l", col = ColClado)
#' legend("topright",
#' legend = c("Colonization", "Extinction", "Anagenesis", "Cladogenesis"),
#' col = c(ColNonEnd, ColEx, ColAna, ColClado), lty = 1, bty = "n", cex = 0.7)
#'
#' # Divide by island richness and multiply by 1000
#' # to obtain rates in units of events per island species per 1 million years
#' plot(1:nrow(Dtt),  1000 * Dtt[, "Immigrants"] / Dtt[, "Richness"], type = "l", col = ColNonEnd,
#' xlim = c(-1, 5000), ylim = c(0, 1.3),
#' xaxs = "i", yaxs = "i",
#' xaxt = "n", xlab = "Time (Ma)",
#' ylab = expression(atop(paste("Rate"), "(events"%.%"species"^-1%.%"my"^-1*")")))
#' axis(side = 1, at = c(0, 1000, 2000, 3000, 4000, 5000), labels = c(5, 4, 3, 2, 1, 0))
#' lines(1:nrow(Dtt), 1000 * Dtt[, "Extinctions"] / Dtt[, "Richness"], col = ColEx)
#' lines(1:nrow(Dtt), 1000 * Dtt[, "NewViaAna"] / Dtt[, "Richness"], col = ColAna)
#' lines(1:nrow(Dtt), 1000 * Dtt[, "NewViaClado"] / Dtt[, "Richness"], type = "l", col = ColClado)
#' legend("topright",
#'        legend = c("Colonization", "Extinction", "Anagenesis", "Cladogenesis"),
#'        col = c(ColNonEnd, ColEx, ColAna, ColClado), lty = 1, bty = "n", cex = 0.7)
#'
#' # Figure 4 of Hauffe et al.
#' TimeSteps <- 4000
#' X <- 1:TimeSteps
#' Island <- 1/( 1+exp(-(0.0001 + 0.004*X[1:3000])) )
#' Island <- Island - min(Island)
#' Island <- Island / max(Island)
#' Island <- Island / 2
#' Island <- c(Island, Island[length(Island)] + Island[1:1000])
#' Clado <- 0.00007
#' DttTar <- diversity_through_time(Elevation = Island,
#'                                  Topography = Island,
#'                                  Area = Island,
#'                                  Vs = 0.9 * Clado,
#'                                  S0 = 0.1 * Clado,
#'                                  E0 = 0.00095,
#'                                  Ms = 300,
#'                                  Iso_t = 0.3,
#'                                  Imm = 0.00019,
#'                                  C0 = 1,
#'                                  Ana = 0.00034,
#'                                  Emi = 0,
#'                                  Z = 0.25,
#'                                  Ma = 200,
#'                                  Msa = 0.3,
#'                                  DivDep = FALSE,
#'                                  TargetEffect = TRUE,
#'                                  EnvirFilt = FALSE)
#'
#' # Plot island ontogeny
#' plot(X, Island, type = "l",
#'      ylim = c(0, 1), xlim = c(-1, max(X)),
#'      xaxs = "i", yaxs = "i",
#'      xaxt = "n", xlab = "Time (Ma)",
#'      ylab = "Elevation Area Topography (%)")
#' axis(side = 1, at = c(0, 1000, 2000, 3000, 4000), labels = c(4, 3, 2, 1, 0))
#'
#' # Diversities
#' plot(1:nrow(DttTar), DttTar[, "Richness"], type = "l", col = ColRich,
#' xlim = c(-1, max(X)), ylim = c(0, 80),
#' xaxs = "i", yaxs = "i",
#' xaxt = "n", xlab = "Time (Ma)",
#' ylab = "Species")
#' axis(side = 1, at = c(0, 1000, 2000, 3000, 4000), labels = c(4, 3, 2, 1, 0))
#' lines(1:nrow(DttTar), DttTar[, "NonEndemics"], type = "l", col = ColNonEnd)
#' lines(1:nrow(DttTar), DttTar[, "EndemicsAna"], type = "l", col = ColAna)
#' lines(1:nrow(DttTar), DttTar[, "EndemicsClado"], type = "l", col = ColClado)
#' legend("topleft",
#'        legend = c("Total species richness", "Non-endemics",
#'                   "Endemics evolved via cladogenesis",
#'                   "Endemics via anagenesis"),
#'        col = c(ColRich, ColNonEnd, ColAna, ColClado), lty = 1, bty = "n", cex = 0.7)
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
                                   TargetEffect = FALSE,
                                   EnvirFilt = FALSE){
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
                              TargetEffect = TargetEffect,
                              EnvirFilt = EnvirFilt)
  }
  return(S)
}



