#' @title Total island species richness
#'
#' @description This function calculates the total island species richness
#' (Equation 12 in Appendix 2 of Borregaard et al. 2016)
#'
#' @param Elevation_t Island elevation at one moment in time
#' @param Topography_t Island topography at one moment in time
#' @param Area_t Island area at one moment in time
#' @param Nat_tm1 Native richness of the previous time step
#' @param End_tm1 Endemic richness of the previous time step
#' @param Clado_tm1 Cladogenetic richness of the previous time step
#' @param Ana_tm1 Anagenetic richness of the previous time step
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
#' Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
#' Lake expansion increases equilibrium diversity via the target effect of island biogeography
#'
#' @export calc diversity
calc_diversity <- function(Elevation_t, Topography_t, Area_t,
                           Nat_tm1, End_tm1, Clado_tm1, Ana_tm1,
                           Vs, S0, E0, Ms, Iso_t, Imm, C0, Ana, Emi, Z, Ma, Msa,
                           DivDep = TRUE, TargetEffect = FALSE){
  # According to Borregaard step()
  ################################
  Sr_tm1 <- sr_t(Nat_t = Nat_tm1, End_t = End_tm1) # Eq. 13 Species richness
  # 1. Kmax and then empty niches
  ###############################
  Kmax_t <- kmax_t(Ms, Elevation_t, Topography_t, Area_t, Z, Ma)
  # Diversity dependent
  if(DivDep){
    N_t <- n_t(Kmax_t, Sr_tm1, Ms) # Eq. 10 empty niches proportional to metacommunity
    if(N_t > 1){
      stop("N_t > 1")
    }
  }
  # Diversity independent
  else{
    N_t <- Inf
  }
  # Avoid overshoot of diversity by limiting diversity to K_max
  # This is not perfect because richness is of the time-step before
  if(DivDep & Sr_tm1 > Kmax_t){
    Nat_tm1 <- Nat_tm1 * (Kmax_t/Sr_tm1)
    End_tm1 <- End_tm1 * (Kmax_t/Sr_tm1)
    Clado_tm1 <- Clado_tm1 * (Kmax_t/Sr_tm1)
    Ana_tm1 <- Ana_tm1 * (Kmax_t/Sr_tm1)
  }
  # 2. non-adaptive & adaptive cladogenesis
  #########################################
  NaS_t <- naS_t(Sr_tm1, Topography_t, Vs) # Eq.6 non-adaptive speciation
  AS_t <- aS_t(Sr_tm1, N_t, S0) # Eq.7 adaptive speciation
  New_cladogenesis <- NaS_t + AS_t # Species evolved via cladogenesis
  # 3. extinction rate
  ####################
  ExtinctProb_t <- extinctProb_t(N_t, E0) # Eq.8 Extinction probability
  Ext <- Sr_tm1 - (1 - ExtinctProb_t) * Sr_tm1
  # 4. Immigration & colonization
  ###############################
  # How many species arrive to the island in the time interval
  NewSpecies_t <- newSpecies_t(Nat_tm1, Ms, Msa) # Eq.1
  if(TargetEffect){
    NewArrivals_t <- newArrivals_t(Imm, Ms, Iso_t, NewSpecies_t, Area_t) # Eq.2
  }
  else{
    NewArrivals_t <- newArrivals_t(Imm, Ms, Iso_t, NewSpecies_t, Area_t = 1) # Eq.2
  }
  NewColonizers_t <- newColonizers_t(NewArrivals_t, C0, N_t) # Eq. 3 establishers
  # 5. Anagenesis
  ################
  A_t <- a_t(Ana, Nat_tm1, NewArrivals_t, C0, N_t) # Eq.4 Anagensis
  # 6. Emigration
  ################
  Em_t <- em_t(End_tm1, Emi, Iso_t) # Eq.5 Emigration
  # Calculate diversities
  #######################
  Nat_t <- nat_t(ExtinctProb_t, Nat_tm1, NewColonizers_t, A_t, Em_t) # Eq.11 native richness
  End_t <- end_t(ExtinctProb_t, End_tm1, A_t, Em_t, NaS_t, AS_t) # Eq.12 endemic richness
  Sr_t <- sr_t(Nat_t, End_t) # Eq. 13 Species richness
  if(End_t == 0){
    Clado_t <- 0
    Ana_t <- 0
  }
  else{
    Clado_t <- (1 - ExtinctProb_t) * Clado_tm1 + NaS_t + AS_t - Em_t * ((Clado_tm1 + NaS_t + AS_t) / End_t)
    Ana_t <- (1 - ExtinctProb_t) * Ana_tm1 + A_t - Em_t * ((Ana_tm1 + A_t) / End_t)
  }
  Div <- c(Sr_t, # Total richness
           Nat_t, # Non-endemic richness
           End_t, # Endemic richness
           Kmax_t, # Carrying capacity
           Clado_t, # Species evolved via cladogenesis
           Ana_t, # Species evolved via anagenesis
           NewColonizers_t, # Species successfully colonizing the island at that time step
           New_cladogenesis, # Species evolved at that time step via cladogenesis
           NaS_t, # Species evolved at that time step via non-adaptive cladogenesis
           AS_t, # Species evolved at that time step via adaptive cladogenesis
           A_t, # Species evolved at that time step via anagenesis
           Ext, # Species going extinct at that time step
           Em_t) # Species emigrating at that time step
  return(Div)
}
