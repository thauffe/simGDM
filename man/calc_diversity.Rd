% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_diversity.R
\name{calc_diversity}
\alias{calc_diversity}
\title{Total island species richness}
\usage{
calc_diversity(Elevation_t, Topography_t, Area_t, Nat_tm1, End_tm1,
  Clado_tm1, Ana_tm1, Vs, S0, E0, Ms, Iso_t, Imm, C0, Ana, Emi, Z, Ma, Msa,
  DivDep = TRUE, TargetEffect = FALSE, EnvirFilt = FALSE)
}
\arguments{
\item{Elevation_t}{Island elevation at one moment in time}

\item{Topography_t}{Island topography at one moment in time}

\item{Area_t}{Island area at one moment in time}

\item{Nat_tm1}{Native richness of the previous time step}

\item{End_tm1}{Endemic richness of the previous time step}

\item{Clado_tm1}{Cladogenetic richness of the previous time step}

\item{Ana_tm1}{Anagenetic richness of the previous time step}

\item{Vs}{Probability of a vicariance event when topography equals 1}

\item{S0}{The per species probability of adaptive speciation at maximum empty niche space}

\item{E0}{The per-species probability of extinction when richness equals Kmax}

\item{Ms}{Species richness    of the mainland    source pool}

\item{Iso_t}{A descriptor of island isolation in arbitrary units}

\item{Imm}{The probability of one species arriving from the mainland per time step}

\item{C0}{The per-species probability of successful colonization at full empty niche space}

\item{Ana}{The per-species probability of anagenesis per time step}

\item{Emi}{Probability of recolonization of the source area per time step}

\item{Z}{The exponent of the species-area relationship between the island and the source area}

\item{Ma}{The realized size of the source area that provides new species}

\item{Msa}{Power exponent controlling the abundance distribution of the source}

\item{DivDep}{TRUE (default) Diversity dependence acting on immigration, extinction and in-situ speciation}

\item{TargetEffect}{FALSE (default) No effect of island area on the immigration probability}

\item{EnvirFilt}{FALSE (default) No effect of island elevation as proxy for habitat diversity on the immigration probability}
}
\value{
The output is a dataframe containing diversity and rates per time step.
\item{Richness}{ Native species richness}
\item{NonEndemics}{ Non-endemic species richness}
\item{Endemics}{ Endemic species richness}
\item{Kmax}{ Carrying capacity}
\item{EndemicsClado}{ Endemic species evolved via in-situ cladogenesis}
\item{EndemicsAna}{ Endemic species evolved via in-situ anagenesis}
\item{Immigrants}{ Species immigrating at that time step to the island}
\item{NewViaClado}{ Species evolved via in-situ cladogenesis at that time step}
\item{NewViaNonadaptClado}{ Species evolved via non-adaptive in-situ cladogenesis at that time step}
\item{NewViaAdaptClado}{ Species evolved via adaptive in-situ cladogenesis at that time step}
\item{NewViaAna}{ Species evolved via in-situ anagenesis at that time step}
\item{Extinctions}{ Species going extinct at that time step}
\item{Emigrants}{ Species emigrating from the island to the mainland}
}
\description{
This function calculates the total island species richness
(Equation 12 in Appendix 2 of Borregaard et al. 2016)
}
\references{
Borregaard, M. K., T. J. Matthews and R. J. Whittaker (2016).
The general dynamic model: towards a unified theory of island biogeography?
Global Ecology and Biogeography, 25(7), 805-816.

Hauffe, T., D. Delicado, R.S. Etienne and L. Valente (submitted).
Lake expansion increases equilibrium diversity via the target effect of island biogeography
}
\author{
Torsten Hauffe
}
