## simGDM
### R package to simulate island biodiversity according to the General Dynamic Model of island biogeography ([Whittaker et al. 2008](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2008.01892.x/abstract))

This is a reimplementation of the deterministic biodiversity simulation of 
[Borregaard et al. (2016)](http://onlinelibrary.wiley.com/doi/10.1111/geb.12348/abstract), 
complemented by optional diversity-independent dynamics and the effect of island area 
on the immigration probability ('target effect'; [Gilpin and Diamond 1976](https://www.pnas.org/content/73/11/4130)).


#### Usage
The package can be installed directly from this github repository and its main function `diversity_through_time` 
calculates native species richness, endemic richness, and rates of immigration, emigration, extinction, 
and *in-situ* speciation for given trajectories of island area, topography, and elevation. 

```{r, warning = F, echo = F}
library(devtools)
install_github("thauffe/simGDM")
library(simGDM)
```

The first example provided in the `diversity_through_time` function showes how to recreate the 
diversity and emerging rate trajectories of Borregaard et al. (2016).

![Borregaard 2016 Fig4a](https://github.com/thauffe/simGDM/blob/master/Figures/BorFig4Div.png)

The second example demonstrates how to switch off the diversity-dependence and to add the target effect
on immigration probability. 

The geographical template is an island ontogeny with two distinct periods of expansion.

![Hauffe Fig4b](https://github.com/thauffe/simGDM/blob/master/Figures/HauffeFig4b.png)

High extinction rates and diversity-independence stimulate a source-sink equilibrium diversity 
during the first period of island ontogeny. The island growth in the second period increase 
the colonization rate via the target effect and increase the species richness.

![Hauffe Fig4c](https://github.com/thauffe/simGDM/blob/master/Figures/HauffeFig4c.png)
