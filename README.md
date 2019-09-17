## simGDM
### R package to simulate island biodiversity according to the General Dynamic Model of island biogeography ([Whittaker et al. 2008](http://onlinelibrary.wiley.com/doi/10.1111/j.1365-2699.2008.01892.x/abstract))

This is a reimplementation of the deterministic biodiversity simulation of [Borregaard et al. (2008)](http://onlinelibrary.wiley.com/doi/10.1111/geb.12348/abstract), complemented by optional diversity-independent dynamics and the effect of island area on the immigration probability ('target effect'; [Gilpin and Diamond 1976](https://www.pnas.org/content/73/11/4130)).


##### Usage
The package can be installed directly from this github repository and its main function `diversity_through_time` calculates native species richness, endemic richness, and rates of immigration, emigration, extinction and *in-situ* speciation for given trajectories of island area, topography, and elevation. 

```{r, warning = F, echo = F}
library(devtools)
install_github("thauffe/simGDM")
library(simGDM)
```
