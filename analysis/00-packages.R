## check for missing packages and install if needed

if(!require(tidyverse))install.packages("tidyverse")
if(!require(pacea))remotes::install_github("pbs-assess/pacea")
if(!require(sf))install.packages("sf")
if(!require(patchwork))install.packages("patchwork")
if(!require(GGally))install.packages("GGally") # used for correlation plots
if(!require(ragg))install.packages("ragg") # used for correlation plots
if(!require(brms))install.packages("brms")
if(!require(ggsidekick))remotes::install_github("seananderson/ggsidekick")

# rosettafish for french translation of figures
# needs to be updated to contain all environmental variables before using
# so may be better to install from a local clone, but if git version has been updated then this works
if(!require(rosettafish))remotes::install_github("pbs-assess/rosettafish")


# install cmdstanr to use instead of rstan as the backend:
if (FALSE) {
  install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
  cmdstanr::install_cmdstan()
}
