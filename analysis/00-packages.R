## check for missing packages and install if needed

if(!require(tidyverse))install.packages("tidyverse")
if(!require(pacea))remotes::install_github("pbs-assess/pacea")
if(!require(sf))install.packages("sf")
if(!require(patchwork))install.packages("patchwork")
if(!require(GGally))install.packages("GGally")

# if(!require(aplot))install.packages("aplot") # for working with lists of plots
# if(!require(gridGraphics))install.packages("gridGraphics")
#
# if(!require(future))install.packages("future") # option for parallel running of condition models
# if(!require(remotes))install.packages("remotes")
#
# if(!require(gfplot))remotes::install_github("pbs-assess/gfplot")
#
# if(!require(ggsidekick))remotes::install_github("seananderson/ggsidekick")
# if(!require(ggeffects))remotes::install_github("seananderson/ggeffects", ref = "sdmTMB")


# source("analysis/01-prep-data.R")
# source("analysis/02-sdm.R")
# source("analysis/03-condition-calc.R")
# source("analysis/04-condition-models.R")
# source("analysis/05-combine-condition-indices.R")
# source("analysis/06-plot-condition-maps.R")
