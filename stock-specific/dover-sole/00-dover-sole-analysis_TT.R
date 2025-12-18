# Prep data and folders for stock specific condition analysis
library(tidyverse)
library(pacea)
# library(brms)
devtools::load_all()

# plotting options
theme_set(ggsidekick::theme_sleek())

FRENCH <- FALSE
## all new environmental variables can be added to the rosettafish dictionary
## and then everything can be rerun to produce french versions of plots
## ultimately code could be modified to save the component part of the figures instead of the timeseries figures themselve to avoid rerunning all iterations
## FRENCH <- FALSE
# FRENCH <- TRUE
## if you've updated the package and want to reload it without restarting R
# detach("package:rosettafish", unload=TRUE)

# species <- "Yelloweye Rockfish"
# stock <- "Outside"

species <- "Dover Sole"
stock <- "Coastwide"
stock_name <- "Dover Sole"

set_utm_crs <- 32609

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(stock_name)))

# create directories
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# TODO: add SS3 outputs

## Prep SS3 outputs ----
raw_mle_fit <- readRDS(paste0("stock-specific/",spp,"/data/mle_out.rdata"))
out_sum <- extract_SS(raw_mle_fit, name=NULL)
n_draws <- 100
scenario <- out_sum$model_name

## only run if first time
out_mcmc <- readRDS(paste0("stock-specific/",spp,"/data/mcmc_out.rdata"))
format_mcmc(out_mcmc, species, stock, scenario, age_recruited = out_sum$age_recruited, samples = n_draws)

format_ss3_summary(out_sum, species, stock, mcmc=out_mcmc)

## Set spatiotemporal scales ----

# define depths of interest
# depth ranges (could be from Love 2011)
species_min <- 15 # suggested "adult min" in Love is 50, but largish catch of small dover recorded as shallow as 15m by MSSM
species_max <- 1400

# from 95% of commercially landed biomass February through April
spawn_min <- 270
spawn_max <- 620

# # maybe skip this for dover?
# juv_min <- ##
# juv_max <- ##

# depth range from our survey data encompassing 95% of this species’ biomass density
# rounded to nearest 5m
summer_min <- 90
summer_max <- 460

# define months of interest
spawning_months <- c(1,2,3,4,5) # same as winter in the stock assessment
pelagic_months <- c(4,5,6,7,8,9,10,11,12)
juv_months <- NULL#
# condition_months_A <- spawning_months
condition_months <- c(4,5,6)


# this uses a new_grid created with 00-pacea-grid.R
load("data/grid.rda")

sp_grid <- new_grid |> filter(depth_min >= species_min & depth_max <= species_max & max_depth < 0)
# plot(sp_grid)
spawn_grid <- new_grid |> filter(depth_min >= spawn_min & depth_max <= spawn_max & max_depth < 0)
# plot(spawn_grid)
juv_grid <- NULL #new_grid |> filter(depth_min >= juv_min & depth_max <= juv_max & max_depth < 0)
# plot(spawn_grid)
summer_grid <- new_grid |> filter(depth_min >= summer_min & depth_max <= summer_max & max_depth < 0)
# plot(summer_grid)


## Get variables ----
# this just gets a bunch of annual averages
# source("analysis/xx-get-all-enviro-vars.R")
load("data/oisst_month_grid26.rda") # not yet added to pacea

# which community variables are we not interested in
# check options
# unique(herring_recruitment$region)
conspecific_ssb <- NULL #TRUE #NULL
herring_stocks <- c("HG", "PRD", "CC", "WCVI") #NULL c("HG", "PRD", "CC", "SOG", "WCVI")
copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")
copepod_months <- NULL #c(pelagic_months) #NULL 
euphausids <- TRUE

source("analysis/01-get-community-vars.R")

npcbi <- select(bi, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1)
npcbi0 <- extract_enviro_var(npcbi, "Current bifurcation")
alpi0 <- select(alpi, year, anomaly) |> rename(value = anomaly) |> mutate(month = 1) %>% extract_enviro_var(., "Aleutian Low Pressure")
pdo0 <- extract_enviro_var(pdo, "PDO (current year)", c(spawning_months, pelagic_months, juv_months))
npgo0 <- extract_enviro_var(npgo, "NPGO (current year)", c(spawning_months, pelagic_months, juv_months))
npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)", c(spawning_months, pelagic_months, juv_months)) |> mutate(year = year +1)
npgo2 <- extract_enviro_var(npgo, "NPGO (2 yrs prior)", c(spawning_months, pelagic_months, juv_months)) |> mutate(year = year +2)

spawn_pdo <- extract_enviro_var(pdo, "PDO (Jan-May)", spawning_months)
spawn_npgo <- extract_enviro_var(npgo, "NPGO (Jan-May)", spawning_months)
spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jan-May)", spawning_months, spawn_grid)
spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Jan-May)", spawning_months, spawn_grid)
spawn_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Jan-May)", spawning_months, spawn_grid)
spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Jan-May)", spawning_months, spawn_grid)

# pelagic_pdo <- extract_enviro_var(pdo, "PDO (Jan-Dec)", c(spawning_months, pelagic_months)) # same as Current year above
# pelagic_npgo <- extract_enviro_var(npgo, "NPGO (Jan-Dec)", c(spawning_months, pelagic_months)) # same as Current year above
pelagic_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Apr-Dec)", pelagic_months, sp_grid)
# pelagic_sst <- extract_enviro_var(bccm_surface_temperature(), "SST (Apr-Dec)", pelagic_months, sp_grid)
pelagic_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-Dec)", pelagic_months, sp_grid)
pelagic_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Apr-Dec)", pelagic_months, sp_grid)
pelagic_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Apr-Dec)", pelagic_months, sp_grid)


# cope.ns.b <- extract_enviro_var(cops.ns.boreal, "Boreal Copepods (North VI)")
# cope.ns.s <- extract_enviro_var(cops.ns.south, "Southern Copepods (North VI)")
# cope.ns.n <- extract_enviro_var(cops.ns.subarctic, "Subarctic copepods (North VI)")
# cope.ss.b <- extract_enviro_var(cops.ss.boreal, "Copepods - Boreal (South VI)")
# cope.ss.s <- extract_enviro_var(cops.ss.south, "Copepods - Southern (South VI)")
# cope.ss.n <- extract_enviro_var(cops.ss.subarctic, "Copepods - Subarctic (South VI)")

## combines south and north VI shelf areas
cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Copepods - medium (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Copepods - small (VI shelf)")
# cope.shelf.sb <- extract_enviro_var(cops.shelf.nonarctic, "Copepods - small  (VI shelf)") # pools small and medium
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Copepods - large (VI shelf)")

# juv_pdo <- extract_enviro_var(pdo, "PDO (Jun-Dec)", juv_months)
# juv_npgo <- extract_enviro_var(npgo, "NPGO (Jun-Dec)", juv_months)
# ssb0 <- extract_enviro_var(conspecific_ssb, "Dover Sole SSB")
# juv_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jun-Dec)", juv_months, juv_grid)
# juv_t <- extract_enviro_var(bccm_bottom_temperature(),  "Seafloor temperature (Jun-Dec)", juv_months, juv_grid)
# juv_sst <- extract_enviro_var(oisst_month_grid26, "SST (Jun-Dec)", juv_months, juv_grid)
# juv_s <- extract_enviro_var(bccm_bottom_salinity(),  "Seafloor salinity (Jun-Dec)", juv_months, juv_grid)
# juv_herr <- extract_enviro_var(herring_recuits,  "Herring recruitment")
# herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")
# juv_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Jun-Dec)", juv_months, juv_grid)



# --- Trav's additional variables --- #

##### REDO ALL

# spawning spatial
win_spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jan-Mar)", c(1:3), spawn_grid)
spr_spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Apr-Jun)", c(4:6), spawn_grid)
sum_spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jul-Sep)", c(7:9), spawn_grid)
fal_spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Oct-Dec)", c(10:12), spawn_grid)

win_spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Jan-Mar)", c(1:3), spawn_grid)
spr_spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Apr-Jun)", c(4:6), spawn_grid)
sum_spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Jul-Sep)", c(7:9), spawn_grid)
fal_spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Oct-Dec)", c(10:12), spawn_grid)

win_spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Jan-Mar)", c(1:3), spawn_grid)
spr_spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Apr-Jun)", c(4:6), spawn_grid)
sum_spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Jul-Sep)", c(7:9), spawn_grid)
fal_spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Oct-Dec)", c(10:12), spawn_grid)

win_spawn_ph <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor pH (Jan-Mar)", c(1:3), spawn_grid)
spr_spawn_ph <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor pH (Apr-Jun)", c(4:6), spawn_grid)
sum_spawn_ph <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor pH (Jul-Sep)", c(7:9), spawn_grid)
fal_spawn_ph <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor pH (Oct-Dec)", c(10:12), spawn_grid)

# pelagic spatial
win_pel_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Jan-Mar)", c(1:3), sp_grid)
spr_pel_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Apr-Jun)", c(4:6), sp_grid)
sum_pel_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Jul-Sep)", c(7:9), sp_grid)
fal_pel_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Oct-Dec)", c(10:12), sp_grid)

win_pel_t <- extract_enviro_var(bccm_surface_temperature(), "Surface temperature (Jan-Mar)", c(1:3), sp_grid)
spr_pel_t <- extract_enviro_var(bccm_surface_temperature(), "Surface temperature (Apr-Jun)", c(4:6), sp_grid)
sum_pel_t <- extract_enviro_var(bccm_surface_temperature(), "Surface temperature (Jul-Sep)", c(7:9), sp_grid)
fal_pel_t <- extract_enviro_var(bccm_surface_temperature(), "Surface temperature (Oct-Dec)", c(10:12), sp_grid)

win_pel_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Jan-Mar)", c(1:3), sp_grid)
spr_pel_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Apr-Jun)", c(4:6), sp_grid)
sum_pel_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Jul-Sep)", c(7:9), sp_grid)
fal_pel_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Oct-Dec)", c(10:12), sp_grid)

win_pel_ph <- extract_enviro_var(bccm_surface_salinity(), "Surface pH (Jan-Mar)", c(1:3), sp_grid)
spr_pel_ph <- extract_enviro_var(bccm_surface_salinity(), "Surface pH (Apr-Jun)", c(4:6), sp_grid)
sum_pel_ph <- extract_enviro_var(bccm_surface_salinity(), "Surface pH (Jul-Sep)", c(7:9), sp_grid)
fal_pel_ph <- extract_enviro_var(bccm_surface_salinity(), "Surface pH (Oct-Dec)", c(10:12), sp_grid)

win_pel_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Jan-Mar)", c(1:3), sp_grid)
spr_pel_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Apr-Jun)", c(4:6), sp_grid)
sum_pel_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Jul-Sep)", c(7:9), sp_grid)
fal_pel_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Oct-Dec)", c(10:12), sp_grid)

win_pel_pp <- extract_enviro_var(bccm_primaryproduction(), "PP (Jan-Mar)", c(1:3), sp_grid)
spr_pel_pp <- extract_enviro_var(bccm_primaryproduction(), "PP (Apr-Jun)", c(4:6), sp_grid)
sum_pel_pp <- extract_enviro_var(bccm_primaryproduction(), "PP (Jul-Sep)", c(7:9), sp_grid)
fal_pel_pp <- extract_enviro_var(bccm_primaryproduction(), "PP (Oct-Dec)", c(10:12), sp_grid)

# copepods
cope.ns.b <- extract_enviro_var(cops.ns.boreal, "Boreal Copepods (North VI)")
cope.ns.s <- extract_enviro_var(cops.ns.south, "Southern Copepods (North VI)")
cope.ns.n <- extract_enviro_var(cops.ns.subarctic, "Subarctic Copepods (North VI)")
cope.ss.b <- extract_enviro_var(cops.ss.boreal, "Copepods - Boreal (South VI)")
cope.ss.s <- extract_enviro_var(cops.ss.south, "Copepods - Southern (South VI)")
cope.ss.n <- extract_enviro_var(cops.ss.subarctic, "Copepods - Subarctic (South VI)")
cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Copepods - medium (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Copepods - small (VI shelf)")
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Copepods - large (VI shelf)")
cope.shelf.sb <- extract_enviro_var(cops.shelf.nonarctic, "Copepods - small,med  (VI shelf)") # pools small and medium

# euphausiids
euphN
euphO
euphS
euph

# herring
herr_rec <- extract_enviro_var(herring_recuits,  "Herring recruitment")
herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")

# climate indices
win_npgo0 <- extract_enviro_var(npgo, "NPGO winter (current year)", c(1,2,3))
spr_npgo0 <- extract_enviro_var(npgo, "NPGO spring (current year)", c(4,5,6))
sum_npgo0 <- extract_enviro_var(npgo, "NPGO summer (current year)", c(7,8,9))
fal_npgo0 <- extract_enviro_var(npgo, "NPGO fall (current year)", c(10,11,12))
win_npgo1 <- extract_enviro_var(npgo, "NPGO winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_npgo1 <- extract_enviro_var(npgo, "NPGO spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_npgo1 <- extract_enviro_var(npgo, "NPGO summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_npgo1 <- extract_enviro_var(npgo, "NPGO fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_npgo2 <- extract_enviro_var(npgo, "NPGO winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_npgo2 <- extract_enviro_var(npgo, "NPGO spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_npgo2 <- extract_enviro_var(npgo, "NPGO summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_npgo2 <- extract_enviro_var(npgo, "NPGO fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_pdo0 <- extract_enviro_var(pdo, "PDO winter (current year)", c(1,2,3))
spr_pdo0 <- extract_enviro_var(pdo, "PDO spring (current year)", c(4,5,6))
sum_pdo0 <- extract_enviro_var(pdo, "PDO summer (current year)", c(7,8,9))
fal_pdo0 <- extract_enviro_var(pdo, "PDO fall (current year)", c(10,11,12))
win_pdo1 <- extract_enviro_var(pdo, "PDO winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_pdo1 <- extract_enviro_var(pdo, "PDO spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_pdo1 <- extract_enviro_var(pdo, "PDO summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_pdo1 <- extract_enviro_var(pdo, "PDO fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_pdo2 <- extract_enviro_var(pdo, "PDO winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_pdo2 <- extract_enviro_var(pdo, "PDO spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_pdo2 <- extract_enviro_var(pdo, "PDO summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_pdo2 <- extract_enviro_var(pdo, "PDO fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_npi0 <- extract_enviro_var(npi_monthly, "NPI winter (current year)", c(1,2,3))
spr_npi0 <- extract_enviro_var(npi_monthly, "NPI spring (current year)", c(4,5,6))
sum_npi0 <- extract_enviro_var(npi_monthly, "NPI summer (current year)", c(7,8,9))
fal_npi0 <- extract_enviro_var(npi_monthly, "NPI fall (current year)", c(10,11,12))
win_npi1 <- extract_enviro_var(npi_monthly, "NPI winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_npi1 <- extract_enviro_var(npi_monthly, "NPI spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_npi1 <- extract_enviro_var(npi_monthly, "NPI summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_npi1 <- extract_enviro_var(npi_monthly, "NPI fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_npi2 <- extract_enviro_var(npi_monthly, "NPI winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_npi2 <- extract_enviro_var(npi_monthly, "NPI spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_npi2 <- extract_enviro_var(npi_monthly, "NPI summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_npi2 <- extract_enviro_var(npi_monthly, "NPI fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_oni0 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI winter (current year)", c(1,2,3))
spr_oni0 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI spring (current year)", c(4,5,6))
sum_oni0 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI summer (current year)", c(7,8,9))
fal_oni0 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI fall (current year)", c(10,11,12))
win_oni1 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_oni1 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_oni1 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_oni1 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_oni2 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_oni2 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_oni2 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_oni2 <- select(oni, year, month, anomaly) %>% extract_enviro_var(., "ONI fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_ao0 <- extract_enviro_var(ao, "AO winter (current year)", c(1,2,3))
spr_ao0 <- extract_enviro_var(ao, "AO spring (current year)", c(4,5,6))
sum_ao0 <- extract_enviro_var(ao, "AO summer (current year)", c(7,8,9))
fal_ao0 <- extract_enviro_var(ao, "AO fall (current year)", c(10,11,12))
win_ao1 <- extract_enviro_var(ao, "AO winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_ao1 <- extract_enviro_var(ao, "AO spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_ao1 <- extract_enviro_var(ao, "AO summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_ao1 <- extract_enviro_var(ao, "AO fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_ao2 <- extract_enviro_var(ao, "AO winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_ao2 <- extract_enviro_var(ao, "AO spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_ao2 <- extract_enviro_var(ao, "AO summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_ao2 <- extract_enviro_var(ao, "AO fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_soi0 <- extract_enviro_var(soi, "SOI winter (current year)", c(1,2,3))
spr_soi0 <- extract_enviro_var(soi, "SOI spring (current year)", c(4,5,6))
sum_soi0 <- extract_enviro_var(soi, "SOI summer (current year)", c(7,8,9))
fal_soi0 <- extract_enviro_var(soi, "SOI fall (current year)", c(10,11,12))
win_soi1 <- extract_enviro_var(soi, "SOI winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_soi1 <- extract_enviro_var(soi, "SOI spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_soi1 <- extract_enviro_var(soi, "SOI summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_soi1 <- extract_enviro_var(soi, "SOI fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_soi2 <- extract_enviro_var(soi, "SOI winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_soi2 <- extract_enviro_var(soi, "SOI spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_soi2 <- extract_enviro_var(soi, "SOI summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_soi2 <- extract_enviro_var(soi, "SOI fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

win_mei0 <- extract_enviro_var(mei, "MEI winter (current year)", c(1,2,3))
spr_mei0 <- extract_enviro_var(mei, "MEI spring (current year)", c(4,5,6))
sum_mei0 <- extract_enviro_var(mei, "MEI summer (current year)", c(7,8,9))
fal_mei0 <- extract_enviro_var(mei, "MEI fall (current year)", c(10,11,12))
win_mei1 <- extract_enviro_var(mei, "MEI winter (prior year)", c(1,2,3)) |> mutate(year = year +1)
spr_mei1 <- extract_enviro_var(mei, "MEI spring (prior year)", c(4,5,6)) |> mutate(year = year +1)
sum_mei1 <- extract_enviro_var(mei, "MEI summer (prior year)", c(7,8,9)) |> mutate(year = year +1)
fal_mei1 <- extract_enviro_var(mei, "MEI fall (prior year)", c(10,11,12)) |> mutate(year = year +1)
win_mei2 <- extract_enviro_var(mei, "MEI winter (2 yrs prior)", c(1,2,3)) |> mutate(year = year +2)
spr_mei2 <- extract_enviro_var(mei, "MEI spring (2 yrs prior)", c(4,5,6)) |> mutate(year = year +2)
sum_mei2 <- extract_enviro_var(mei, "MEI summer (2 yrs prior)", c(7,8,9)) |> mutate(year = year +2)
fal_mei2 <- extract_enviro_var(mei, "MEI fall (2 yrs prior)", c(10,11,12)) |> mutate(year = year +2)

npcbi <- select(bi, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1)
npcbi0 <- extract_enviro_var(npcbi, "Current bifurcation")
npcbi1 <- extract_enviro_var(npcbi, "Current bifurcation (prior year)")  |> mutate(year = year +1)
npcbi2 <- extract_enviro_var(npcbi, "Current bifurcation (2 yrs prior)")  |> mutate(year = year +2)

npi <- select(npi_annual, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1)
npi0 <- extract_enviro_var(npi, "NPI ann.")
npi1 <- extract_enviro_var(npi, "NPI ann. (prior year)")  |> mutate(year = year +1)
npi2 <- extract_enviro_var(npi, "NPI ann. (2 yrs prior)")  |> mutate(year = year +2)

alpi0 <- select(alpi, year, anomaly) |> rename(value = anomaly) |> mutate(month = 1) %>% extract_enviro_var(., "ALPI (current year)") 
alpi1 <- select(alpi, year, anomaly) |> rename(value = anomaly) |> mutate(month = 1) %>% extract_enviro_var(., "ALPI (prior year)") |> mutate(year = year +1)
alpi2 <- select(alpi, year, anomaly) |> rename(value = anomaly) |> mutate(month = 1) %>% extract_enviro_var(., "ALPI (2 yrs prior)") |> mutate(year = year +2)



dt <- bind_rows(
  
  #spatial 
  win_spawn_o2,spr_spawn_o2,sum_spawn_o2,fal_spawn_o2
  ,win_spawn_t,spr_spawn_t,sum_spawn_t,fal_spawn_t
  ,win_spawn_s,spr_spawn_s,sum_spawn_s,fal_spawn_s
  ,win_spawn_ph,spr_spawn_ph,sum_spawn_ph,fal_spawn_ph
  
  ,win_pel_o2,spr_pel_o2,sum_pel_o2,fal_pel_o2
  ,win_pel_t,spr_pel_t,sum_pel_t,fal_pel_t
  ,win_pel_s,spr_pel_s,sum_pel_s,fal_pel_s
  ,win_pel_ph,spr_pel_ph,sum_pel_ph,fal_pel_ph
  ,win_pel_p,spr_pel_p,sum_pel_p,fal_pel_p
  ,win_pel_pp,spr_pel_pp,sum_pel_pp,fal_pel_pp
  
  #bio
  ,cope.ns.b,cope.ns.s,cope.ns.n
  ,cope.ss.b,cope.ss.s,cope.ss.n
  ,cope.shelf.b,cope.shelf.s,cope.shelf.n,cope.shelf.sb
  ,euphN,euphO,euphS,euph
  # ,herr_rec,herr_ssb
  
  #CI
  ,win_npgo0,spr_npgo0,sum_npgo0,fal_npgo0
  ,win_npgo1,spr_npgo1,sum_npgo1,fal_npgo1
  ,win_npgo2,spr_npgo2,sum_npgo2,fal_npgo2
  
  ,win_pdo0,spr_pdo0,sum_pdo0,fal_pdo0
  ,win_pdo1,spr_pdo1,sum_pdo1,fal_pdo1
  ,win_pdo2,spr_pdo2,sum_pdo2,fal_pdo2
  
  ,win_npi0,spr_npi0,sum_npi0,fal_npi0
  ,win_npi1,spr_npi1,sum_npi1,fal_npi1
  ,win_npi2,spr_npi2,sum_npi2,fal_npi2
  
  ,win_oni0,spr_oni0,sum_oni0,fal_oni0
  ,win_oni1,spr_oni1,sum_oni1,fal_oni1
  ,win_oni2,spr_oni2,sum_oni2,fal_oni2
  
  ,win_ao0,spr_ao0,sum_ao0,fal_ao0
  ,win_ao1,spr_ao1,sum_ao1,fal_ao1
  ,win_ao2,spr_ao2,sum_ao2,fal_ao2
  
  ,win_soi0,spr_soi0,sum_soi0,fal_soi0
  ,win_soi1,spr_soi1,sum_soi1,fal_soi1
  ,win_soi2,spr_soi2,sum_soi2,fal_soi2
  
  ,win_mei0,spr_mei0,sum_mei0,fal_mei0
  ,win_mei1,spr_mei1,sum_mei1,fal_mei1
  ,win_mei2,spr_mei2,sum_mei2,fal_mei2
  
  ,npcbi0,npcbi1,npcbi2
  ,alpi0,alpi1,alpi2
  ,npi0,npi1,npi2
)



# split up dt based on life stage

ds <- bind_rows(
  pelagic_p
  ,euphN
  ,spr_spawn_s
  ,spr_spawn_t
  ,npcbi0
  ,npcbi1
  ,fal_npgo2
  ,fal_npgo1
  ,spr_npgo1
  ,sum_npgo1
  ,win_npgo2
  ,win_npgo1
  ,fal_pdo1
  ,sum_pdo1
  ,spr_oni0
  ,win_oni1
  ,fal_ao2
  ,spr_mei1
  ,win_mei1
  ,win_soi1
  ,spr_soi1
)

dp <- bind_rows(
  pelagic_p
  ,euphN
  ,pelagic_o2
  ,pelagic_s
  ,spr_pelagic_o2
  ,win_pelagic_o2
  ,spr_pelagic_s
  ,npcbi0
  ,npcbi1
  ,fal_npgo2
  ,fal_npgo1
  ,spr_npgo1
  ,sum_npgo1
  ,win_npgo2
  ,win_npgo1
  ,fal_pdo1
  ,sum_pdo1
  ,fal_oni0
  ,spr_oni0
  ,sum_oni0
  ,win_oni1
  ,fal_ao2
  ,sum_ao0
  ,fal_mei0
  ,sum_mei0
  ,spr_mei1
  ,win_mei1
  ,win_soi1
  ,spr_soi1
)

dj <- NULL

## combine variables into separate dataframes based on lifestage (spawn, pelagic, juvenile)
# make sure variables aren't duplicated?

# ds <- bind_rows(
#   npgo2
#   # ,alpi0
#   # ,npcbi0
#   # ,npgo1
#   ,spawn_o2
#   ,spawn_t
#   # ,spawn_sstoi
#   ,spawn_s
#   ,spawn_pdo
#   ,spawn_npgo
# )
# 
# dp <- bind_rows(
#   pdo0
#   ,npgo0
#   ,npcbi0
#   ,alpi0
#   ,pelagic_o2
#   ,pelagic_sstoi
#   ,pelagic_s
#   ,pelagic_p
#   ,cope.shelf.b
#   ,cope.shelf.s
#   ,cope.shelf.n
#   ,euphN
#   ,euphO
#   ,euphS
# )

# dj <- NULL
# dj <- bind_rows(
#   juv_pdo
#   ,juv_npgo
#   ,juv_o2
#   ,juv_t
#   ,juv_sst
#   ,juv_s
#   ,juv_pp
#   ,juv_herr
#   ,herr_ssb
#   ,ssb0
# )


## combine all variables into one data frame
# dvr <- bind_rows(ds,dp,dj)
# dvr <- bind_rows(ds,dp,dj,dt)
dvr <- dt # all variables

saveRDS(dvr, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))

# --- Body condition
cond_months <- c(4,5,6)

cond_pdo <- extract_enviro_var(pdo, "PDO (Apr-Jun)", cond_months)
cond_npgo <- extract_enviro_var(npgo, "NPGO (Apr-Jun)", cond_months)
cond_npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)") |> mutate(year = year +1)
cond_npgo2 <- extract_enviro_var(npgo, "NPGO (2 yrs prior)") |> mutate(year = year +2)
# cond_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (prior year)", c(spawning_months, pelagic_months, juv_months), sp_grid) |> mutate(year = year +1)
cond_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Apr-Jun)", cond_months, summer_grid)
cond_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Apr-Jun)", cond_months, summer_grid)
cond_sst <- extract_enviro_var(bccm_surface_temperature(), "Surface temperature (Apr-Jun)", cond_months, summer_grid)
cond_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Apr-Jun)", cond_months, summer_grid)
# herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")


dvc <- bind_rows(
  cond_pdo
  # ,npcbi0
  # ,cond_npgo
  # ,cond_npgo1
  ,cond_npgo2
  # ,cond_pp
  ,cond_o2
  ,cond_t
  ,cond_s
  ,cond_sst
  # ,herr_ssb
  # ,juv_herr
  # ,juv_pp
)

saveRDS(dvc, paste0("stock-specific/",spp,"/data/envrio-vars-for-condition.rds"))


## Choose colours ----

dvr <- readRDS(paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))
dvc <- readRDS(paste0("stock-specific/",spp,"/data/envrio-vars-for-condition.rds"))

nvars <- length(sort(unique(c(dvr$type, dvc$type))))
colours <- c(seq(1:nvars))
colkey <- data.frame(type = sort(unique(c(dvr$type, dvc$type))), id = colours)
colkey[grepl("ALPI", colkey$type),]$id <- min(colkey[grepl("ALPI", colkey$type),]$id)
colkey[grepl("AO", colkey$type),]$id <- min(colkey[grepl("AO", colkey$type),]$id)
colkey[grepl("Current", colkey$type),]$id <- min(colkey[grepl("Current", colkey$type),]$id)
colkey[grepl("MEI", colkey$type),]$id <- min(colkey[grepl("MEI", colkey$type),]$id)
colkey[grepl("NPGO", colkey$type),]$id <- min(colkey[grepl("NPGO", colkey$type),]$id)
colkey[grepl("NPI", colkey$type),]$id <- min(colkey[grepl("NPI", colkey$type),]$id)
colkey[grepl("ONI", colkey$type),]$id <- min(colkey[grepl("ONI", colkey$type),]$id)
colkey[grepl("PDO", colkey$type),]$id <- min(colkey[grepl("PDO", colkey$type),]$id)
colkey[grepl("SOI", colkey$type),]$id <- min(colkey[grepl("SOI", colkey$type),]$id)
colkey[grepl("Copepods", colkey$type),]$id <- min(colkey[grepl("Copepods", colkey$type),]$id)
colkey[grepl("Euphausiid", colkey$type),]$id <- min(colkey[grepl("Euphausiid", colkey$type),]$id)
colkey[grepl("Herring", colkey$type),]$id <- min(colkey[grepl("Herring", colkey$type),]$id)
colkey[grepl("Phytoplankton", colkey$type),]$id <- min(colkey[grepl("Phytoplankton", colkey$type),]$id)
colkey[grepl("PP", colkey$type),]$id <- min(colkey[grepl("PP", colkey$type),]$id)
colkey[grepl("O2", colkey$type),]$id <- min(colkey[grepl("O2", colkey$type),]$id)
colkey[grepl("pH", colkey$type),]$id <- min(colkey[grepl("pH", colkey$type),]$id)
colkey[grepl("salinity", colkey$type),]$id <- min(colkey[grepl("salinity", colkey$type),]$id)
colkey[grepl("temperature", colkey$type),]$id <- min(colkey[grepl("temperature", colkey$type),]$id)
# colkey[grepl("SST", colkey$type),]$id <- min(colkey[grepl("SST", colkey$type),]$id)

# length(unique(colkey$id))
pal <- scales::hue_pal()(length(unique(colkey$id)))
# plot(1:length(pal), pch = 20, cex = 4, col = pal)
colours <- data.frame(colour = pal, id = rev(unique(colkey$id)))
colkey <- left_join(colkey, colours)
# plot(1:length(colkey$type), pch = 20, cex = 4, col = colkey$colour)

## Model setting and priors ----
median_model_iter <- 2000
median_chains <- 4
control_list <- list(adapt_delta = 0.9)
set_priors <- c(
  brms::set_prior("normal(0, 0.5)", class = "ar"),
  brms::set_prior("normal(0, 10)", class = "b"),
  brms::set_prior("student_t(3, 0, 2)", class = "sigma"),
  brms::set_prior("normal(0, 10)", class = "Intercept")
  )

## Recruitment analysis ----
# little environmental and age data pre-1995
plot(out_sum$RecDevs$Yr[out_sum$RecDevs$type=="Main_RecrDev"], out_sum$RecDevs$Value[out_sum$RecDevs$type=="Main_RecrDev"])

year_range <- range(out_sum$RecDevs$Yr[out_sum$RecDevs$type=="Main_RecrDev"]) - out_sum$age_recruited

r_start_year <- year_range[1] ## # first year with significant age data
r_end_year <- 2020 # current most recent age data is from 2022 on a survey with youngest ages ever being 2

start_year <- r_start_year
final_year <- r_end_year

# plot variables using script
# source("analysis/02-plot-vars.R")

## FRENCH argument doesn't matter until this point,
## so can also be changed here as needed
# FRENCH <- FALSE
# FRENCH <- TRUE

remove_outliers <- NULL
shortlist <- FALSE


# --- run linear and quadratic model first --- #
# set r^2 threshold for filtering out variables - run as NULL first
r2 <- NULL

source("analysis/03-correlation-w-recruitment-lm.R")
source("analysis/03-correlation-w-recruitment-quadratic.R")

# set threshold and max number of variables to consider
r2 <- 0.1
max_vars <- 30  # max number of variables to include

source("analysis/03-correlation-w-recruitment-lm.R")
source("analysis/03-correlation-w-recruitment-quadratic.R")

# subset environmental variables based on r2 selection from lm and quadratic
topvars <- bind_rows(topvars_lm, topvars_qm) |> 
  group_by(var_names) |> 
  summarise(m_r2 = mean(r_squared, na.rm = TRUE),
            min_r2 = min(r_squared, na.rm = TRUE),
            max_r2 = max(r_squared, na.rm = TRUE)) |>
  mutate(diff = max_r2 - min_r2) |>
  arrange(desc(m_r2)) 
topvars <- topvars$var_names[1:max_vars]
dvrsub <- dvr |> filter(type %in% topvars)
saveRDS(dvrsub, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-dvrsub.rds"))
saveRDS(dvrsub, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))

# bayesian model
source("analysis/03-correlations-w-recruitment-brms.R")

# plot variables using script after running analysis - selected variables used
source(paste0("stock-specific/", spp, "/02-plot-vars_", spp, ".R"))

# --- --- #
## once you've reviewed these results

# shortlist <- TRUE
# dvs2 <- bind_rows(
#   # pick a shortlist from of recruitment variables that represent the strongest relationships
# )
# saveRDS(dvs2, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))

# source("analysis/03-correlations-w-recruitment-brms.R")

# try removing outliers
# remove_outliers <- 2016
# shortlist <- FALSE
# source("analysis/03-correlations-w-recruitment-brms.R")

## Condiiton analyses ----

# copy cond-index and cond-index-sims folders from gfcondition to data folder
# review condition rmd
c_start_year <- 1996 # first year in condition models
c_end_year <- 2024 # last year in condition models


control_list <- list(adapt_delta = 0.95)
which_cond_model1 <- "2025-09"
cutoff <- 20
source("analysis/04-correlations-btw-rdev-condition-brms.R")


control_list <- list(adapt_delta = 0.9)
which_cond_model2 <- "2025-09-ld0c"
source("analysis/05-correlations-w-condition-brms.R")

final_year <- 2025 # for variable plotting
# source("analysis/02-plot-vars.R") # not working sourced
