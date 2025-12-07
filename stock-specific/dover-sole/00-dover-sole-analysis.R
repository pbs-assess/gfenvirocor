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
## mesh cutoff, usually 20 for coastwide stocks, sometimes smaller for smaller stock areas
cutoff <- 20

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
format_ss3_summary(out_sum, species, stock, mcmc = out_mcmc)
format_mcmc(out_mcmc, species, stock, scenario, age_recruited = out_sum$age_recruited, samples = n_draws)

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
cond_months <- c(4,5,6)


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
conspecific_ssb <- NULL #TRUE
herring_stocks <- NULL #c("HG", "PRD", "CC", "WCVI")
copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")
copepod_months <- NULL #c(pelagic_months)
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
spawn_npcbi <- extract_enviro_var(npcbi, "Current bifurcation", spawning_months)
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



# spawning
ds <- bind_rows(
  npgo2
  # ,alpi0
  # ,npcbi0
  # ,npgo1
  ,spawn_o2
  ,spawn_t
  # ,spawn_sstoi
  ,spawn_s
  ,spawn_pdo
  ,spawn_npgo
)

#pelagic larvae
dp <- bind_rows(
  pdo0
  ,npgo0
  ,npcbi0
  ,alpi0
  ,pelagic_o2
  ,pelagic_sstoi
  ,pelagic_s
  ,pelagic_p
  ,cope.shelf.b
  ,cope.shelf.s
  ,cope.shelf.n
  ,euphN
  ,euphO
  ,euphS
)

dj <- NULL
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

dvr <- bind_rows(ds,dp,dj)

saveRDS(dvr, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))


# cond_months <- c(4,5,6)

cond_pdo <- extract_enviro_var(pdo, "PDO (Apr-Jun)", cond_months)
cond_npgo <- extract_enviro_var(npgo, "NPGO (Apr-Jun)", cond_months)
cond_npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)") |> mutate(year = year +1)
cond_npgo2 <- extract_enviro_var(npgo, "NPGO (2 yrs prior)") |> mutate(year = year +2)
cond_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Jan-May)", c(spawning_months), summer_grid)
cond_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Apr-Jun)", cond_months, summer_grid)
cond_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Apr-Jun)", cond_months, summer_grid)
cond_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-Jun)", cond_months, summer_grid)
cond_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Apr-Jun)", cond_months, summer_grid)
# herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")


dvc <- bind_rows(
  cond_pdo
  # ,cond_npgo
  # ,cond_npgo1
  ,cond_npgo2
  ,cond_pp
  ,cond_o2
  ,cond_t
  ,cond_s
  ,cond_sstoi
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
colkey[grepl("NPGO", colkey$type),]$id <- min(colkey[grepl("NPGO", colkey$type),]$id)
colkey[grepl("PDO", colkey$type),]$id <- min(colkey[grepl("PDO", colkey$type),]$id)
colkey[grepl("O2", colkey$type),]$id <- min(colkey[grepl("O2", colkey$type),]$id)
colkey[grepl("SST", colkey$type),]$id <- min(colkey[grepl("SST", colkey$type),]$id)
colkey[grepl("salinity", colkey$type),]$id <- min(colkey[grepl("salinity", colkey$type),]$id)
colkey[grepl("temperature", colkey$type),]$id <- min(colkey[grepl("temperature", colkey$type),]$id)
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

# copy cond-index and cond-index-sims folders from gfcondition to data folder
# review condition rmd
c_start_year <- 1996 # first year in condition models
c_end_year <- 2024 # last year in condition models


## FRENCH argument doesn't matter until this point,
## so can also be changed here as needed
# FRENCH <- FALSE
# FRENCH <- TRUE

remove_outliers <- NULL
shortlist <- FALSE
source("analysis/03-correlations-w-recruitment-brms.R")

## once you've reviewed these results

# shortlist <- TRUE
dvs2 <- bind_rows(
  # pick a shortlist from of recruitment variables that represent the strongest relationships
)
saveRDS(dvs2, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))

# source("analysis/03-correlations-w-recruitment-brms.R")

# try removing outliers
remove_outliers <- 2016
shortlist <- FALSE
source("analysis/03-correlations-w-recruitment-brms.R")

## Condiiton analyses ----

control_list <- list(adapt_delta = 0.95)
which_cond_model1 <- "2025-07"
source("analysis/04-correlations-btw-rdev-condition-brms.R")


control_list <- list(adapt_delta = 0.9)
which_cond_model2 <- "2025-07-ld0c"
source("analysis/05-correlations-w-condition-brms.R")

final_year <- 2025 # for variable plotting
start_year <- min(c_start_year, r_start_year)
# source("analysis/02-plot-vars.R") # not working sourced
