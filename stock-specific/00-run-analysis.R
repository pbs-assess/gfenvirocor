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

# species <- "Dover Sole"
# stock_name <- "Dover Sole"

species <- "Pacific Cod"
stock_name <- "Pacific Cod WCVI"

set_utm_crs <- 32609

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(stock_name)))

# create directories
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# TODO: describe generation of SS3 outputs
## maybe a sample of Nick's code here?

## Prep SS3 outputs ----
out_sum <- readRDS(paste0("stock-specific/",spp,"/data/sum.rdata"))
n_draws <- 100
scenario <- out_sum$model_name

# check years with deviations, ie. excluding any forecast years
r_year_range <- range(out_sum$RecDevs$Yr[out_sum$RecDevs$type=="Main_RecrDev"])

## only run if first time
format_ss3_summary(out_sum, species, stock)
out_mcmc <- readRDS(paste0("stock-specific/",spp,"/data/mcmc.rdata"))

# MCMC - function doesn't return anything. Writes Rdata files.
format_mcmc(out_mcmc, species, stock, scenario, samples = n_draws)



## OR provide params for other custom outputs ----

n_draws <- 100
scenario <- "base"

# e.g., iscam
# Note that the mcmc csv files have to be in a folder called mcmc within iscamdir
# but there seems to be a quirk, where they also have to be in the root directory of iscamdir
iscamdir <- here::here(paste0("stock-specific/",spp,"/data/1a_3CD_2023_reference"))
# This gets both MPD and MSE outputs in one list
iscam_pcod_output <- MSEtool::load.iscam.files(iscamdir)

age_recruited <- iscam_pcod_output$dat$start.age

# load.iscam.files is not accounting for burnin, even with a burnin argument set,
#  so remove burnin samples in the format-mcmc-iscam function
# Set burnin argument to the number of samples to remove from the beginning of the mcmc samples
# Pick the main commercial gear for Ut, in most cases it will be the first gear in iscam outputs
format_mcmc_iscam(iscam_pcod_output,
                  species = species,
                  stock = stock_name,
                  scenario = scenario,
                  samples = n_draws,
                  seed = 10,
                  burnin = 1000,
                  main_commercial_gear = 1)

# check years with deviations, ie. excluding any forecast years
df <- readRDS(paste0("stock-specific/", spp, "/output/mcmc/",scenario,"/df1.RData")) |>
  filter(!is.na(rdev))

r_year_range <- range(df$year)



## Set spatiotemporal scales ----

# define depths of interest
# depth ranges (could be from Love 2011)
species_min <- ###
species_max <- ###

spawn_min <- ##
spawn_max <- ###

juv_min <- ##
juv_max <- ##

# depth range from our survey data encompassing 95% of this species’ biomass
summer_min <- ##
summer_max <- ###

# define months of interest
spawning_months <- c(1,2,3)
pelagic_months <- c(4,5)
juv_months <- c(6,7,8,9,10,11,12)
# condition_months_A <- spawning_months
condition_months <- c(4,5,6)


# this uses a new_grid created with 00-pacea-grid.R
load("data/grid.rda")

sp_grid <- new_grid |> filter(depth_min >= species_min & depth_max <= species_max & max_depth < 0)
# plot(sp_grid)
spawn_grid <- new_grid |> filter(depth_min >= spawn_min & depth_max <= spawn_max & max_depth < 0)
# plot(spawn_grid)
juv_grid <- new_grid |> filter(depth_min >= juv_min & depth_max <= juv_max & max_depth < 0)
# plot(spawn_grid)
summer_grid <- new_grid |> filter(depth_min >= summer_min & depth_max <= summer_max & max_depth < 0)
# plot(summer_grid)


## Get variables ----
# this just gets a bunch of annual averages
# source("analysis/xx-get-all-enviro-vars.R")
load("data/oisst_month_grid26.rda") # not yet added to pacea

# which community variables are we not interested in
euphausids <- NULL
conspecific_ssb <- NULL

# check options
# unique(herring_recruitment$region)
herring_stocks <- c("HG", "PRD", "CC", "WCVI")
copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")
copepod_months <- NULL #c(pelagic_months)

source("analysis/01-get-community-vars.R")

# npcbi <- select(bi, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1)
# npcbi0 <- extract_enviro_var(npcbi, "Current bifurcation")
alpi0 <- select(alpi, year, anomaly) |> rename(value = anomaly) |> mutate(month = 1) %>% extract_enviro_var(., "Aleutian Low Pressure")
pdo0 <- extract_enviro_var(pdo, "PDO (current year)", c(spawning_months, pelagic_months, juv_months))
npgo0 <- extract_enviro_var(npgo, "NPGO (current year)", c(spawning_months, pelagic_months, juv_months))
npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)", c(spawning_months, pelagic_months, juv_months)) |> mutate(year = year +1)
npgo2 <- extract_enviro_var(npgo, "NPGO (2 yrs prior)", c(spawning_months, pelagic_months, juv_months)) |> mutate(year = year +2)
spawn_pdo <- extract_enviro_var(pdo, "PDO (Jan-Mar)", spawning_months)
spawn_npgo <- extract_enviro_var(npgo, "NPGO (Jan-Mar)", spawning_months)
spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jan-Mar)", spawning_months, spawn_grid)
spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Jan-Mar)", spawning_months, spawn_grid)
spawn_sst <- extract_enviro_var(oisst_month_grid26, "SST (Jan-Mar)", spawning_months, spawn_grid)
spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Jan-Mar)", spawning_months, spawn_grid)

pelagic_pdo <- extract_enviro_var(pdo, "PDO (Jan-May)", c(spawning_months, pelagic_months))
pelagic_npgo <- extract_enviro_var(npgo, "NPGO (Jan-May)", c(spawning_months, pelagic_months))
pelagic_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Apr-May)", pelagic_months, sp_grid)
# pelagic_sst <- extract_enviro_var(bccm_surface_temperature(), "SST (Apr-May)", pelagic_months, sp_grid)
pelagic_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-May)", pelagic_months, sp_grid)
pelagic_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Apr-May)", pelagic_months, sp_grid)
pelagic_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Apr-May)", pelagic_months, sp_grid)


# cope.ns.b <- extract_enviro_var(cops.ns.boreal, "Boreal Copepods (North VI)")
# cope.ns.s <- extract_enviro_var(cops.ns.south, "Southern Copepods (North VI)")
# cope.ns.n <- extract_enviro_var(cops.ns.subarctic, "Subarctic copepods (North VI)")
cope.ss.b <- extract_enviro_var(cops.ss.boreal, "Copepods - Boreal (South VI)")
cope.ss.s <- extract_enviro_var(cops.ss.south, "Copepods - Southern (South VI)")
cope.ss.n <- extract_enviro_var(cops.ss.subarctic, "Copepods - Subarctic (South VI)")

cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Copepods - medium (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Copepods - small (VI shelf)")

cope.shelf.sb <- extract_enviro_var(cops.shelf.nonarctic, "Copepods - small  (VI shelf)")
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Copepods - large (VI shelf)")


# juv_pdo <- extract_enviro_var(pdo, "PDO (Jun-Dec)", juv_months)
# juv_npgo <- extract_enviro_var(npgo, "NPGO (Jun-Dec)", juv_months)
ssb0 <- extract_enviro_var(conspecific_ssb, "Lingcod SSB")
juv_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jun-Dec)", juv_months, juv_grid)
juv_t <- extract_enviro_var(bccm_bottom_temperature(),  "Seafloor temperature (Jun-Dec)", juv_months, juv_grid)
juv_sst <- extract_enviro_var(oisst_month_grid26, "SST (Jun-Dec)", juv_months, juv_grid)
juv_s <- extract_enviro_var(bccm_bottom_salinity(),  "Seafloor salinity (Jun-Dec)", juv_months, juv_grid)
juv_herr <- extract_enviro_var(herring_recuits,  "Herring recruitment")
herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")
# juv_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Jun-Dec)", juv_months, juv_grid)
# juv_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Current year)", c(spawning_months, pelagic_months), sp_grid)
juv_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Jan-Jun)", c(spawning_months, pelagic_months, condition_months), juv_grid)

ds <- bind_rows(
  npgo2
  # ,alpi0
  # ,npcbi0
  # ,npgo1
  ,spawn_o2
  ,spawn_t
  # ,spawn_sst
  ,spawn_s
  # ,pdo0
  # ,npgo0
  ,spawn_pdo
  ,spawn_npgo
)

dp <- bind_rows(
  pelagic_o2
  ,pelagic_sstoi
  ,pelagic_s
  ,pelagic_p
  # ,pelagic_pdo
  # ,pelagic_npgo
  ## maybe drop these since more habitat in the south?
  # ,cope.ns.b
  # ,cope.ns.s
  # ,cope.ns.n
  # ,cope.ss.b
  # ,cope.ss.s
  # ,cope.ss.n
  ,cope.shelf.b
  ,cope.shelf.s
  ,cope.shelf.n
  # ,cope.shelf.sb
)

dj <- bind_rows(
  # juv_pdo
  # ,juv_npgo
  juv_o2
  ,juv_t
  # ,juv_sst
  ,juv_s
  # ,juv_pp
  ,juv_herr
  ,herr_ssb
  # ,ssb0
)

dvr <- bind_rows(ds,dp,dj)

saveRDS(dvr, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))


cond_months <- c(4,5,6)

cond_pdo <- extract_enviro_var(pdo, "PDO (Apr-Jun)", cond_months)
cond_npgo <- extract_enviro_var(npgo, "NPGO (Apr-Jun)", cond_months)
cond_npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)") |> mutate(year = year +1)
cond_npgo2 <- extract_enviro_var(npgo, "NPGO (2 yrs prior)") |> mutate(year = year +2)
# cond_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (prior year)", c(spawning_months, pelagic_months, juv_months), sp_grid) |> mutate(year = year +1)
cond_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Apr-Jun)", cond_months, summer_grid)
cond_t <- extract_enviro_var(bccm_bottom_temperature(), "Seafloor temperature (Apr-Jun)", cond_months, summer_grid)
cond_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-Jun)", cond_months, summer_grid)
cond_s <- extract_enviro_var(bccm_bottom_salinity(), "Seafloor salinity (Apr-Jun)", cond_months, summer_grid)
herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")


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
  ,cond_sstoi
  ,herr_ssb
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

## Recruitment analysis PART 1----

r_start_year <- ## # first year with significant age data
r_end_year <- # last year informed by age or length data
c_start_year <- ## # first year with condition data
c_end_year <- ## # last year with condition data


## FRENCH argument doesn't matter until this point,
## so can also be changed here as needed
# FRENCH <- FALSE
# FRENCH <- TRUE


remove_outliers <- NULL
shortlist <- FALSE
source("analysis/03-correlations-w-recruitment-brms.R")


### if you have extreme outliers
# # try removing outliers
# remove_outliers <- 2016
# shortlist <- FALSE
# source("analysis/03-correlations-w-recruitment-brms.R")

## Recruitment analysis PART 2----
## once you've reviewed the part 1 results
## pick a shortlist from of recruitment variables that represent the strongest relationships

# shortlist <- TRUE
# dvs2 <- bind_rows(
#
# )
# saveRDS(dvs2, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))
#
# source("analysis/03-correlations-w-recruitment-brms.R")


## Condiiton analyses ----

## mesh cutoff, usually 20 for coastwide stocks, sometimes smaller for smaller stock areas
cutoff <- 20

# copy cond-index and cond-index-sims folders from gfcondition to data folder
# review condition rmd
c_start_year <- # first year in condition models
c_end_year <- # last year in condition models


control_list <- list(adapt_delta = 0.95)
which_cond_model1 <- "2025-02"
source("analysis/04-correlations-btw-rdev-condition-brms.R")


control_list <- list(adapt_delta = 0.9)
which_cond_model2 <- "2025-02-ld0c"
source("analysis/05-correlations-w-condition-brms.R")



final_year <- 2025 # for variable plotting
# source("analysis/02-plot-vars.R") # not working sourced
