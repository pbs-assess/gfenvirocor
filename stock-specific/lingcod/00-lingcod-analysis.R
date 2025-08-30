# Prep data and folders for stock specific condition analysis
library(tidyverse)
library(pacea)
# library(brms)
devtools::load_all()

FRENCH <- FALSE
# plotting options
theme_set(ggsidekick::theme_sleek())

species <- "Lingcod"
stock <- "Coastwide"
stock_name <- "Lingcod"
set_utm_crs <- 32609

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))


# create directories
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
if (FRENCH) dir.create(paste0("stock-specific/", spp, "/figs-french/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

## Prep SS3 outputs ----
out_sum <- readRDS(paste0("stock-specific/",spp,"/data/sum.rdata"))
n_draws <- 100
scenario <- out_sum$model_name

## only run if first time
format_ss3_summary(out_sum, species, stock)
out_mcmc <- readRDS(paste0("stock-specific/",spp,"/data/mcmc.rdata"))
format_mcmc(out_mcmc, species, stock, scenario, samples = n_draws)

## Set spatiotemporal scales ----

# define depths of interest
# depth ranges for Love 2011
species_min <- 3
species_max <- 400

spawn_min <- 3
spawn_max <- 100

juv_min <- 20
juv_max <- 75 # changed from 100, need to rerun recruitment stuff

# depth range from our survey data encompassing 95% of this species’ biomass
summer_min <- 40
summer_max <- 278

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
conspecific_ssb <- TRUE

# check options
# unique(herring_recruitment$region)
herring_stocks <- c("HG", "PRD", "CC", "WCVI")

copepod_regions <- NULL
# copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")
copepod_months <- c(4,5)
copepod_months <- c(3,4,5,6)

copepod_months <- c(1,2,3,4,5,6,7,8,9)

focal_month <- 4
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

cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Copepods - medium (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Copepods - small (VI shelf)")
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Copepods - large (VI shelf)")


ssb0 <- extract_enviro_var(conspecific_ssb, "Lingcod SSB")
juv_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Seafloor O2 (Jun-Dec)", juv_months, juv_grid)
juv_t <- extract_enviro_var(bccm_bottom_temperature(),  "Seafloor temperature (Jun-Dec)", juv_months, juv_grid)
juv_sst <- extract_enviro_var(oisst_month_grid26, "SST (Jun-Dec)", juv_months, juv_grid)
juv_s <- extract_enviro_var(bccm_bottom_salinity(),  "Seafloor salinity (Jun-Dec)", juv_months, juv_grid)
juv_herr <- extract_enviro_var(herring_recuits,  "Herring recruitment")
herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")
juv_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Jan-Jun)", c(spawning_months, pelagic_months, condition_months), juv_grid)

ds <- bind_rows(
  npgo2
  ,spawn_o2
  ,spawn_t
  ,spawn_s
  ,spawn_pdo
  ,spawn_npgo
)

dp <- bind_rows(
  pelagic_o2
  ,pelagic_sstoi
  ,pelagic_s
  ,pelagic_p
  ,cope.shelf.b
  ,cope.shelf.s
  ,cope.shelf.n
)

dj <- bind_rows(
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

dvs2 <- bind_rows(
  spawn_o2
  ,spawn_s
  ,pelagic_o2
  ,pelagic_sstoi
  ,pelagic_p
  ,npgo2
  ,cope.shelf.s
  ,herr_ssb
)
saveRDS(dvs2, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))

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
  ,cond_npgo2
  ,cond_o2
  ,cond_t
  ,cond_s
  ,cond_sstoi
  ,herr_ssb
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
year_range <- range(out_sum$RecDevs$Yr[out_sum$RecDevs$type=="Main_RecrDev"])

r_start_year <- 1975 # 1978 is first year with significant age data
r_end_year <- 2018


remove_outliers <- NULL

## FRENCH argument doesn't matter until this point,
## so can also be changed here as needed
# FRENCH <- FALSE
# FRENCH <- TRUE
## if you've loaded and updated the package and want to reload it without restarting R
## detach("package:rosettafish", unload=TRUE)
## but scripts should run without package being loaded

shortlist <- FALSE
source("analysis/03-correlations-w-recruitment-brms.R")
shortlist <- TRUE
source("analysis/03-correlations-w-recruitment-brms.R")


# try removing outliers
remove_outliers <- 2016
shortlist <- FALSE
source("analysis/03-correlations-w-recruitment-brms.R")

## Condition analyses ----
c_start_year <- 2002
c_end_year <- 2024

control_list <- list(adapt_delta = 0.95)
which_cond_model1 <- "2025-02"
source("analysis/04-correlations-btw-rdev-condition-brms.R")


control_list <- list(adapt_delta = 0.9)
which_cond_model2 <- "2025-02-ld0c"
source("analysis/05-correlations-w-condition-brms.R")



final_year <- 2025 # for variable plotting
# source("analysis/02-plot-vars.R") # not working sourced


