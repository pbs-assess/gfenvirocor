# Prep data and folders for stock specific condition analysis
library(tidyverse)
library(pacea)
devtools::load_all()


species <- "Lingcod"
stock <- "Coastwide"
# final_year <- 2025
set_utm_crs <- 32609

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

# create directories ----
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/models/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# prep mcmc data
out_sum <- readRDS(paste0("stock-specific/",spp,"/data/sum.rdata"))
out_mcmc <- readRDS(paste0("stock-specific/",spp,"/data/mcmc.rdata"))

n_draws <- 100
format_mcmc(out_mcmc, species, stock, samples = n_draws)

# little environmental and age data pre-1995
year_range <- range(out_sum$RecDevs$Yr[out_sum$RecDevs$type=="Main_RecrDev"])

start_year <- 1978 # first year with significant age data
end_year <- year_range[2]

# shortlist <- TRUE
shortlist <- FALSE

# define depths of interest ----
# depth ranges for Love 2011
species_min <- 3
species_max <- 400

spawn_min <- 3
spawn_max <- 100

juv_min <- 20
juv_max <- 100

# depth range from our survey data encompassing 95% of this species’ biomass
summer_min <- 40
summer_max <- 278

spawning_months <- c(1,2,3)
pelagic_months <- c(4,5)
juv_months <- c(6,7,8,9,10,11,12)
condition_months_A <- spawning_months
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

# this just gets a bunch of annual averages
# source("analysis/xx-get-all-enviro-vars.R")
load("data/oisst_month_grid26.rda")

# which community variables are we interested in
euphausids <- NULL
# check options
# unique(herring_recruitment$region)
herring_stocks <- c("HG", "PRD", "CC", "WCVI")
copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")

source("analysis/01-get-community-vars.R")

spawn_pdo <- extract_enviro_var(pdo, "PDO (Jan-Mar)", spawning_months)
spawn_npgo <- extract_enviro_var(npgo, "NPGO (Jan-Mar)", spawning_months)
spawn_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Sea floor O2 (Jan-Mar)", spawning_months, spawn_grid)
spawn_t <- extract_enviro_var(bccm_bottom_temperature(), "Sea floor temperature (Jan-Mar)", spawning_months, spawn_grid)
spawn_s <- extract_enviro_var(bccm_bottom_salinity(), "Sea floor salinity (Jan-Mar)", spawning_months, spawn_grid)

pelagic_pdo <- extract_enviro_var(pdo, "PDO (Apr-May)", pelagic_months)
pelagic_npgo <- extract_enviro_var(npgo, "NPGO (Apr-May)", pelagic_months)
pelagic_o2 <- extract_enviro_var(bccm_surface_oxygen(), "Surface O2 (Apr-May)", pelagic_months, sp_grid)
# pelagic_sst <- extract_enviro_var(bccm_surface_temperature(), "SST (Apr-May)", pelagic_months, sp_grid)
pelagic_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-May)", pelagic_months, sp_grid)
pelagic_s <- extract_enviro_var(bccm_surface_salinity(), "Surface salinity (Apr-May)", pelagic_months, sp_grid)
pelagic_p <- extract_enviro_var(bccm_phytoplankton(), "Phytoplankton (Apr-May)", pelagic_months, sp_grid)
pelagic_pp <- extract_enviro_var(bccm_primaryproduction(), "Primary production (Apr-May)", pelagic_months, sp_grid)

# cope.ns.b <- extract_enviro_var(cops.ns.boreal, "Boreal Copepods (North VI)")
# cope.ns.s <- extract_enviro_var(cops.ns.south, "Southern Copepods (North VI)")
# cope.ns.n <- extract_enviro_var(cops.ns.subarctic, "Subarctic copepods (North VI)")
# cope.ss.b <- extract_enviro_var(cops.ss.boreal, "Boreal Copepods (South VI)")
# cope.ss.s <- extract_enviro_var(cops.ss.south, "Southern Copepods (South VI)")
# cope.ss.n <- extract_enviro_var(cops.ss.subarctic, "Subarctic copepods (South VI)")

cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Copepods - Boreal (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Copepods - Southern (VI shelf)")
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Copepods - Subarctic (VI shelf)")


juv_pdo <- extract_enviro_var(pdo, "PDO (Jun-Dec)", juv_months)
juv_npgo <- extract_enviro_var(npgo, "NPGO (Jun-Dec)", juv_months)
juv_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Sea floor O2 (Jun-Dec)", juv_months, juv_grid)
juv_t <- extract_enviro_var(bccm_bottom_temperature(),  "Sea floor temperature (Jun-Dec)", juv_months, juv_grid)
juv_s <- extract_enviro_var(bccm_bottom_salinity(),  "Sea floor salinity (Jun-Dec)", juv_months, juv_grid)
juv_herr <- extract_enviro_var(herring_recuits,  "Herring recruitment")


dvs <- bind_rows(
  spawn_pdo
  ,spawn_npgo
  ,spawn_o2
  ,spawn_t
  ,spawn_s
)

dvp <- bind_rows(
  pelagic_pdo
  ,pelagic_npgo
  ,pelagic_o2
  ,pelagic_sstoi
  ,pelagic_s
  ,pelagic_p
  ,pelagic_pp
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
)

dvj <- bind_rows(
  juv_pdo
  ,juv_npgo
  ,juv_o2
  ,juv_t
  ,juv_s
  ,juv_herr
)

dvr <- bind_rows(dvs,dvp,dvj)

saveRDS(dvr, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))

# dvs2 <- bind_rows(
#
# )
# saveRDS(dvs2, paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))

cond_months <- c(4,5,6)

cond_pdo <- extract_enviro_var(pdo, "PDO (Apr-Jun)", cond_months)
cond_npgo <- extract_enviro_var(npgo, "NPGO (Apr-Jun)", cond_months)
cond_npgo1 <- extract_enviro_var(npgo, "NPGO (prior year)") |> mutate(year = year +1)
cond_o2 <- extract_enviro_var(bccm_bottom_oxygen(), "Sea floor O2 (Apr-Jun)", cond_months, sp_grid)
cond_t <- extract_enviro_var(bccm_bottom_temperature(), "Sea floor temperature (Apr-Jun)", cond_months, sp_grid)
cond_sstoi <- extract_enviro_var(oisst_month_grid26, "SST (Apr-Jun)", cond_months, sp_grid)
herr_ssb <- extract_enviro_var(herring_ssb, "Herring SSB")


dvc <- bind_rows(
  cond_pdo
  ,cond_npgo
  ,cond_npgo1
  ,cond_o2
  ,cond_t
  ,cond_sstoi
  ,herr_ssb
  ,juv_herr
)

saveRDS(dvc, paste0("stock-specific/",spp,"/data/envrio-vars-for-condition.rds"))


length(sort(unique(data$type)))

if (shortlist) {
  colours <- c(5, 3, 2, 7, 8, 6)
  pal <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
} else {
  colours <- c(11, 5, 12, 3, 2, 1, 4, 9, 10, 7, 8, 6)
  pal <- RColorBrewer::brewer.pal(n = 12, name = "Paired")
  # plot(1:length(pal), pch = 20, cex = 4, col = pal)
  # change yellow to be more visible
  pal[11] <- "#E5E74C"
  # plot(1:length(pal), pch = 20, cex = 4, col = pal)
}

source("analysis/02-plot-vars.R")

source("analysis/03-correlations-w-recruitment-brms.R")



# ## set major areas ----
# ## this defines the "stock"
# ## here using all canadian waters
# major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09",
#                  "11", # bc offshore waters
#                  "71","72","73","74","75","76","77","99")
