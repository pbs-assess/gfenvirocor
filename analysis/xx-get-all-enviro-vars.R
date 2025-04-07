# get roms covariates from pacea-data-main and using their spatial_average function
# updated here with a default polygon for whole outer coast because I couldn't install the package
# remotes::install_github("pbs-assess/pacea") ## not working so retrieved files from github
library(tidyverse)
library(pacea)
theme_set(ggsidekick::theme_sleek())


# load("../pacea-data-main/data/bccm_primaryproduction_01.rds")
# pp <- spatial_average(bccm_primaryproduction, area = area)
pp <- spatial_average(bccm_primaryproduction(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_primary_production.rds"))
# pp <- spatial_average(bccm_primaryproduction(), area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_primary_production_spawn.rds"))
pp <- spatial_average(bccm_primaryproduction(), area = juv_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_primary_production_juv.rds"))
pp <- spatial_average(bccm_primaryproduction(), area = summer_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_primary_production_summer.rds"))

# surface plankton, only useful for pelagic larval periods
# load("../pacea-data-main/data/bccm_phytoplankton_01.rds")
# pp <- spatial_average(bccm_phytoplankton, area = area)
pp <- spatial_average(bccm_phytoplankton(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_phytoplankton.rds"))
# pp <- spatial_average(bccm_phytoplankton(), area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_phytoplankton_spawn.rds"))
# pp <- spatial_average(bccm_phytoplankton(), area = juv_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_phytoplankton_juv.rds"))
# pp <- spatial_average(bccm_phytoplankton(), area = summer_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_phytoplankton_summer.rds"))

# load("../pacea-data-main/data/bccm_bottom_oxygen_01.rds")
# pp <- spatial_average(bccm_bottom_oxygen, area = area)
pp <- spatial_average(bccm_bottom_oxygen(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_oxygen.rds"))
pp <- spatial_average(bccm_bottom_oxygen(), area = spawn_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_oxygen_spawn.rds"))
pp <- spatial_average(bccm_bottom_oxygen(), area = juv_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_oxygen_juv.rds"))
pp <- spatial_average(bccm_bottom_oxygen(), area = summer_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_oxygen_summer.rds"))

# load("../pacea-data-main/data/bccm_surface_oxygen_01.rds")
# pp <- spatial_average(bccm_surface_oxygen, area = area)
pp <- spatial_average(bccm_surface_oxygen(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_oxygen.rds"))
# pp <- spatial_average(bccm_surface_oxygen(), area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_oxygen_spawn.rds"))
# pp <- spatial_average(bccm_surface_oxygen(), area = juv_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_oxygen_juv.rds"))
# pp <- spatial_average(bccm_surface_oxygen(), area = summer_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_oxygen_summer.rds"))

# load("../pacea-data-main/data/bccm_bottom_temperature_01.rds")
# pp <- spatial_average(bccm_bottom_temperature, area = area)
pp <- spatial_average(bccm_bottom_temperature(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_temperature.rds"))
pp <- spatial_average(bccm_bottom_temperature(), area = spawn_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_temperature_spawn.rds"))
pp <- spatial_average(bccm_bottom_temperature(), area = juv_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_temperature_juv.rds"))
pp <- spatial_average(bccm_bottom_temperature(), area = summer_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_temperature_summer.rds"))

# load("../pacea-data-main/data/bccm_surface_temperature_01.rds")
# pp <- spatial_average(bccm_surface_temperature(), area = area)
pp <- spatial_average(bccm_surface_temperature(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature.rds"))
# pp <- spatial_average(bccm_surface_temperature(), area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_spawn.rds"))
# pp <- spatial_average(bccm_surface_temperature(), area = juv_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_juv.rds"))
# pp <- spatial_average(bccm_surface_temperature(), area = summer_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_summer.rds"))

load("data/oisst_month_grid26.rda")
pp <- spatial_average(oisst_month_grid26, area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_oi.rds"))
pp <- spatial_average(oisst_month_grid26, area = spawn_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_oi_spawn.rds"))
pp <- spatial_average(oisst_month_grid26, area = juv_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_oi_juv.rds"))
pp <- spatial_average(oisst_month_grid26, area = summer_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_temperature_oi_summer.rds"))

# load("../pacea-data-main/data/bccm_surface_salinity_01.rds")
# pp <- spatial_average(bccm_surface_salinity, area = area)
pp <- spatial_average(bccm_surface_salinity(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_salinity.rds"))
# pp <- spatial_average(bccm_surface_salinity, area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_salinity_spawn.rds"))
# pp <- spatial_average(bccm_surface_salinity, area = summer_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_salinity_summer.rds"))

# load("../pacea-data-main/data/bccm_surface_ph_01.rds")
# pp <- spatial_average(bccm_surface_salinity, area = area)
pp <- spatial_average(bccm_surface_ph(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_pH.rds"))
# pp <- spatial_average(bccm_surface_ph(), area = spawn_grid$geometry)
# saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_surface_pH_spawn.rds"))

# load("../pacea-data-main/data/bccm_bottom_ph_01.rds")
# pp <- spatial_average(bccm_surface_salinity, area = area)
pp <- spatial_average(bccm_bottom_salinity(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_salinity.rds"))
pp <- spatial_average(bccm_bottom_salinity(), area = spawn_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_salinity_spawn.rds"))

pp <- spatial_average(bccm_bottom_ph(), area = sp_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_pH.rds"))
pp <- spatial_average(bccm_bottom_ph(), area = spawn_grid$geometry)
saveRDS(pp, paste0("stock-specific/",spp,"/data/cw_bottom_pH_spawn.rds"))



# Get mean for a specific set of months and standardize ----
# these are now all potentially occupied depths year-round
pp_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_primary_production.rds"))
pt_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_phytoplankton.rds"))
sst_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_surface_temperature.rds"))
sstoi_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_surface_temperature_oi.rds"))
tob_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_bottom_temperature.rds"))
do_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_bottom_oxygen.rds"))
so2_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_surface_oxygen.rds"))
sph_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_surface_pH.rds"))
ssa_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_surface_salinity.rds"))
bph_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_bottom_pH.rds"))
bsa_monthly <- readRDS(paste0("stock-specific/",spp,"/data/cw_bottom_salinity.rds"))


npgoA <- pacea::npgo |> rename(value = anomaly) |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "NPGO")
soiA <- pacea::soi |> rename(value = anomaly) |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "SOI")
oniA <- pacea::oni |> mutate(value = anomaly) |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "ENSO")
npiA <- pacea::npi_monthly |> filter_months(c(1,2,3), "NPI")
pdoA <- pacea::pdo |> rename(value = anomaly) |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "PDO")
ppA <- pp_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Primary production")
ptA <- pt_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Phytoplankton")
sstA <- sst_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea surface temperature")
sstoiA <- sstoi_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea surface temperature (OI)")
tobA <- tob_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea floor temperature")
o2A <- do_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), type = "Sea floor O2")
so2A <- so2_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea surface O2")
ssaA <- ssa_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea surface salinity")
sphA <- sph_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea surface pH")
bsaA <- bsa_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea floor salinity")
bphA <- bph_monthly |>
  filter_months(c(1,2,3,4,5,6,7,8,9,10,11,12), "Sea floor pH")

