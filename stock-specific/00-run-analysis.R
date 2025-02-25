# Prep data and folders for stock specific condition analysis
library(tidyverse)
devtools::load_all()


species <- "Lingcod"
final_year <- 2025

spp <- gsub(" ", "-", gsub("\\/", "-", tolower(species)))

# create directories ----
dir.create(paste0("stock-specific/", spp, "/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/data/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/figs/"), showWarnings = FALSE)
# dir.create(paste0("stock-specific/", spp, "/models/"), showWarnings = FALSE)
dir.create(paste0("stock-specific/", spp, "/output/"), showWarnings = FALSE)

# prep mcmc data


# out<- readRDS(paste0("stock-specific/",spp,"/data/mcmc.rdata"))


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
# source("analysis/01-get-all-enviro-vars.R")
load("data/oisst_month_grid26.rda")

# which community variables are we interested in
euphausids <- NULL
# check options
# unique(herring_recruitment$region)
herring_stocks <- c("HG", "PRD", "CC", "WCVI")
copepod_regions <- c("Southern Vancouver Island Shelf","Northern Vancouver Island Shelf")

source("analysis/02-get-community-vars.R")

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

cope.shelf.b <- extract_enviro_var(cops.shelf.boreal, "Boreal Copepods (VI shelf)")
cope.shelf.s <- extract_enviro_var(cops.shelf.south, "Southern Copepods (VI shelf)")
cope.shelf.n <- extract_enviro_var(cops.shelf.subarctic, "Subarctic copepods (VI shelf)")


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

scale_fact <- 20

ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-spawn.png"),
  width = length(unique(dvs$type))*2,
  height = length(unique(dvs$type))*2,
  units = "in", res = 300, scaling = 1)

check_correlations(dvs)
dev.off()

# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-spawn.png"),
#        width = length(unique(dvs$type))*2,
#        height = length(unique(dvs$type))*2)


ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-pelagic.png"),
  width = length(unique(dvp$type))*1.75,
  height = length(unique(dvp$type))*1.75,
  units = "in", res = 300, scaling = 1)

check_correlations(dvp)

dev.off()

# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-pelagic.png"),
#        width = length(unique(dvp$type))*1.75,
#        height = length(unique(dvp$type))*1.75)

check_correlations(dvj)

ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-juv.png"),
       width = length(unique(dvj$type))*2,
       height = length(unique(dvj$type))*2)

check_correlations(dvc)

ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-cond.png"),
       width = length(unique(dvc$type))*2,
       height = length(unique(dvc$type))*2)




# Explore covariates ----

(ev1 <- bind_rows(pdoA, oniA, npgoA) |>
   filter(year >= 1990) |>
   mutate(type = factor(type, levels = c("ENSO", "PDO", "NPGO"))) %>%
   ggplot() +
   geom_line(aes(year, value, colour = type), alpha = 0.7, linewidth = 1) +
   scale_colour_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Paired")[c(1,2,8)]) +
   scale_x_continuous(limits = c(1990,final_year), breaks = seq(1990, final_year, 5) ) +
   theme(
     axis.title = element_blank(),
     legend.justification=c(0, 1)) +
   labs(x = "Year", y = "Standardized index", colour = "Climate Indices"))

# ggsave("figs/climate-indices.png", width = 4, height = 2)


(ev2 <- bind_rows(sstA, ppA, ptA, sphA, so2A, ssaA, sstoiA) |>
    ggplot() +
    geom_line(aes(year, value, colour = type), alpha = 0.7, linewidth = 1) +
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3,4,2,10,8,6,5)])  +
    scale_x_continuous(limits = c(1990,final_year), breaks = seq(1990, final_year, 5) ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "BCCM Surface Variables"))
ev2


(ev3 <- bind_rows(tobA, bphA, o2A, bsaA) |>
    ggplot() +
    geom_line(aes(year, value, colour = type), alpha = 0.7, linewidth = 1) +
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(2,10,8,6)])  +
    scale_x_continuous(limits = c(1990,final_year), breaks = seq(1990, final_year, 5) ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "BCCM Sea Floor"))
ev3

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           colour = "grey30",
           label = "Standardized annual values", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()


y_lab_big + (ev1/ev2) + patchwork::plot_layout(width = c(0.1,1))

ggsave(paste0("stock-specific/",spp,"/figs/ev-indices.png"), width =7.5, height = 4.5)


# Explore covariates ----

(ev1 <- bind_rows(pdoA,  npgoA) |>
   filter(year >= 1990) |>
   mutate(type = factor(type, levels = c("ENSO", "PDO", "NPGO"))) %>%
   ggplot() +
   geom_line(aes(year, value, colour = type), alpha = 0.7, linewidth = 1) +
   scale_colour_manual(values = RColorBrewer::brewer.pal(n = 8, name = "Paired")[c(2,8)]) +
   scale_x_continuous(limits = c(1990,final_year), breaks = seq(1990, final_year, 5) ) +
   theme(
     axis.title = element_blank(),
     legend.justification=c(0, 1)) +
   labs(x = "Year", y = "Standardized index", colour = "Climate Indices"))

# ggsave("figs/climate-indices.png", width = 4, height = 2)


(ev2 <- bind_rows(sstA, tobA, ppA, ptA, o2A,) |>
    ggplot() +
    geom_line(aes(year, value, colour = type), alpha = 0.7, linewidth = 1) +
    scale_colour_manual(values = RColorBrewer::brewer.pal(n = 10, name = "Paired")[c(3,4,9,5,6)])  +
    scale_x_continuous(limits = c(1990,final_year), breaks = seq(1990, final_year, 5) ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "BCCM Variables"))
ev2


y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           colour = "grey30",
           label = "Standardized annual values", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()

y_lab_big + (ev1/ev2) + patchwork::plot_layout(width = c(0.1,1))

ggsave(paste0("stock-specific/",spp,"/figs/ev-indices-subset.png"), width =6, height = 4)



# Plot correlations -----
library(GGally)

dw <- bind_rows(pdoA, npgoA, sstA, tobA, ppA, ptA, o2A) |>
  select(year, type, value) |>
  pivot_wider(names_from = type, values_from = value)

ggpairs(dw, columns = c(2:8),
        upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 ='))),
        progress = FALSE)


ggsave(paste0("stock-specific/",spp,"/figs/clim-variables-correlations.png"), width = 11, height = 11)









## set major areas ----
## this defines the "stock"
## here using all canadian waters
major_areas <- c("01", "03", "04", "05", "06", "07", "08", "09",
                 "11", # bc offshore waters
                 "71","72","73","74","75","76","77","99")


set_utm_crs <- 32609

