# plot environmental variables

library(tidyverse)
# library(GGally)
theme_set(ggsidekick::theme_sleek())

# scale_fact <- 20

# Explore covariates ----


# --- plot ocean indixes --- #
# combine indices used 
# din <- bind_rows(
#   ao |> mutate(type = "AO",
#                date = as.Date(paste(year, month, "01", sep="-"))),
#   bi |> mutate(month = 6,
#                type = "BI",
#                date = as.Date(paste(year, month, "01", sep="-"))),
#   mei |> mutate(type = "MEI",
#                 date = as.Date(paste(year, month, "01", sep="-"))),
#   npgo |> mutate(type = "NPGO",
#                  date = as.Date(paste(year, month, "01", sep="-"))),
#   oni |> mutate(type = "ONI",
#                 date = as.Date(paste(year, month, "01", sep="-"))),
#   soi |> mutate(type = "SOI",
#                 date = as.Date(paste(year, month, "01", sep="-"))),
#   pdo |> mutate(type = "PDO",
#                 date = as.Date(paste(year, month, "01", sep="-"))),
# ) |> mutate(value = anomaly)

din <- bind_rows(
  extract_enviro_var(ao, "AO"), 
  extract_enviro_var(select(bi, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1), "BI"),  
  extract_enviro_var(mei, "MEI"), 
  extract_enviro_var(npgo, "NPGO"), 
  extract_enviro_var(select(oni, year, month, anomaly), "ONI"), 
  extract_enviro_var(soi, "SOI"),
  extract_enviro_var(pdo, "PDO")
)

dev <- bind_rows(
  extract_enviro_var(bccm_bottom_oxygen(), "Bot O2", spawning_months, spawn_grid),
  extract_enviro_var(bccm_bottom_salinity(), "Bot sal", spawning_months, spawn_grid),
  extract_enviro_var(bccm_bottom_ph(), "Bot pH", spawning_months, spawn_grid),
  extract_enviro_var(bccm_bottom_temperature(), "Bot temp", spawning_months, spawn_grid),
  extract_enviro_var(bccm_phytoplankton(), "Phyto", pelagic_months, sp_grid),
  extract_enviro_var(bccm_primaryproduction(), "PP", pelagic_months, sp_grid),
  extract_enviro_var(bccm_surface_oxygen(), "Surf O2", pelagic_months, sp_grid),
  extract_enviro_var(bccm_surface_temperature(), "Surf temp", pelagic_months, sp_grid),
)

nvars <- length(sort(unique(c(din$type, dev$type))))
colours <- c(seq(1:nvars))

# ---
# keep same colours as model output plots
colkey2 <- data.frame(type = sort(unique(c(din$type, dev$type))), id = colours, colour = NA)

colkey2[grepl("ALPI", colkey2$type),]$colour <- unique(colkey[grepl("ALPI", colkey$type),]$colour)
colkey2[grepl("AO", colkey2$type),]$colour <- unique(colkey[grepl("AO", colkey$type),]$colour)
colkey2[grepl("BI", colkey2$type),]$colour <- unique(colkey[grepl("Current bi", colkey$type),]$colour)
colkey2[grepl("MEI", colkey2$type),]$colour <- unique(colkey[grepl("MEI", colkey$type),]$colour)
colkey2[grepl("NPGO", colkey2$type),]$colour <- unique(colkey[grepl("NPGO", colkey$type),]$colour)
colkey2[grepl("NPI", colkey2$type),]$colour <- unique(colkey[grepl("NPI", colkey$type),]$colour)
colkey2[grepl("ONI", colkey2$type),]$colour <- unique(colkey[grepl("ONI", colkey$type),]$colour)
colkey2[grepl("PDO", colkey2$type),]$colour <- unique(colkey[grepl("PDO", colkey$type),]$colour)
colkey2[grepl("SOI", colkey2$type),]$colour <- unique(colkey[grepl("SOI", colkey$type),]$colour)
colkey2[grepl("Copepods", colkey2$type),]$colour <- unique(colkey[grepl("Copepods", colkey$type),]$colour)
colkey2[grepl("Euphausiid", colkey2$type),]$colour <- unique(colkey[grepl("Euphausiid", colkey$type),]$colour)
colkey2[grepl("Herring", colkey2$type),]$colour <- unique(colkey[grepl("Herring", colkey$type),]$colour)
colkey2[grepl("Phyto", colkey2$type),]$colour <- unique(colkey[grepl("Phytoplankton", colkey$type),]$colour)
colkey2[grepl("PP", colkey2$type),]$colour <- unique(colkey[grepl("PP", colkey$type),]$colour)
colkey2[grepl("O2", colkey2$type),]$colour <- unique(colkey[grepl("O2", colkey$type),]$colour)
colkey2[grepl("pH", colkey2$type),]$colour <- unique(colkey[grepl("pH", colkey$type),]$colour)
colkey2[grepl("sal", colkey2$type),]$colour <- unique(colkey[grepl("salinity", colkey$type),]$colour)
colkey2[grepl("temp", colkey2$type),]$colour <- unique(colkey[grepl("temperature", colkey$type),]$colour)

# ---
# new original colours 
colkey2 <- data.frame(type = sort(unique(c(din$type, dev$type))), id = colours)
pal <- scales::hue_pal()(length(unique(colkey2$id)))
# plot(1:length(pal), pch = 20, cex = 4, col = pal)
colours <- data.frame(colour = pal, id = rev(unique(colkey2$id)))
colkey2 <- left_join(colkey2, colours)
# plot(1:length(colkey2$type), pch = 20, cex = 4, col = colkey2$colour)


# Hack for two of same colour in first plot
din_labels <- sort(unique(din$type))

(ev1 <- din |>
  left_join(colkey2) |>
  filter(year >= r_start_year)|>
  mutate(
    type2 = as.numeric(as.factor(type)),
    colour = fct_reorder(colour, type2)) |>
  #
  ggplot() +
  geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
  scale_colour_identity(labels = rosettafish::en2fr(din_labels, FRENCH), guide="legend") +
  scale_x_continuous(limits = c(start_year, final_year), breaks = seq(start_year, final_year, 5)) +
  theme(
    axis.title = element_blank(),
    legend.justification=c(0, 1)) +
  labs(x = rosettafish::en2fr("Year", FRENCH),
       y = rosettafish::en2fr("Standardized index", FRENCH),
       colour = rosettafish::en2fr("Climate indices", FRENCH)
       ))

(ev2 <- dev |>
    left_join(colkey2) |>
    filter(year >= r_start_year)|>
    mutate(
      type2 = as.numeric(as.factor(type)),
      colour = fct_reorder(colour, type2)) |>
    #
    ggplot() +
    geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
    scale_colour_identity(labels = rosettafish::en2fr(sort(unique(dev$type)), FRENCH), guide="legend") +
    scale_x_continuous(limits = c(start_year, final_year), breaks = seq(start_year, final_year, 5)) +
    theme(
      axis.title = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = rosettafish::en2fr("Year", FRENCH),
         y = rosettafish::en2fr("Standardized index", FRENCH),
         colour = rosettafish::en2fr("BCCM variables", FRENCH)))

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           colour = "grey30",
           label = rosettafish::en2fr("Standardized annual values", FRENCH), angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()


y_lab_big + patchwork::wrap_elements(ev1/ev2) + patchwork::plot_layout(width = c(0.05,1))

ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/ev-indices.png"), width = 9, height = 8.5)

# --- plot spatial map of surface and bottom O2  --- #
#spawning O2
botO2 <- bccm_bottom_oxygen() |>
  select("2000_3", "2010_3")
botO2 <- botO2[spawn_grid,]
class(botO2) <- c("pacea_st", class(botO2))

plot(botO2, years.plot = c(2000,2010), months.plot = 3)
ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/spawn-O2.png"), width = 9, height = 5)

#surface pelagic O2
sO2 <- bccm_surface_oxygen() |>
  select("2000_8", "2010_8")
sO2 <- sO2[sp_grid,]
class(sO2) <- c("pacea_st", class(sO2))

plot(sO2, years.plot = c(2000,2010), months.plot = 8)
ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/sp_O2.png"), width = 9, height = 5)



# --- correlation plot --- #
dcor <- bind_rows(
  extract_enviro_var(ao, "AO"), 
  extract_enviro_var(select(bi, year, anomaly) |> rename(value = anomaly)  |> mutate(month = 1), "BI"),  
  extract_enviro_var(mei, "MEI"), 
  extract_enviro_var(npgo, "NPGO"), 
  extract_enviro_var(select(oni, year, month, anomaly), "ONI"), 
  extract_enviro_var(soi, "SOI"),
  extract_enviro_var(pdo, "PDO"),
  extract_enviro_var(bccm_bottom_temperature(), "Bot temp", c(4:6), spawn_grid),
  extract_enviro_var(bccm_bottom_salinity(), "Bot sal", c(4:6), spawn_grid),
  extract_enviro_var(bccm_phytoplankton(), "Phytop", pelagic_months, sp_grid),
  extract_enviro_var(bccm_surface_oxygen(), "Surf O2", pelagic_months, sp_grid),
  extract_enviro_var(bccm_surface_salinity(), "Surf sal", pelagic_months, sp_grid)
)


library(GGally)
dw <- dcor |> select(year, type, value) |>
  mutate(type = rosettafish::en2fr(type, FRENCH)) |>
  pivot_wider(names_from = type, values_from = value)

ggpairs(dw, columns = c(2:ncol(dw)),
        upper = list(continuous = wrap(cor_func, method = 'spearman', symbol = expression('\u03C1 ='), size = 3)),
        progress = FALSE, 
        ggtheme = theme_bw(base_size = 6))

ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/env_corplot.png"), width = 9, height = 9)


# ---
# --- Table of variable coefs for rmd ---#
# need to be run after 03-linear and quadratic scripts 
coefs_lm <- readRDS(here::here(paste0("stock-specific/dover-sole/output/rdev-enviro-corr-coefs-lm-Final11-147.rds"))) |>
  bind_rows()
coefs_qm <- readRDS(here::here(paste0("stock-specific/dover-sole/output/rdev-enviro-corr-coefs-lm-quadratic-Final11-147.rds"))) |>
  bind_rows()

dvrsub <- readRDS(here::here(paste0("stock-specific/dover-sole/data/envrio-vars-for-rdevs-dvrsub.rds")))
selected_vars <- unique(dvrsub$type)

# coefs_lm and coefs_qm are lists of tibbles; bind each set
coefs_lm2 <- coefs_lm |>
  transmute(
    var_names,
    model = "linear",
    r2    = r_squared
  )

coefs_qm2 <- coefs_qm |>
  transmute(
    var_names,
    model = "quadratic",
    r2    = r_squared
  )

coefs_all <- bind_rows(coefs_lm2, coefs_qm2) |>
  tidyr::pivot_wider(id_cols = var_names, names_from = model, values_from = r2)


# ---- 2. Collapse to broader "categories" (e.g. MEI, NPGO, Seafloor O2) ----
make_group <- function(x) {
  case_when(
    grepl("^MEI", x)                    ~ "MEI",
    grepl("^NPGO", x)                   ~ "NPGO",
    grepl("^PDO", x)                    ~ "PDO",
    grepl("^ONI", x)                    ~ "ONI",
    grepl("^SOI", x)                    ~ "SOI",
    grepl("^AO", x)                     ~ "AO",
    grepl("NPI", x)                     ~ "NPI",
    grepl("Current bifurcation", x)     ~ "Current bifurcation",
    grepl("Copepods", x)                     ~ "Copepods",
    TRUE ~ gsub("\\s*\\(.*", "", x)   # strip "(...)" so e.g. "Seafloor O2 (Jan-Mar)" -> "Seafloor O2"
  )
}

coefs_all_grp <- coefs_all |>
  mutate(group = make_group(var_names))

selected_vars <- data.frame(type = selected_vars) |>
  mutate(group = make_group(type)) 

# ---- 3. One row per group: highest R^2 and whether any member was selected ----
env_summary <- coefs_all_grp |>
  group_by(group) |>
  summarise(
    max_r2_lm   = max(linear, na.rm = TRUE),
    max_r2_qm   = max(quadratic, na.rm = TRUE),
    selected = any(var_names %in% unique(selected_vars$type)),
    .groups  = "drop"
  ) 

# ---- 4. Make a clean table for the Rmd ----
env_table <- env_summary |>
  mutate(
    Variable   = group,
    `Max lm R^2`  = round(max_r2_lm, 3),
    `Max qm R^2`  = round(max_r2_qm, 3),
    Selected   = if_else(selected, "\u2713", "")
  ) |>
  select(Variable, `Max lm R^2`, `Max qm R^2`, Selected)

saveRDS(env_table, paste0("stock-specific/",spp,"/figs/env_var_table.rds"))































# ---
# --- Table of variable coefs for rmd ---#
# need to be run after 03-linear and quadratic scripts 
coefs_lm <- readRDS(paste0("stock-specific/",spp,"/output/rdev-enviro-corr-coefs-lm-", 
                           scenario, "-", length(plots), if (isTRUE(shortlist)) "-short", ".rds")) |>
  bind_rows()
coefs_qm <- readRDS(paste0("stock-specific/",spp,"/output/rdev-enviro-corr-coefs-lm-quadratic-", 
                           scenario, "-", length(plots), if (isTRUE(shortlist)) "-short", ".rds")) |>
  bind_rows()

selected_vars <- unique(dvrsub$type)

# coefs_lm and coefs_qm are lists of tibbles; bind each set
coefs_lm2 <- coefs_lm |>
  transmute(
    var_names,
    model = "linear",
    r2    = r_squared
  )

coefs_qm2 <- coefs_qm |>
  transmute(
    var_names,
    model = "quadratic",
    r2    = r_squared
  )

coefs_all <- bind_rows(coefs_lm2, coefs_qm2) |>
  pivot_wider(id_cols = var_names, names_from = model, values_from = r2)


# ---- 2. Collapse to broader "categories" (e.g. MEI, NPGO, Seafloor O2) ----
make_group <- function(x) {
  case_when(
    grepl("^MEI", x)                    ~ "MEI",
    grepl("^NPGO", x)                   ~ "NPGO",
    grepl("^PDO", x)                    ~ "PDO",
    grepl("^ONI", x)                    ~ "ONI",
    grepl("^SOI", x)                    ~ "SOI",
    grepl("^AO", x)                     ~ "AO",
    grepl("Current bifurcation", x)     ~ "Current bifurcation",
    TRUE ~ gsub("\\s*\\(.*", "", x)   # strip "(...)" so e.g. "Seafloor O2 (Jan-Mar)" -> "Seafloor O2"
  )
}

coefs_all_grp <- coefs_all |>
  mutate(group = make_group(var_names))

selected_vars <- data.frame(type = selected_vars) |>
  mutate(group = make_group(type)) 

# ---- 3. One row per group: highest R^2 and whether any member was selected ----
env_summary <- coefs_all_grp |>
  group_by(group) |>
  summarise(
    max_r2_lm   = max(linear, na.rm = TRUE),
    max_r2_qm   = max(quadratic, na.rm = TRUE),
    selected = any(var_names %in% unique(selected_vars$type)),
    .groups  = "drop"
  ) 

# ---- 4. Make a clean table for the Rmd ----
env_table <- env_summary |>
  mutate(
    Variable   = group,
    `Max lm R^2`  = round(max_r2_lm, 3),
    `Max qm R^2`  = round(max_r2_qm, 3),
    Selected   = if_else(selected, "\u2713", "")
  ) |>
  select(Variable, `Max lm R^2`, `Max qm R^2`, Selected)

knitr::kable(
  env_table,
  caption = "Summary of variables grouped by category, showing the maximum R^2 across linear and quadratic models and whether any member of the group was selected (R^2 > 0.1)."
)

