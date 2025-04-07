# plot environmental variables
library(tidyverse)
# library(GGally)
theme_set(ggsidekick::theme_sleek())

scale_fact <- 20

# Explore covariates ----

# Hack for two of same colour in first plot
ds_labels <- sort(unique(ds$type))
ds_labels[ds_labels=="NPGO (2 yrs prior)"] <- "NPGO (2 yrs prior or Jan-Mar)"
ds_labels[ds_labels=="NPGO (Jan-Mar)"] <- "NPGO (2 yrs prior or Jan-Mar)"
ds_labels <- sort(unique(ds_labels))

(ev1 <- ds |>
   left_join(colkey) |>
    filter(year >= r_start_year)|>
   # mutate(colour = factor(colour, levels = unique(colkey$colour))) |>
   mutate(
     type2 = as.numeric(as.factor(type)),
     colour = fct_reorder(colour, type2)) |>
   # filter(year >= 1990) |>
   # mutate(type = factor(type, levels = c("ENSO", "PDO", "NPGO"))) %>%
   ggplot() +
   geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
   scale_colour_identity(labels = ds_labels, guide="legend") +
   scale_x_continuous(limits = c(start_year, final_year), breaks = seq(start_year, final_year, 5) ) +
   theme(
     axis.title = element_blank(),
     legend.justification=c(0, 1)) +
   labs(x = "Year", y = "Standardized index", colour = "Spawning"))

(ev2 <- dp |> left_join(colkey) |>
    filter(year >= r_start_year)|>
    mutate(
      type2 = as.numeric(as.factor(type)),
      colour = fct_reorder(colour, type2)) |>
    ggplot() +
      geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
      scale_colour_identity(labels = sort(unique(dp$type)), guide="legend") +
    # scale_colour_manual(values = RColorBrewer::brewer.pal(n = length(unique(dvp$type)), name = "Paired")) +
    scale_x_continuous(limits = c(start_year,final_year), breaks = seq(start_year, final_year, 5) ) +
    theme(
      axis.title = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "Pelagic"))

(ev3 <- dj |> left_join(colkey) |>
    filter(year >= r_start_year)|>
    mutate(
      type2 = as.numeric(as.factor(type)),
      colour = fct_reorder(colour, type2)) |>
    ggplot() +
    geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
    scale_colour_identity(labels = sort(unique(dj$type)), guide="legend") +
    # scale_colour_manual(values = RColorBrewer::brewer.pal(n = length(unique(dvj$type)), name = "Paired"))  +
    scale_x_continuous(limits = c(start_year,final_year), breaks = seq(start_year, final_year, 5) ) +
    theme(
      axis.title = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "Juvenile"))

(ev4 <- dvc |>
    left_join(colkey) |>
    filter(year >= c_start_year)|>
    mutate(
      type2 = as.numeric(as.factor(type)),
      colour = fct_reorder(colour, type2)) |>
    ggplot() +
    geom_line(aes(year, value, group = type, colour = colour), alpha = 0.7, linewidth = 1) +
    scale_colour_identity(labels = sort(unique(dvc$type)), guide="legend") +
    # scale_colour_manual(values = RColorBrewer::brewer.pal(n = length(unique(dvc$type)), name = "Paired"))  +
    scale_x_continuous(limits = c(start_year,final_year), breaks = seq(start_year, final_year, 5) ) +
    theme(
      axis.title.y = element_blank(),
      legend.justification=c(0, 1)) +
    labs(x = "Year", y = "Standardized value", colour = "Condition"))

y_lab_big <- ggplot() +
  annotate(geom = "text", x = 1, y = 1, size = 4,
           colour = "grey30",
           label = "Standardized annual values", angle = 90) +
  coord_cartesian(clip = "off")+
  theme_void()


y_lab_big + patchwork::wrap_elements(ev1/ev2/ev3/ev4) + patchwork::plot_layout(width = c(0.05,1))

ggsave(paste0("stock-specific/",spp,"/figs/ev-indices.png"), width = 9, height = 8.5)



## check_correlations() doesn't seem to work with sourced scripts
ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-spawn.png"),
  width = length(unique(ds$type))*2,
  height = length(unique(ds$type))*2,
  units = "in", res = 300, scaling = 1)
check_correlations(ds)
dev.off()
# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-spawn.png"),
#        width = length(unique(ds$type))*2,
#        height = length(unique(ds$type))*2)

ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-pelagic.png"),
  width = length(unique(dp$type))*2,
  height = length(unique(dp$type))*2,
  units = "in", res = 300, scaling = 1)
check_correlations(dp)
dev.off()

# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-pelagic.png"),
#        width = length(unique(dp$type))*1.75,
#        height = length(unique(dp$type))*1.75)
ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-juv.png"),
  width = length(unique(dj$type))*2,
  height = length(unique(dj$type))*2,
  units = "in", res = 300, scaling = 1)
check_correlations(dj)
dev.off()
# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-juv.png"),
#        width = length(unique(dj$type))*2,
#        height = length(unique(dj$type))*2)

# ragg::agg_png(
#   paste0("stock-specific/",spp,"/figs/variable-correlations-spawn-juv.png"),
#   width = length(unique(dj$type))*2,
#   height = length(unique(dj$type))*2,
#   units = "in", res = 300, scaling = 1)
#
# dsj <- bind_rows(ds,dj)
# check_correlations(dsj)
# dev.off()
# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-juv.png"),
#        width = length(unique(dj$type))*2,
#        height = length(unique(dj$type))*2)

ragg::agg_png(
  paste0("stock-specific/",spp,"/figs/variable-correlations-cond.png"),
  width = length(unique(dvc$type))*2,
  height = length(unique(dvc$type))*2,
  units = "in", res = 300, scaling = 1)
check_correlations(dvc)
dev.off()
# ggsave(paste0("stock-specific/",spp,"/figs/variable-correlations-cond.png"),
#        width = length(unique(dvc$type))*2,
#        height = length(unique(dvc$type))*2)

