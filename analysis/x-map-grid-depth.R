load("data/grid.rda")
library(ggsidekick)
# species_min <- 20
# species_max <- 450
sp_grid <- new_grid |> filter(depth_min >= species_min & depth_max <= species_max & max_depth < 0)
new_grid |> filter(depth_min > -100, depth_max < 1600, longitude > -134) |> ggplot() +
  geom_sf(aes(fill = depth_m, colour = depth_m)) +
  scale_fill_viridis_c(trans = fourth_root_power_trans(), direction = -1, breaks = c(0, 10, 100, 1000 )) +
  scale_color_viridis_c(trans = fourth_root_power_trans(), direction = -1, breaks = c(0, 10, 100, 1000 )) +
  guides(colour =guide_colorbar(reverse = TRUE), fill = guide_colorbar(reverse = TRUE)) +
  theme(legend.position = c(0.2,0.2))

sp_grid |> filter(longitude > -134) |> ggplot() +
  geom_sf(aes(fill = depth_m, colour = depth_m)) +
  scale_fill_viridis_c(trans = fourth_root_power_trans(), direction = -1) +
  scale_color_viridis_c(trans = fourth_root_power_trans(), direction = -1) +
  guides(colour =guide_colorbar(reverse = TRUE), fill = guide_colorbar(reverse = TRUE)) +
  theme(legend.position = c(0.2,0.2))

ggsave(paste0("stock-specific/",spp,"/figs/map-grid-depth.png"), width = 5, height = 5)
