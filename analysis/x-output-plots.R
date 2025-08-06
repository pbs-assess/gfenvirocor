# recruitment
library(sdmTMB)
library(glmmTMB)
library(ggeffects)
library(tidyverse)
library(patchwork)

theme_set(ggsidekick::theme_sleek())

mcmc <- TRUE

if(mcmc){
  # ts <- readRDS(paste0("data-generated/df-", scenario,".RData"))
  ts <- readRDS(paste0("stock-specific/",spp,"/output/summary-", scenario, ".rds"))
  mcmc <- "mcmc"
} else {
  ts <- readRDS(paste0("data-generated/timeseries-", scenario,".rds"))
  mcmc <- ""
}

# ts |>
#   ggplot() +
#   geom_path(aes(biomass/max(ts$biomass), p_by_biomass, colour = year),
#             size = 0.75,
#             arrow=arrow(angle=30,length=unit(0.1,"inches"),type="open")) +
#   scale_colour_viridis_c() +
#   labs(x = "Proportion of max biomass", y = "Production rate")




# ts2 <- readRDS(paste0("data-generated/timeseries-180.rds"))
# ts2$scenario
# ts <- ts |> filter(year < 2019 & year >= 1990)

ts |> ggplot() +
  geom_line(aes(year, ssb/mean(ts$ssb, na.rm = TRUE)), colour = "gold", linewidth = 2) +
  geom_point(aes(year, recruits/mean(ts$recruits, na.rm = TRUE)), colour = "purple", size =3) +
  # geom_point(data= ts2, aes(year, recruits/2), colour = "red", size =3) +
  # geom_point(aes(year, p_by_biomass*10000), colour = "red", size =3) +
  # scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
  labs(y = "Biomass/Recruits", x = "Year") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(paste0("stock-specific/",spp,"/figs/recruits-", mcmc, "-", scenario, ".png"), width = 4, height = 2.5)


n_draws <- 100

dd <- purrr::map_dfr(seq_len(n_draws), \(j) {
  .draw <- readRDS(paste0("stock-specific/",spp,"/output/mcmc/",scenario,"/df",j,".RData"))
  .d <- left_join(.draw, ts) |> filter(year >= r_start_year & year <= r_end_year)
  # .d <- na.omit(.d)
  .d$original_iter <- j
  .d
})


ts  |> filter(year <= end_year & year >= start_year) |> ggplot() +
  # geom_line(data = dd, aes(year, rdev, group = original_iter), colour = "purple", alpha = 0.1) +
  geom_violin(data = dd, aes(year, rdev, group = year), fill = "purple", alpha = 0.1) +
  geom_point(aes(year, rdev), colour = "purple", size =3) +
  # geom_point(data= ts2, aes(year, recruits/2), colour = "red", size =3) +
  # geom_point(aes(year, p_by_biomass*10000), colour = "red", size =3) +
  geom_hline(yintercept =  0 ) +
  # scale_x_continuous(breaks = c(1995,2000,2005,2010,2015)) +
  labs(y = "Recruitment deviations", x = "Year") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ggsave(paste0("stock-specific/",spp,"/figs/rdev-", mcmc, "-", scenario, ".png"), width = 6, height = 2.5)




#
# ts |> ggplot() +
#   # geom_line(aes(year, biomass), colour = "gold", linewidth = 2) +
#   geom_point(aes(year, rdev), colour = "purple", size =3) +
#   scale_x_continuous(breaks = c(1990,1995,2000,2005,2010,2015,2020)) +
#   labs(y = "Biomass/Recruits", x = "Year") +
#   theme(axis.text.y = element_blank(),
#         axis.ticks.y = element_blank())
