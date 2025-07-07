## generate biomass indices for copepod groups

library(sdmTMB)
load("data/SubarcticCopepodGroupsAbund_Biom_byRegion.RData")
# biomSN |> filter(mon %in% copepod_months) |>
#   group_by(yr, region) |>
#   summarise(effort = sum(volfilt.m3),
#             Cops.boreal = mean(Cops.boreal/volfilt.m3, na.rm = TRUE),
#             Cops.south = mean(Cops.south/volfilt.m3, na.rm = TRUE),
#             Cops.subarctic = mean(Cops.subarctic/volfilt.m3, na.rm = TRUE)
#             ) |> pivot_longer(cols = 4:6, names_to = "group")

dat <- biomSN |> filter(mon %in% copepod_months) |>
  mutate(year = yr,
         net.type = stats::relevel(factor(net.type), ref = "Bongo VNH"),
         twilight = stats::relevel(factor(twilight), ref = "Daylight"),
         month = stats::relevel(factor(mon), ref = "4"),
         logvolfilt = log(volfilt.m3)
  ) |>
  sdmTMB::add_utm_columns(ll_names = c("lon", "lat"), utm_crs = 32609, units = "km")

mesh <- make_mesh(dat, c("X", "Y"), cutoff = 15)
plot(mesh)

dat |>
  ggplot() + geom_point(aes(lon, lat, colour = region), alpha = 0.2) #+ facet_wrap(~yr)

syn_grid <- gfplot::synoptic_grid %>%
  mutate(Xm = X * 1e3, Ym = Y * 1e3,
         log_depth = log(depth)) %>%
  rename(area = cell_area) %>%
  sdmTMB::add_utm_columns(ll_names = c("Xm", "Ym"), ll_crs = 32609,
                          utm_names = c("longitude", "latitude"), utm_crs = 4326, units = "m") |>
  filter(latitude < 53 & longitude > -131) |>
  mutate(net.type = "Bongo VNH",
         twilight = "Daylight",
         mon = focal_month,
         month = factor(focal_month)
  )

syn_grid |>
  ggplot() +
  geom_point(aes(longitude, latitude), size = 2, alpha = 0.1) +
  geom_point(data = dat, aes(lon, lat, colour = region), alpha = 0.2)

gg <- sdmTMB::replicate_df(syn_grid, time_name = "year", time_values = sort(unique(dat$year)))

fit_sdmTMB <- function(formula){
 sdmTMB(
   formula,
  offset = "logvolfilt",
  spatial = "off",
  spatiotemporal = "rw",
  anisotropy = TRUE,
  family = tweedie(),
  time = "year",
  extra_time = c(1980, 1982, 2020),
  mesh = mesh,
  data = dat
)
}

set_k <- length(unique(dat$mon))-1

if (length(unique(dat$month)) > 9) {
  f <- response ~ net.type + twilight + s(mon, k = set_k, bs = "cc")
} else {
  if (length(unique(dat$month)) > 3) {
  f <- response ~ net.type + twilight + s(mon, k = set_k)
  } else {
    f <- response ~ net.type + twilight
  }
}

dat$response <- dat$Cops.boreal

mboreal <- fit_sdmTMB(f)

sanity(mboreal)
mboreal
plot_anisotropy(mboreal)

if (!all(sanity(mboreal, gradient_thresh = 0.005))) {
  warning("Boreal copepod model didn't converge.")
}

pred <- predict(mboreal, newdata = gg)

ggplot(pred, aes(X, Y)) + geom_point(aes(colour = epsilon_st)) +
  geom_point(data = dat, aes(size = sqrt(Cops.boreal/volfilt.m3)), alpha = 0.1) +
  facet_wrap(~year) +
  scale_color_gradient2()

ind <- get_index_split(mboreal,
                       newdata = gg,
                       predict_args = list(re_form_iid = NA),
                       nsplit = 1,
                       bias_correct = TRUE,
                       area = 4)
ind$group <- "Cops.boreal"

dat$response <- dat$Cops.south
msouth <- fit_sdmTMB(f)

sanity(msouth)
msouth
plot_anisotropy(msouth)

if (!all(sanity(msouth, gradient_thresh = 0.005))) {
  warning("Southern copepod model didn't converge.")
}

ind2 <- get_index_split(msouth,
                        newdata = gg,
                        predict_args = list(re_form_iid = NA),
                        nsplit = 1,
                        bias_correct = TRUE,
                        area = 4)
ind2$group <- "Cops.south"

dat$response <- dat$Cops.subarctic
msubarctic  <- fit_sdmTMB(f)
sanity(msubarctic)
msubarctic
plot_anisotropy(msubarctic)

if (!all(sanity(msubarctic, gradient_thresh = 0.005))) {
  warning("Subarctic copepod model didn't converge.")
}

ind3 <- get_index_split(msubarctic,
                        newdata = gg,
                        predict_args = list(re_form_iid = NA),
                        nsplit = 1,
                        bias_correct = TRUE,
                        area = 4)
ind3$group <- "Cops.subarctic"


inds <- bind_rows(ind, ind2, ind3) |> mutate(month = focal_month,
                                             months = paste0(copepod_months, collapse = "-"))

(g <- inds |>
    mutate(group = stats::relevel(factor(group, labels = c("Medium (Boreal)", "Small (Southern)", "Large (Subarctic)")), ref = "Small (Southern)")) |>
    ggplot(aes(year, est, ymin = lwr, ymax = upr)) +
    geom_line(aes(colour = group)) +
    geom_ribbon(aes(fill = group), alpha = 0.2) +
    ylim(0, NA) +
    labs(y = "Relative biomass index", x = "", colour = "Copepod group", fill = "Copepod group") +
    theme(legend.position = "inside", legend.position.inside = c(0.8,0.8))
)

ggsave(paste0("analysis/copepod-biomass-geostat-indices-",
              paste0(copepod_months, collapse = "-"),".png"), width = 6, height = 4)

saveRDS(inds, paste0("data/copepod-biomass-geostat-indices-",
                     paste0(copepod_months, collapse = "-"),".rds"))


nd <- expand.grid(
  net.type = "Bongo VNH",
  twilight = "Daylight",
  mon = c(1,2,3,4,5,6,7,8,9),
  year = max(dat$year) # a chosen year
)

p <- predict(mboreal, newdata = nd, se_fit = TRUE,
             re_form = NA, re_form_iid = NA)

p2 <- predict(msouth, newdata = nd, se_fit = TRUE,
              re_form = NA, re_form_iid = NA)

p3 <- predict(msubarctic, newdata = nd, se_fit = TRUE,
              re_form = NA, re_form_iid = NA)



p$group <- "Medium (Boreal)"
p2$group <- "Small (Southern)"
p3$group <- "Large (Subarctic)"

bind_rows(p, p2, p3) |>
  mutate(group = stats::relevel(factor(group,
    labels = c("Medium (Boreal)", "Small (Southern)", "Large (Subarctic)")),
                                ref = "Small (Southern)")) |>
  ggplot() +
  geom_pointrange(aes(as.factor(mon), exp(est),
                      ymin = exp(est - est_se*2),
                      ymax = exp(est + est_se*2),
                      colour = group)) +
  facet_wrap(~group, scales = "free_y") +
  theme(legend.position = "none") +
  labs(y = "Conditional effect on biomass", x = "Month")

ggsave(paste0("analysis/copepod-biomass-month-effect-",
              paste0(copepod_months, collapse = "-"),".png"), width = 6, height = 4)
