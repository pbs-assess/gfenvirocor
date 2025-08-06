# time series regressions for body condition
library(sdmTMB)
library(glmmTMB)
library(ggeffects)
library(tidyverse)
library(patchwork)
library(brms)
theme_set(ggsidekick::theme_sleek())
op <- options()

ts <- readRDS(paste0("stock-specific/",spp,"/output/summary-", scenario, ".rds"))
ts <- ts |> filter(year >= max(c_start_year, r_start_year - 1) & year <= min(c_end_year + 1, r_end_year))


f1 <- list.files(paste0(
  "stock-specific/",spp,"/data/cond-index/",
  which_cond_model1
), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS) %>%
  mutate(group = factor(group,
    levels = c("immatures", "mature males", "mature females"),
    labels = c("Immatures", "Mature males", "Mature females")
  ))
# d1$model <- "Density-agnostic"

min_est <- min(d1$est)
max_est <- max(d1$est)

yrs <- sort(unique(d1$year))

dd1 <- d1 |>
  group_by(group) |>
  mutate(
    value = (est - 1) / sd(est),
    type = ifelse(group == "Mature females", "Female condition",
      ifelse(group == "Mature males", "Male condition",
        "Immature condition"
      )
    ),
    group = factor(group,
      levels = c("immatures", "mature males", "mature females"),
      labels = c("Immatures", "Mature males", "Mature females")
    )
  ) |>
  ungroup() |>
  select(year, value, type)
dd1 <- na.omit(dd1)

dd2 <- dd1 |> filter(type != "Immature condition")

ts2 <- left_join(ts, dd2)
# ts2 <- na.omit(ts2)

# correlations between condition and recruitment ----

poly <- TRUE
lag <- TRUE

p <- list()
m <- list()
coefs <- list()

colours <- c(
  "#fde725",
  "#21918c"
)


if (lag) {
  data <- ts2 |>
    group_by(type) |>
    mutate(year = lead(year)) |>
    ungroup() |> ## lag condition
    filter(year >= max(c_start_year + 1 , r_start_year) & year <= min(c_end_year + 1, r_end_year))
  data <- na.omit(data)
} else {
  data <- ts2 |> filter(year >= max(c_start_year, r_start_year) & year <= min(c_end_year, r_end_year))
  data <- na.omit(data)
}

data$response <- data[["rdev"]]
data$var_names <- data[["type"]]

for (i in seq_along(sort(unique(data$var_names)))) {
  dat <- filter(data, var_names == sort(unique(data$var_names))[[i]])
  dat <- dat |> mutate(time = as.integer(seq_along(year)))

  if (poly) {
    m[[i]] <- brm(
      bf(response ~ poly(value, 2) + ar(time = time)),
      data = dat,
      iter = median_model_iter,
      chains = median_chains,
      control = control_list,
      prior = set_priors,
      # iter = 1000,
      # chains = 3,
      # control = list(adapt_delta = 0.9),
      # prior =
      #   c(
      #     set_prior("normal(0, 1)", class = "ar"),
      #     set_prior("normal(0, 10)", class = "b"),
      #     set_prior("student_t(3, 0, 2)", class = "sigma"),
      #     set_prior("normal(0, 10)", class = "Intercept")
      #   ),
      backend = "cmdstan"
    )
  } else {
    m[[i]] <- brm(
      bf(response ~ value + ar(time = time)),
      data = dat,
      iter = median_model_iter,
      chains = median_chains,
      control = control_list,
      prior = set_priors,
      # iter = 2000,
      # chains = 4,
      # control = list(adapt_delta = 0.9),
      # prior =
      #   c(
      #     set_prior("normal(0, 1)", class = "ar"),
      #     set_prior("normal(0, 10)", class = "b"),
      #     set_prior("student_t(3, 0, 2)", class = "sigma"),
      #     set_prior("normal(0, 10)", class = "Intercept")
      #   ),
      backend = "cmdstan"
    )
  }

  if (unique(dat$var_names) == "Immature condition") group <- "imm"
  if (unique(dat$var_names) == "Female condition") group <- "mat-fem"
  if (unique(dat$var_names) == "Male condition") group <- "mat-m"

  # retrieve MCMC/MVN sample data frames for 'response' and covariate:
  dd <- purrr::map_dfr(seq_len(n_draws), \(j) {

  .draw <- readRDS(paste0("stock-specific/",spp,"/output/mcmc/",scenario,"/df",j,".RData"))

  .cov <- readRDS(paste0(
      "stock-specific/",spp,"/data/cond-index-sims/",
      which_cond_model1, "/cond-index-sims-", group,
      "-", spp, "-", which_cond_model1, "-20-km.rds"
    )) |>
      filter(.iteration == j) |>
      mutate(
        value = (.value - 1) / sd(.value),
        type = unique(dat$var_names)
      ) |>
      select(year, value, type)

    if (lag) {
      # only one iteration and type, so shouldn't be grouped
      .cov <- .cov |>
        mutate(year = lead(year)) |>
        ungroup() ## lag condition
    }

    .d <- left_join(.draw, .cov) |> filter(year >= start_year & year <= end_year)
    .d <- na.omit(.d)
    .d$response <- .d[["rdev"]]
    .d$var_names <- .d[["type"]]
    .d <- filter(.d, var_names == unique(dat$var_names))
    .d <- .d |> mutate(time = as.integer(seq_along(year)))
    .d$original_iter <- j
    .d
  })


  dd_sum <- select(dat, year, value, response) |>
    rename(value_med = value, response_med = response) |>
    right_join(dd) |>
    group_by(year, value_med, response_med, time) |>
    summarise(
      y_max = quantile(response, probs = 0.975),
      y_min = quantile(response, probs = 0.025),
      x_max = quantile(value, probs = 0.975),
      x_min = quantile(value, probs = 0.025),
      response_new_med = median(response),
      value_new_med = median(value)
    )

  if (poly) {
    fits <- dd |>
      split(dd$original_iter) |>
      lapply(do_fit, control_list, set_priors)
  } else {
    fits <- dd |>
      split(dd$original_iter) |>
      lapply(do_fit, control_list, set_priors, poly = FALSE)
  }

  nd <- data.frame(value = seq(min(dd$value), max(dd$value), length.out = 200), time = NA)

  # make predictions for each:
  preds <- fits |> lapply(\(x) {
    posterior_epred(
      x,
      incl_autocor = FALSE,
      re.formula = NA,
      newdata = nd
    )
  })
  pred <- do.call(rbind, preds)

  # plot
  nd$est <- apply(pred, 2, median)
  nd$lwr <- apply(pred, 2, quantile, probs = 0.025)
  nd$upr <- apply(pred, 2, quantile, probs = 0.975)

  # nd$value_raw <- nd$value*dd$sd[1]+dd$mean[1]
  preds_full_posterior <- fits |> lapply(\(x) {
    posterior_predict(
      x,
      incl_autocor = FALSE,
      re.formula = NA,
      newdata = nd
    )
  })
  pred2 <- do.call(rbind, preds_full_posterior)

  nd$lwr2 <- apply(pred2, 2, quantile, probs = 0.025)
  nd$upr2 <- apply(pred2, 2, quantile, probs = 0.975)


  if (FRENCH) options(OutDec = ",")

  (p[[i]] <- ggplot() +
    geom_ribbon(
      data = nd, aes(value, ymin = lwr, ymax = upr),
      alpha = 0.5, fill = colours[[i]]
    ) +
    geom_ribbon(
      data = nd, aes(value, ymin = lwr2, ymax = upr2),
      alpha = 0.25, fill = colours[[i]]
    ) +
    # this is median of simulation draws
    geom_point(
      data = dd_sum, aes(value_new_med, response_new_med, alpha = time),
      colour = colours[[i]]
      # colour = "white"
    ) +
    # a place holder to set the axes correctly
    geom_linerange(
      data = dd_sum, aes(x = value_new_med, ymin = y_min, ymax = y_max),
      colour = colours[[i]]
    ) +
    geom_linerange(
      data = dd_sum, aes(y = response_new_med, xmin = x_min, xmax = x_max),
      colour = colours[[i]]
    ) +
    geom_line(
      data = nd, aes(value, est),
      colour = colours[[i]]
    ) +
    labs(
      x = rosettafish::en2fr(unique(dd$var_names), FRENCH), y = "",
      colour = "", fill = ""
    ) +
    ggtitle("") +
    ggsidekick::theme_sleek()
  )

  options(op)

  if (poly) {
    coefs[[i]] <- fits |> purrr::map_dfr(\(x) {
      a <- as.data.frame(x)
      data.frame(poly1 = a$b_polyvalue21, poly2 = a$b_polyvalue22, p = a$`ar[1]`, sigma = a$sigma)
    })
  } else {
    coefs[[i]] <- fits |> purrr::map_dfr(\(x) {
      a <- as.data.frame(x)
      data.frame(slope = a$b_value, p = a$`ar[1]`, sigma = a$sigma)
    })
  }
  coefs[[i]]$type <- unique(dd$var_names)[1]
}

if (lag) {
  if (poly) {
    saveRDS(coefs, paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-",
                          n_draws, "-poly-lag.rds"))
    saveRDS(p, paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-",
                      n_draws, "-poly-lag", if(FRENCH){"-FR"}, ".rds"))
    saveRDS(m, paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-",
                      n_draws, "-poly-lag.rds"))
  } else {
    saveRDS(coefs, paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-",
                          n_draws, "-lag.rds"))
    saveRDS(p, paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-",
                      n_draws, "-lag", if(FRENCH){"-FR"}, ".rds"))
    saveRDS(m, paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-",
                      n_draws, "-lag.rds"))
  }
} else {
  if (poly) {
    saveRDS(coefs, paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-",
                          n_draws, "-poly.rds"))
    saveRDS(p, paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-",
                      n_draws, "-poly", if(FRENCH){"-FR"}, ".rds"))
    saveRDS(m, paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-",
                      n_draws, "-poly.rds"))
  } else {
    saveRDS(coefs, paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-",
                          n_draws, ".rds"))
    saveRDS(p, paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-",
                      n_draws, "", if(FRENCH){"-FR"}, ".rds"))
    saveRDS(m, paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-",
                      n_draws, ".rds"))
  }
}

# # reload saved versions
# if(lag){
#   if(poly){
#     coefs <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-", n_draws, "-poly-lag.rds"))
#     p <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-", n_draws, "-poly-lag", if(FRENCH){"-FR"}, ".rds"))
# m <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-", n_draws, "-poly-lag.rds"))
#   } else{
#     coefs <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-", n_draws, "-lag.rds"))
#     p <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-", n_draws, "-lag", if(FRENCH){"-FR"}, ".rds"))
#     m <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-", n_draws, "-lag.rds"))
#   }
# } else{
#   if(poly){
#     coefs <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-", n_draws, "-poly.rds"))
#     p <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-", n_draws, "-poly", if(FRENCH){"-FR"}, ".rds"))
#     m <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-", n_draws, "-poly.rds"))
#   } else{
#     coefs <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-coefs-", n_draws, ".rds"))
#     p <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-plot-list-", n_draws, "", if(FRENCH){"-FR"}, ".rds"))
#     m <- readRDS(paste0("stock-specific/",spp,"/output/rdev-condition-corr-model-list-", n_draws, ".rds"))
#   }
# }

# check for convergence
lapply(m, get_ess)
lapply(m, max_rhat)

y_lab_big <- ggplot() +
  annotate(
    geom = "text", x = 1, y = 1, size = 4.5, colour = "grey30",
    label = paste0(rosettafish::en2fr("Recruitment deviations", FRENCH)), angle = 90
  ) +
  coord_cartesian(clip = "off") +
  theme_void()

if (FRENCH) options(OutDec = ",")
(pp <- ((y_lab_big |
  wrap_plots(gglist = p, ncol = 2) &
    theme(
      text = element_text(size = 12), legend.position = "none",
      plot.tag.position = c(.2, .77)
    )) +
  plot_annotation(tag_levels = list(c("", "A", "B"))) +
  plot_layout(widths = c(0.04, 1)))
)

ggsave(
  paste0(
    "stock-specific/",spp,"/figs", if(FRENCH){"-french"},
    "/rdev-condition-corr-timeseries-", scenario,
    "-start", start_year, "-", n_draws, "-draws-bmrs",
    if(poly){"-poly"}, if(lag){"-lag"}, ".png"
  ),
  width = 7, height = 3
)



coefs2 <- do.call(rbind, coefs)
head(coefs2)

coefs2 |>
  pivot_longer(1:(ncol(coefs2) - 1), values_to = "est", names_to = "coef") |>
  mutate(coef = factor(coef, levels = if(FRENCH){
    c("poly1", "poly2", "pente", "p", "sigma")
  }else{
    c("poly1", "poly2", "slope", "p", "sigma")
  }),
    type = rosettafish::en2fr(type, FRENCH)
  ) |>
  ggplot() +
  geom_violin(aes(forcats::fct_rev(type), est, fill = type), colour = NA, alpha = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  facet_grid(~coef, scales = "free") +
  labs(x = "", y = rosettafish::en2fr("Estimate", FRENCH),
       colour = rosettafish::en2fr("Variable", FRENCH),
       fill = rosettafish::en2fr("Variable", FRENCH)) +
  theme(legend.position = "none")


ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/rdev-condition-corr-coef-violins-", scenario, "-start",
              start_year, "-", n_draws, "-draws-bmrs",
              if(poly){"-poly"}, if(lag){"-lag"}, ".png"),
       width = if(FRENCH){7.7}else{6}, height = 1.5)


coefs2 |>
  pivot_longer(1:2, values_to = "est", names_to = "coef") |>
  mutate(coef = factor(coef, levels = c("poly1", "poly2"))) |>
  ggplot() +
  geom_violin(aes(forcats::fct_rev(type), est, fill = type), colour = NA, alpha = 0.7) +
  coord_flip() +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  scale_fill_manual(values = colours) +
  scale_colour_manual(values = colours) +
  facet_grid(rows = vars(coef), scales = "free") +
  labs(x = "", y = rosettafish::en2fr("Estimate", FRENCH),
       colour = rosettafish::en2fr("Variable", FRENCH),
       fill = rosettafish::en2fr("Variable", FRENCH)) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

ggsave(paste0("stock-specific/",spp,"/figs", if(FRENCH){"-french"},
              "/rdev-condition-corr-coef-violins-", scenario, "-start",
              start_year, "-", n_draws, "-draws-bmrs",
              if(poly){"-poly"}, if(lag){"-lag"}, "-inset.png"),
       width = 1.5, height = 2)


options(op)
