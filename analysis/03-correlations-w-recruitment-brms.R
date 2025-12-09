# time series regressions for recruitment deviations
library(glmmTMB)
library(brms)
library(tidyverse)
library(patchwork)

# # install cmdstanr to use instead of rstan as the backend:
# if (FALSE) {
#   install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
#   cmdstanr::install_cmdstan()
# }

theme_set(ggsidekick::theme_sleek())
op <- options()

p <- list()
m <- list()
coefs <- list()


if (shortlist) {
  dvs <- readRDS(paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs-shortlist.rds"))
} else {
  dvs <- readRDS(paste0("stock-specific/",spp,"/data/envrio-vars-for-rdevs.rds"))
}

ts <- readRDS(paste0("stock-specific/",spp,"/output/summary-", scenario, ".rds"))


data <- left_join(ts, dvs) |> filter(year >= r_start_year & year <= r_end_year)
data <- na.omit(data) # not needed but kept as a precaution

data$response <- data[["rdev"]]
data$var_names <- data[["type"]]

# n_draws <- 100

for (i in seq_along(sort(unique(data$type)))) {
  # for(i in 1:3) {
  dat <- filter(data, var_names == sort(unique(data$type))[[i]])

  print(dat$var_names[1])
  # retrieve a bunch of `.d` data frames above as MCMC samples from 'response' posterior:
  dd <- purrr::map_dfr(seq_len(n_draws), \(j) {
    .draw <- readRDS(paste0("stock-specific/",spp,"/output/mcmc/",scenario,"/df",j,".RData"))
    .d <- left_join(.draw, dvs) |> filter(year >= r_start_year & year <= r_end_year)
    .d <- na.omit(.d)
    .d <- .d |>
      group_by(type) |>
      mutate(time = as.integer(seq_along(year))) |>
      ungroup()

    .d$response <- .d[["rdev"]]
    .d$var_names <- .d[["type"]]
    .d <- filter(.d, var_names == sort(unique(data$type))[[i]])
    .d <- .d |> mutate(time = as.integer(seq_along(year)))
    .d$original_iter <- j
    .d
  })

  if(!is.null(remove_outliers)){
    # 2016 is a recruitment outlier for many Pacific groundfish species
    dd <- filter(dd, !(year %in% c(remove_outliers)))
    dat <- filter(dat, !(year %in% c(remove_outliers)))
  }


  dd_sum <- dd |>
    group_by(year, value_raw) |>
    summarise(
      max = quantile(response, probs = 0.025),
      min = quantile(response, probs = 0.975),
      response_new_med = median(response)
    )

  m[[i]] <- brm(
    bf(response ~ poly(value, 2) + ar(time = time)),
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

  summary(m[[i]])

    # still plot rhat > 1.01, but very faintly
    if (max(rhat(m[[i]])) > 1.01) {
      set_alpha <- 0.2
    } else {
      set_alpha <- 1
    }
    fits <- dd |>
      split(dd$original_iter) |>
      lapply(do_fit, control_list, set_priors)

    nd <- data.frame(value = seq(min(dd$value), max(dd$value), length.out = 200), time = NA)


    # check effective sample size of each model/chain:
    # browser()
    check_ess <- fits |> lapply(\(x) {
      get_ess(x)
    })

    # # also make faint if ESS of any less than 50
    if (min(unlist(check_ess)) < 50) {
    # # or make faint if ESS of more than % chains less than ??
    # if (quantile(unlist(check_ess),0.1) < 200) {
      set_alpha <- 0.2
    }

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

    nd$value_raw <- nd$value * dd$sd[1] + dd$mean[1]

    # make predictions for each:
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

    # browser()
    # set_colour <- pal[colours[i]]
    set_colour <- colkey[colkey$type == sort(unique(data$type))[[i]],]$colour


    if (FRENCH) options(OutDec = ",")
    (p[[i]] <- ggplot() +
      # a place holder to set the axes correctly
      geom_linerange(
        data = dd_sum, aes(value_raw, ymin = min, ymax = max),
        colour = set_colour
      ) +
      # points from MLE or median if provided
      geom_point(
        data = dat, aes(value_raw, response, alpha = time),
        colour = set_colour
      ) +
      # # attempt points from MCMC?
      # geom_point(
      #   data = dd_sum, aes(value_raw, response_new_med, alpha = year),
      #   colour = set_colour
      # ) +
      geom_line(
        data = nd, aes(value_raw, est),
        alpha = set_alpha,
        colour = set_colour
      ) +
      geom_ribbon(
        data = nd, aes(value_raw, ymin = lwr, ymax = upr),
        alpha = 0.5, fill = set_colour
      ) +
      geom_ribbon(
        data = nd, aes(value_raw, ymin = lwr2, ymax = upr2),
        alpha = 0.25, fill = set_colour
      ) +
      labs(
        x = rosettafish::en2fr(unique(dd$var_names), FRENCH), y = "",
        colour = "", fill = ""
      ) +
      ggtitle("") +
      ggsidekick::theme_sleek()
    )

    options(op)

    # combine coefs:
    coefs[[i]] <- fits |> purrr::map_dfr(\(x) {
      a <- as.data.frame(x)
      data.frame(poly1 = a$b_polyvalue21, poly2 = a$b_polyvalue22, p = a$`ar[1]`, sigma = a$sigma)
    })
    coefs[[i]]$var_names <- sort(unique(data$var_names))[[i]]
  # }

}

  saveRDS(coefs, paste0(
    "stock-specific/",spp,"/output/rdev-enviro-corr-coefs-",
    "start-", r_start_year, "-",
    n_draws, "-draws-", length(unique(data$type)),
    if(shortlist){"-short"}, ".rds"
  ))
    saveRDS(p, paste0(
      "stock-specific/",spp,"/output/rdev-enviro-corr-plot-list-",
      "start-", r_start_year, "-",
      n_draws, "-draws-", length(unique(data$type)),
      if(shortlist){"-short"}, if(FRENCH){"-FR"}, ".rds"
    ))

  saveRDS(m, paste0(
    "stock-specific/",spp,"/output/rdev-enviro-corr-model-list-",
    "start-", r_start_year, "-",
    n_draws, "-draws-", length(unique(data$type)),
    if(shortlist){"-short"}, ".rds"
  ))


## load saved
  coefs <- readRDS(paste0("stock-specific/",spp,"/output/rdev-enviro-corr-coefs-",
                          "start-", r_start_year, "-",
                          n_draws, "-draws-", length(unique(data$type)),
                          if(shortlist){"-short"}, ".rds"))
  p <- readRDS(paste0("stock-specific/",spp,"/output/rdev-enviro-corr-plot-list-",
                      "start-", r_start_year, "-",
                      n_draws, "-draws-", length(unique(data$type)),
                      if(shortlist){"-short"}, if(FRENCH){"-FR"}, ".rds"))
  m <- readRDS(paste0("stock-specific/",spp,"/output/rdev-enviro-corr-model-list-",
                      "start-", r_start_year, "-",
                      n_draws, "-draws-", length(unique(data$type)),
                      if(shortlist){"-short"}, ".rds"))

lapply(m, get_ess)
lapply(m, max_rhat)

if (FRENCH) options(OutDec = ",")
p <- p %>% discard(is.null)

y_lab_big <- ggplot() +
  annotate(
    geom = "text", x = 1, y = 1, size = 5, colour = "grey30",
    label = paste0(rosettafish::en2fr("Recruitment deviations", FRENCH)), angle = 90
  ) +
  coord_cartesian(clip = "off") +
  theme_void()


(pp <- ((y_lab_big |
  wrap_plots(gglist = p, ncol = 3) &
    scale_color_viridis_d(option = "D", direction = 1) &
    theme(
      text = element_text(size = 12),
      plot.title = element_blank(),
      legend.position = "none",
      plot.tag.position = c(.2, .85)
    )) +
  plot_annotation(tag_levels = list(c(
    "", "A", "B", "C", "D",
    "E", "F", "G", "H", "I",
    "J", "K", "L", "M", "N",
    "O", "P", "Q", "R", "S", "T", "U"
  ))) +
  plot_layout(widths = c(0.05, 2)))
)

if (shortlist) {
  set_width = 9.5
  set_height = 7.5

  (pp <- ((y_lab_big |
             wrap_plots(gglist = p, ncol = 3) &
             scale_color_viridis_d(option = "D", direction = 1) &
             theme(
               text = element_text(size = 12),
               plot.title = element_blank(),
               legend.position = "none",
               plot.tag.position = c(.2, .85)
             )) +
            plot_annotation(tag_levels = list(c(
              "", "A", "B", "C", "D",
              "E", "F", "G", "H", "I",
              "J", "K", "L", "M", "N",
              "O", "P", "Q", "R", "S", "T", "U"
            ))) +
            plot_layout(widths = c(0.05, 2)))
  )

  ggsave(paste0(
      "stock-specific/",spp,"/figs", if(FRENCH){"-french"}, "/rdev-enviro-corr-timeseries-",
      "start-", r_start_year, "-",
      scenario, "-", n_draws, "-draws-",
      length(unique(data$type)), "-short.png"
    ), width = set_width, height = set_height)
  } else {

  ggsave(paste0(
    "stock-specific/",spp,"/figs", if(FRENCH){"-french"}, "/rdev-enviro-corr-timeseries-",
    "start-", r_start_year, "-",
    scenario, "-", n_draws, "-draws-",
    length(unique(data$type)), ".png"
  ), width = 9.5, height = 14)

}

coefs2 <- do.call(rbind, coefs)
head(coefs2)


if (!shortlist) {
  g <- coefs2 |> left_join(colkey, by=c("var_names" = "type")) |>
    mutate(var_names = rosettafish::en2fr(var_names, FRENCH)) |>
    pivot_longer(1:4, values_to = "est", names_to = "coef") |>
    mutate(coef = factor(coef, levels = if(FRENCH){
      c("poly1", "poly2", "pente", "p", "sigma")
      }else{
        c("poly1", "poly2", "slope", "p", "sigma")
        })) |>
    ggplot() +
    geom_hline(yintercept = 0, colour = "darkgrey") +
    geom_violin(aes(forcats::fct_rev(var_names), est, fill = colour,
                    colour = colour), linewidth =0.1, alpha = 0.7) +
    # geom_violin(aes(forcats::fct_rev(var_names), est, fill = var_names), colour = NA, alpha = 0.7) +
    coord_flip() +
    scale_fill_identity() +
    scale_colour_identity() +
    # scale_fill_manual(values = pal[colours]) +
    # scale_colour_manual(values = pal[colours]) +
    facet_grid(~coef, scales = "free_x") +
    labs(x = "", y = rosettafish::en2fr("Estimate", FRENCH),
         colour = rosettafish::en2fr("Variable", FRENCH),
         fill = rosettafish::en2fr("Variable", FRENCH)) +
    theme(legend.position = "none")
  g

  ggsave(paste0(
    "stock-specific/",spp,"/figs", if(FRENCH){"-french"}, "/rdev-enviro-corr-coef-violins-",
    "start-", r_start_year, "-",
    scenario, "-", n_draws, "-draws-brms-",
    length(unique(data$type)), ".png"
  ), width = 8, height = 4)

  # if(FRENCH) {
  # saveRDS(dd_sum, paste0("stock-specific/",spp,"/output/rdev-uncertainty-range-FR.rds"))
  # }else{
  saveRDS(dd_sum, paste0("stock-specific/",spp,"/output/rdev-uncertainty-range.rds"))
  # }
}

  coefs2 |> left_join(colkey, by=c("var_names" = "type")) |>
    mutate(var_names = rosettafish::en2fr(var_names, FRENCH)) |>
    pivot_longer(1:2, values_to = "est", names_to = "coef") |>
    mutate(coef = factor(coef, levels = if(FRENCH){
      c("poly1", "poly2", "pente", "p", "sigma")
    }else{
      c("poly1", "poly2", "slope", "p", "sigma")
    })) |>
    ggplot() +
    geom_hline(yintercept = 0, colour = "darkgrey") +
    geom_violin(aes(forcats::fct_rev(var_names), est,
                    colour = colour,
                    fill = colour),
                linewidth =0.1,
                alpha = 0.7) +
    # geom_violin(aes(forcats::fct_rev(var_names), est, fill = var_names), colour = NA, alpha = 0.7) +
    coord_flip() +
    scale_fill_identity() +
    scale_colour_identity() +
    # scale_fill_manual(values = pal[colours]) +
    # scale_colour_manual(values = pal[colours]) +
    facet_grid(~coef, scales = "free_x") +
    labs(x = "", y = rosettafish::en2fr("Estimate", FRENCH),
         colour = rosettafish::en2fr("Variable", FRENCH),
         fill = rosettafish::en2fr("Variable", FRENCH)) +
    theme(legend.position = "none")

  if (shortlist) {
    ggsave(paste0(
      "stock-specific/",spp,"/figs", if(FRENCH){"-french"}, "/rdev-enviro-corr-coef-violins-",
      "start-", r_start_year, "-",
      scenario, "-", n_draws, "-draws-brms-",
      length(unique(data$type)), "-just-poly-short.png"
    ), width = 5, height = 2.5)

  } else {
    ggsave(paste0(
      "stock-specific/",spp,"/figs", if(FRENCH){"-french"}, "/rdev-enviro-corr-coef-violins-",
      "start-", r_start_year, "-",
      scenario, "-", n_draws, "-draws-brms-",
      length(unique(data$type)), "-just-poly.png"
    ), width = 5, height = 4)

  }

  options(op)


# if (shortlist) {
#
#   library(GGally)
#
#   dvsw <- dvs |>
#     select(year, type, value) |>
#     mutate(type = ifelse(type == "Sea surface salinity (Q1-3: all pelagic)", "Surface salinity (all pelagic)", type)) |>
#     pivot_wider(names_from = type, values_from = value) ## |> View()
#   unique(dvs$type)
#
#   ggpairs(dvsw,
#     columns = c(2:7),
#     upper = list(continuous = wrap(cor_func, method = "spearman", symbol = expression("\u03C1 ="))),
#     progress = FALSE
#   )
#
#   ggsave("stock-specific/",spp,"/figs/rdev-clim-variables-correlations.png", width = 11, height = 11)
# }

