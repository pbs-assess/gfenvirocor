# time series regressions for body condition
library(sdmTMB)
library(glmmTMB)
library(ggeffects)
library(tidyverse)
library(patchwork)
library(brms)
load_all()

theme_set(ggsidekick::theme_sleek())

f1 <- list.files(paste0(
  "stock-specific/",spp,"/data/cond-index/",
  which_cond_model2
), pattern = ".rds", full.names = TRUE)

d1 <- purrr::map_dfr(f1, readRDS) %>%
  mutate(group = factor(group,
                        levels = c("immatures", "mature males", "mature females"),
                        labels = c("Immatures", "Mature males", "Mature females")
  ))

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


# correlations with environment ----
dvc <- readRDS(paste0("stock-specific/",spp,"/data/envrio-vars-for-condition.rds"))
data <- dd1 |>
  rename(group = type, est_st = value) |>
  left_join(dvc, relationship = "many-to-many")

data$response <- data[["est_st"]]
data$var_names <- data[["type"]]

poly <- TRUE
using_posteriors <- TRUE
# poly <- FALSE
# using_posteriors <- FALSE

p <- list()
m <- list()
coefs <- list()

# n_draws <- 100

for (i in seq_along(sort(unique(data$var_names)))) {
# for(i in 2) {
  dat0 <- filter(data, var_names == sort(unique(data$var_names))[[i]])

  set_colour <- colkey[colkey$type == sort(unique(data$type))[[i]],]$colour

  for (g in seq_along(sort(unique(data$group)))) {
    idx <- length(unique(data$group)) * (i - 1) + g

    dat <- filter(dat0, group == unique(data$group)[[g]])


    if (unique(dat$group) == "Immature condition") {
      group <- "imm"
    }
    if (unique(dat$group) == "Female condition") {
      group <- "mat-fem"
    }
    if (unique(dat$group) == "Male condition") {
      group <- "mat-m"
    }

    # retrieve a bunch of `.d` data frames above as MCMC samples from 'response' posterior:
    dd <- purrr::map_dfr(seq_len(n_draws), \(j) {
      .draw <- readRDS(paste0(
        "stock-specific/",spp,"/data/cond-index-sims/", which_cond_model2, "/cond-index-sims-", group,
        "-", spp, "-", which_cond_model2, "-20-km.rds"
      )) |>
        filter(
          .iteration == j
        ) |>
        mutate(
          est_st = (.value - 1) / sd(.value),
          group = unique(dat$var_names)
        ) |>
        select(year, est_st)
      .d <- left_join(.draw, dvc)
      .d <- na.omit(.d)

      .d$response <- .d[["est_st"]]
      .d$var_names <- .d[["type"]]
      .d <- filter(.d, var_names == sort(unique(data$var_names))[[i]])
      .d <- .d |> mutate(time = as.integer(seq_along(year)))
      .d$original_iter <- j
      .d
    })

    dd_sum <- dd |>
      group_by(year, value_raw) |>
      summarise(
        max = quantile(response, probs = 0.025),
        min = quantile(response, probs = 0.975),
        response_new_med = median(response)
      )

    # browser()
    if(poly){
    m[[idx]] <- tryCatch(brm(
      bf(response ~ poly(value, 2) + ar(time = time)
         ),
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
    ))
    } else {
      m[[idx]] <- tryCatch(brm(
        bf(response ~ value + ar(time = time)),
        data = dat,
        iter = median_model_iter,
        chains = median_chains,
        control = control_list,
        prior = priors,
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
      ))
    }
    summary(m[[idx]])


    if (max(rhat(m[[idx]])) > 1.01) {
      set_alpha <- 0.2
    } else {
      set_alpha <- 1
    }

    nd <- data.frame(value = seq(min(dat$value), max(dat$value), length.out = 200), time = NA)

    if (using_posteriors) {
      if(poly){
      fits <- dd |>
        split(dd$original_iter) |>
        lapply(do_fit, control_list, set_priors)
      }else{
        fits <- dd |>
          split(dd$original_iter) |>
          lapply(do_fit, control_list, set_priors, poly = FALSE)
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

      (p[[idx]] <- ggplot() +
          # a place holder to set the axes correctly
          geom_linerange(
            data = dd_sum, aes(value_raw, ymin = min, ymax = max),
            colour = set_colour
          ) +
          geom_point(
            data = dat, aes(value_raw, response, alpha = time),
            colour = set_colour
          ))

      # combine coefs:
      coefs[[idx]] <- fits |> purrr::map_dfr(\(x) {
        a <- as.data.frame(x)
        data.frame(poly1 = a$b_polyvalue21, poly2 = a$b_polyvalue22)
      })
    } else {
      pred <- posterior_epred(
        m[[idx]],
        incl_autocor = FALSE,
        re.formula = NA,
        newdata = nd
      )

      # plot
      nd$est <- apply(pred, 2, median)
      nd$lwr <- apply(pred, 2, quantile, probs = 0.025)
      nd$upr <- apply(pred, 2, quantile, probs = 0.975)

      nd$value_raw <- nd$value * dd$sd[1] + dd$mean[1]

      # make predictions for each:
      pred2 <- posterior_predict(
        m[[idx]],
        incl_autocor = FALSE,
        re.formula = NA,
        newdata = nd
      )

      nd$lwr2 <- apply(pred2, 2, quantile, probs = 0.025)
      nd$upr2 <- apply(pred2, 2, quantile, probs = 0.975)

      (p[[idx]] <- ggplot() +
          # a place holder to set the axes correctly
          geom_linerange(
            data = dd_sum, aes(value_raw, ymin = min, ymax = max),
            colour = set_colour
          ) +
          geom_point(data = dat, aes(value_raw, response, alpha = time), colour = set_colour))

      # combine coefs:
      if(poly){
      a <- as.data.frame(m[[i]])
      coefs[[idx]] <- data.frame(poly1 = a$b_polyvalue21, poly2 = a$b_polyvalue22, ar1 = a$`ar[1]`, sigma = a$sigma)
      }
    }


    (p[[idx]] <- p[[idx]] +
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
          x = unique(dat$var_names), y = "",
          colour = "", fill = ""
        ) +
        ggsidekick::theme_sleek()
    )

    if (idx %in% seq_along(unique(data$group))) {
      p[[idx]] <- p[[idx]] + ggtitle(unique(dat$group))
    }

    if (!(idx %in% c(seq(from = 2, to = length(unique(data$group)) * length(unique(data$var_names)), by = 3)))) {
      p[[idx]] <- p[[idx]] + theme(axis.title.x = element_blank())
    }
    # }

    # combine coefs:
    if(poly){
    coefs[[idx]] <- fits |> purrr::map_dfr(\(x) {
      a <- as.data.frame(x)
      data.frame(poly1 = a$b_polyvalue21, poly2 = a$b_polyvalue22, p = a$`ar[1]`, sigma = a$sigma)
    })

    coefs[[idx]]$group <- unique(dat$group)[1]
    coefs[[idx]]$var_names <- unique(dat$var_names)[1]
    }
  }
}


saveRDS(p, paste0(
  "stock-specific/",spp,"/output/cond-enviro-corr-plot-list-poly-", n_draws, "-draws-",
  length(unique(data$var_names)), ".rds"
))

saveRDS(m, paste0(
  "stock-specific/",spp,"/output/cond-enviro-corr-model-list-poly-", n_draws, "-draws-",
  length(unique(data$var_names)), ".rds"
))

saveRDS(coefs, paste0("stock-specific/",spp,"/output/cond-enviro-corr-coefs-", n_draws, "-draws-",
                      length(unique(data$var_names)), ".rds"))


p <- readRDS(paste0(
  "stock-specific/",spp,"/output/cond-enviro-corr-plot-list-poly-", n_draws, "-draws-",
  length(unique(data$var_names)), ".rds"
))


p <- p %>% discard(is.null)

y_lab_big <- ggplot() +
  annotate(
    geom = "text", x = 1, y = 1, size = 4.5, colour = "grey30",
    label = paste0("Condition index (scaled)"), angle = 90
  ) +
  coord_cartesian(clip = "off") +
  theme_void()


(pp <- ((y_lab_big |
           wrap_plots(gglist = p, ncol = 3) &
           scale_color_viridis_d(option = "D", direction = 1) &
           theme(
             text = element_text(size = 12), plot.background = element_rect(fill = "transparent", color = NA),
             legend.position = "none"
           )) +
          plot_layout(widths = c(0.05, 2)))
)

ggsave(paste0(
  "stock-specific/",spp,"/figs/cond-enviro-corr-timeseries-", scenario, "-", n_draws, "-draws-brms-",
  length(unique(data$var_names)), ".png"
), width = 7, height = 10)


coefs2 <- do.call(rbind, coefs)
head(coefs2)

coefs2 |> left_join(colkey, by=c("var_names" = "type")) |>
  pivot_longer(1:4, values_to = "est", names_to = "coef") |>
  mutate(
    group = factor(group, levels = c("Immature condition", "Male condition", "Female condition"))
  ) |>
  mutate(coef = factor(coef, levels = c("poly1", "poly2", "slope", "p", "ar1", "sigma"))) |>
  ggplot() +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_violin(aes(forcats::fct_rev(var_names), est,
                  fill = colour, colour = colour),
              linewidth =0.1, alpha = 0.7) +
  # geom_violin(aes(forcats::fct_rev(var_names), est, fill = var_names), colour = NA, alpha = 0.7) +
  coord_flip() +
  scale_fill_identity() +
  scale_colour_identity() +
  # scale_fill_manual(values = pal[colours]) +
  # scale_colour_manual(values = pal[colours]) +
  facet_grid(group ~ coef, scales = "free") +
  labs(x = "", y = "Estimate", colour = "Variable", fill = "Variable") +
  theme(legend.position = "none")

ggsave(paste0(
  "stock-specific/",spp,"/figs/cond-enviro-corr-coef-violins-", scenario, "-", n_draws, "-draws-brms-",
  length(unique(data$var_names)), ".png"
), width = 7, height = 4.5)

coefs2 |>
  pivot_longer(1:2, values_to = "est", names_to = "coef") |>
  mutate(
    group = factor(group, levels = c("Immature condition", "Male condition", "Female condition"))
  ) |>
  mutate(coef = factor(coef, levels = c("poly1", "poly2", "slope", "p", "ar1", "sigma"))) |>
  ggplot() +
  geom_hline(yintercept = 0, colour = "darkgrey") +
  geom_violin(aes(forcats::fct_rev(group), est, fill = group, colour = group),
              linewidth =0.1, alpha = 0.7) +
  coord_flip() +
  scale_colour_viridis_d() +
  scale_fill_viridis_d() +
  facet_grid(var_names ~ coef, switch = "y", scales = "free") +
  theme(
    axis.ticks = element_blank(),
    axis.text.y = element_blank(),
    strip.text.y.left = element_text(angle = 0)
  ) +
  labs(x = NULL, y = "Estimate", colour = "", fill = "")

ggsave(paste0(
  "stock-specific/",spp,"/figs/cond-enviro-corr-coef-violins-by-group-", scenario, "-", n_draws, "-draws-brms-",
  length(unique(data$var_names)), ".png"
), width = 7, height = 4.5)



# # check convergence
# m <- m %>% discard(is.null)
#
# lapply(m, max_rhat)
# lapply(m, get_ess)
#
# # check posteriors
# pc <- list()
# yrep <- list()
# for (i in seq_along(m)){
#   df <- m[[i]]$data
#   yrep[[i]] <- brms::posterior_predict(m[[i]])
#   pc[[i]] <- bayesplot::ppc_dens_overlay(df$response, yrep[[i]])
#   # pc[[i]] <- bayesplot::ppc_intervals(df$response, yrep[[i]], x = df$value)
#   # pc[[i]] <- bayesplot::ppc_intervals(df$response, yrep[[i]], x = df$time)
# }
#
# wrap_plots(gglist = pc, ncol = 3) +
#   plot_layout(guides = "collect")
#
#
# ggsave(paste0(
#   "stock-specific/",spp,"/figs/pp-check-cond-enviro-corr-", scenario, "-", n_draws, "-draws-brms-",
#   length(unique(data$var_names)), ".png"
# ), width = 7, height = 10)



# library(GGally)
#
# dvcw <- dvc |>
#   select(year, type, value) |>
#   pivot_wider(names_from = type, values_from = value)
# unique(dvc$type)
#
# ggplot(dvcw) +
#   geom_point(aes(`Primary production (Q2: Apr-Jun)`, `Sea floor O2 (Q2: Apr-Jun)`))
#
# ggpairs(dvcw, columns = 2:8)
# ggsave("stock-specific/",spp,"/figs/condition-variable-correlations.png", width = 15, height = 15)
#
# ggpairs(dvcw, columns = 5:8)
# ggsave("stock-specific/",spp,"/figs/condition-ROMS-variable-correlations.png", width = 9, height = 9)
