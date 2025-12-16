# ===== Exploratory quadratic fits & plots (per env var) =====
# Fits response ~ a + b1*x + b2*x^2  (x = value_raw if present else value)
# Annotates each panel with y = a + b1·x + b2·x² and R², small, in a corner

library(tidyverse)
library(patchwork)
library(broom)

# Optional theme used in your codebase
if (requireNamespace("ggsidekick", quietly = TRUE)) {
  theme_set(ggsidekick::theme_sleek())
} else {
  theme_set(theme_minimal())
}

# ---- Inputs expected to exist in your session/environment ----
# spp, scenario, r_start_year, r_end_year, shortlist (TRUE/FALSE)
# Optional: FRENCH (TRUE/FALSE), remove_outliers (e.g., c(2016)),
#           colkey (data.frame: type, colour)
if (!exists("FRENCH")) FRENCH <- FALSE

# ---- Read data (same paths as your original script) ----
if (isTRUE(shortlist)) {
  dvs <- readRDS(paste0("stock-specific/", spp, "/data/envrio-vars-for-rdevs-shortlist.rds"))
} else {
  dvs <- readRDS(paste0("stock-specific/", spp, "/data/envrio-vars-for-rdevs.rds"))
}
ts <- readRDS(paste0("stock-specific/", spp, "/output/summary-", scenario, ".rds"))

# ---- Merge & filter time window ----
data <- left_join(ts, dvs) |>
  filter(year >= r_start_year, year <= r_end_year) |>
  drop_na()

# Define response and variable names
data$response  <- data[["rdev"]]
data$var_names <- data[["type"]]

# Choose x variable: use raw if available, else standardized
xvar <- if ("value_raw" %in% names(data)) "value_raw" else "value"

# Optional: drop outlier years
if (exists("remove_outliers") && !is.null(remove_outliers)) {
  data <- filter(data, !(year %in% remove_outliers))
}

# ---- Helpers ----
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# colour lookup per variable (falls back to grey)
get_colour <- function(v) {
  if (exists("colkey") && is.data.frame(colkey) &&
      "type" %in% names(colkey) && "colour" %in% names(colkey)) {
    cval <- colkey$colour[colkey$type == v]
    if (length(cval) == 1) return(cval)
  }
  "grey40"
}

# EN/FR label helper (only if rosettafish available)
lab_fr <- function(x) {
  if (FRENCH && requireNamespace("rosettafish", quietly = TRUE)) {
    rosettafish::en2fr(x, TRUE)
  } else x
}

fmt_num <- function(x, k = 3) ifelse(is.finite(x), formatC(x, digits = k, format = "f"), "NA")

# ---- Annotation appearance ----
stats_corner <- "tr"  # pick: "tl", "tr", "bl", "br"
stats_size   <- 2.6   # text size (smaller = less intrusive)
stats_alpha  <- 0.92  # label background transparency (0–1)

# ---- Loop over variables ----
vars    <- sort(unique(data$var_names))
plots   <- vector("list", length(vars))
models  <- vector("list", length(vars))
coefs   <- vector("list", length(vars))

for (i in seq_along(vars)) {
  v <- vars[i]
  dat <- filter(data, var_names == v)
  
  # Skip if too few points or zero variance in x/y
  if (nrow(dat) < 3 || sd(dat[[xvar]]) == 0 || sd(dat$response) == 0) {
    warning(sprintf("Skipping '%s' (insufficient variation or n < 3).", v))
    plots[[i]] <- NULL
    next
  }
  
  # ---- Fit quadratic: response ~ a + b1*x + b2*x^2 ----
  # Use raw=TRUE so coefficients are on x and x^2 (not orthogonal polynomials)
  form <- as.formula(paste("response ~ poly(", xvar, ", 2, raw = TRUE)", sep = ""))
  fit  <- lm(form, data = dat)
  models[[i]] <- fit
  
  # ---- Tidy stats ----
  tid <- broom::tidy(fit)
  gl  <- broom::glance(fit)
  
  # Extract a (Intercept), b1 (x), b2 (x^2)
  # When using poly(xvar, 2, raw=TRUE), terms are: (Intercept), poly(x, 2, raw = TRUE)1, ...2
  t1 <- paste0("poly(", xvar, ", 2, raw = TRUE)1")
  t2 <- paste0("poly(", xvar, ", 2, raw = TRUE)2")
  
  a  <- tid$estimate[tid$term == "(Intercept)"] %||% NA_real_
  b1 <- tid$estimate[tid$term == t1] %||% NA_real_
  b2 <- tid$estimate[tid$term == t2] %||% NA_real_
  
  # Store row for export
  coefs[[i]] <- tibble::tibble(
    var_names     = v,
    n             = nrow(dat),
    intercept     = a,
    b1_linear     = b1,
    b2_quadratic  = b2,
    r_squared     = gl$r.squared %||% NA_real_,
    adj_r_squared = gl$adj.r.squared %||% NA_real_
  )
  
  # ---- Build compact label (quadratic formula + R^2) ----
  # y = a + b1·x + b2·x²
  label_str <- sprintf("y = %s + %s·x + %s·x\u00B2\nR\u00B2 = %s",
                       fmt_num(a, 3), fmt_num(b1, 3), fmt_num(b2, 3),
                       fmt_num(gl$r.squared, 3))
  
  # Corner coordinates with a little padding
  xr <- range(dat[[xvar]], na.rm = TRUE); yr <- range(dat$response, na.rm = TRUE)
  x_pad <- 0.02 * diff(xr); y_pad <- 0.02 * diff(yr)
  pos <- switch(stats_corner,
                "tr" = list(x = xr[2] - x_pad, y = yr[2] - y_pad, h = 1, v = 1),
                "tl" = list(x = xr[1] + x_pad, y = yr[2] - y_pad, h = 0, v = 1),
                "br" = list(x = xr[2] - x_pad, y = yr[1] + y_pad, h = 1, v = 0),
                "bl" = list(x = xr[1] + x_pad, y = yr[1] + y_pad, h = 0, v = 0),
                list(x = xr[2] - x_pad, y = yr[2] - y_pad, h = 1, v = 1) # default "tr"
  )
  
  set_colour <- get_colour(v)
  
  # ---- Plot: scatter + quadratic lm + CI + small in-plot label ----
  plots[[i]] <- ggplot(dat, aes(x = .data[[xvar]], y = response)) +
    geom_point(alpha = 0.85, colour = set_colour) +
    geom_smooth(
      method  = "lm",
      formula = y ~ poly(x, 2, raw = TRUE),  # matches the fit above
      se      = TRUE,
      linewidth = 0.9,
      colour    = set_colour
    ) +
    annotate("label",
             x = pos$x, y = pos$y, hjust = pos$h, vjust = pos$v,
             label = label_str,
             size = stats_size,
             label.size = 0.15,
             fill = "white",
             alpha = stats_alpha) +
    labs(
      x = lab_fr(unique(dat$var_names)),
      y = NULL,
      colours = NULL, fill = NULL
    )
}

# ---- Save results ----
plots <- purrr::discard(plots, is.null)

saveRDS(coefs, paste0(
  "stock-specific/", spp, "/output/rdev-enviro-corr-coefs-lm-quadratic-",
  scenario, "-", length(plots), if (isTRUE(shortlist)) "-short", ".rds"
))
saveRDS(plots, paste0(
  "stock-specific/", spp, "/output/rdev-enviro-corr-plot-list-lm-quadratic-",
  scenario, "-", length(plots), if (isTRUE(shortlist)) "-short", if (FRENCH) "-FR", ".rds"
))
saveRDS(models, paste0(
  "stock-specific/", spp, "/output/rdev-enviro-corr-model-list-lm-quadratic-",
  scenario, "-", length(plots), if (isTRUE(shortlist)) "-short", ".rds"
))

# Combined coef table (ranked by R²)
coefs2 <- bind_rows(coefs) 

# filter out based on r2
if(!is.null(r2)){
  
  coefs2_arr <- coefs2 |>
    arrange(desc(r_squared)) |>
    filter(r_squared >= r2)
  
  topvars_qm <- coefs2_arr[1:min(nrow(coefs2_arr),max_vars), c("var_names", "r_squared")]
  
  var_ind <- which(coefs2$var_names %in% topvars_qm$var_names)
  
  print(coefs2_arr[1:min(nrow(coefs2_arr),max_vars),], n = min(nrow(coefs2_arr),max_vars))
  
} else {
  var_ind <- 1:nrow(coefs2)
  
  print(coefs2 |> arrange(desc(r_squared)), n=nrow(coefs2))
}

# ---- Panel plot (layout like your original) ----
y_lab_big <- ggplot() +
  annotate("text", x = 1, y = 1, size = 5, colour = "grey30",
           label = lab_fr("Recruitment deviations"), angle = 90) +
  coord_cartesian(clip = "off") +
  theme_void()

(pp <- ((y_lab_big |
           wrap_plots(gglist = plots[var_ind], ncol = 3) &
           theme(
             text = element_text(size = 12),
             plot.title = element_blank(),
             legend.position = "none",
             plot.tag.position = c(.2, .85)
           )) +
          plot_layout(widths = c(0.05, 2)))
)

# Save panel PNG
if (isTRUE(shortlist)) {
  ggsave(paste0(
    "stock-specific/", spp, "/figs", if (FRENCH) "-french",
    "/rdev-enviro-corr-timeseries-", scenario, "-lm-quadratic-",
    length(plots), if (!is.null(r2)) "sub", "-short.png"
  ), width = 9.5, height = 7.5)
} else {
  ggsave(paste0(
    "stock-specific/", spp, "/figs", if (FRENCH) "-french",
    "/rdev-enviro-corr-timeseries-", scenario, "-lm-quadratic-",
    length(plots), if (!is.null(r2)) "sub", ".png"
  ), width = 9.5, height = 14)
}

