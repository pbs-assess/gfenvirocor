#' brms helper functions: fit once per response draw
#'
#' @param dat
#' @param poly
#'
#' @return
#' @export
#'
#' @examples
do_fit <- function(dat, poly = TRUE) {
  if(poly){
    tryCatch(brm(
      bf(response ~  poly(value, 2) + ar(time = time)),
      data = dat,
      iter = 1000,
      chains = 1,
      control = list(adapt_delta = 0.9),
      prior =
        # c(set_prior("normal(0, 0.5)", class = "ar"),
        c(set_prior("normal(0, 1)", class = "ar"),
          set_prior("normal(0, 10)", class = "b"),
          set_prior("student_t(3, 0, 2)", class = "sigma"),
          set_prior("normal(0, 10)", class = "Intercept")
        ),
      backend = "cmdstan"
    ))
  } else{
    tryCatch(brm(
      bf(response ~  value + ar(time = time)),
      data = dat,
      iter = 500,
      chains = 1,
      prior =
        # c(set_prior("normal(0, 0.5)", class = "ar"),
        c(set_prior("normal(0, 1)", class = "ar"),
          set_prior("normal(0, 10)", class = "b"),
          set_prior("student_t(3, 0, 2)", class = "sigma"),
          set_prior("normal(0, 10)", class = "Intercept")
        ),
      backend = "cmdstan"
    ))
  }
}

#' @export
max_rhat <- function(m){max(rhat(m))}

#' @export
get_ess <- function(m){
  ms <- summary(m)
  min(min(ms$spec_pars$Bulk_ESS),min(ms$spec_pars$Tail_ESS))
}
