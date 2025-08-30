# format MCMC output - based on format-mcmc for SS outputs
# Author: Robyn Forrest August 26 2025

format_mcmc_iscam <- function(data,
  species,
  stock,
  scenario = "base",
  samples = 10,
  seed = 10,
  burnin=0,
  main_commercial_gear=1 # main commercial gear
  ) {

  set.seed(seed)

  age_recruited <- data$dat$start.age

  # Some of the mcmc outputs are in different formats so just get the ones we want
  mcmc_data <- list()
  mcmc_data$ssb <- data$mcmc$sbt[[1]]
  mcmc_data$recruits <- data$mcmc$rt[[1]]
  mcmc_data$rdev <- data$mcmc$rdev[[1]]

  # TODO: check how this is used and which number we actually want.
  #mcmc_data$harvest_rate <- data$mcmc$ut[[1]][[main_commercial_gear]]
  mcmc_data$harvest_rate <- data$mcmc$ft[[1]][[main_commercial_gear]] # Not sure if you want Ft or Ut??
  #View( mcmc_data$ssb)

  nsamp <- nrow(mcmc_data[[1]])

  # Remove burn in samples
  if(burnin>0){
    for(i in 1:length(mcmc_data)){
      mcmc_data[[i]] <- mcmc_data[[i]][(burnin+1):nsamp,]
      }
    }

  # rewrite nsamp
  nsamp<-nrow(mcmc_data[[1]])

  for(s in 1:samples){

  sample_row <- sample(1:nsamp,size=1)

  rdev <- mcmc_data$rdev[sample_row,] |>
    rename_with(~str_replace(., "Main_RecrDev_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "rdev") |>
    mutate(year = as.numeric(year) - age_recruited)

  ssb <- mcmc_data$ssb[sample_row,] |>
    rename_with(~str_replace(., "SSB_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "ssb") |>
    mutate(year = as.numeric(year))

  recruits <- mcmc_data$recruits[sample_row,] |>
    rename_with(~str_replace(., "Recr_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "recruits") |>
    mutate(year = as.numeric(year) - age_recruited)

  # This should be called fishing_mortality since it is sampling F not U
  fheader<-paste0("ft1_gear",main_commercial_gear,"_")
  harvest_rate <- mcmc_data$harvest_rate[sample_row,] |>
    rename_with(~str_replace(., fheader, "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "harvest_rate")|>
    mutate(year = as.numeric(year))

  df <- left_join(ssb, recruits) |> left_join(rdev) |> left_join(harvest_rate)
  df$species <- species
  df$stock <- stock
  df$scenario <- scenario
  # df$year <- as.numeric(df$year)

  df <- filter(df, !is.na(year))

  dir.create(paste0("stock-specific/", spp, "/output/mcmc/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/mcmc/", scenario,"/"), showWarnings = FALSE)
  saveRDS(df,paste0("stock-specific/", spp, "/output/mcmc/", scenario, "/df", s,".RData"))

  }
  # browser()
  suppressWarnings({
    rdev <- data.frame(year = as.numeric(colnames(mcmc_data$rdev)) - age_recruited,
                       rdev = sapply(mcmc_data$rdev, median),
                       rdev_sd = sapply(mcmc_data$rdev, sd)) |> na.omit()

    ssb <- data.frame(year = as.numeric(colnames(mcmc_data$ssb)),
                      ssb = sapply(mcmc_data$ssb, median),
                      ssb_sd = sapply(mcmc_data$ssb, sd)) |> na.omit()

    recruits <- data.frame(year = as.numeric(colnames(mcmc_data$recruits)) - age_recruited,
                           recruits = sapply(mcmc_data$recruits, median),
                           recruits_sd = sapply(mcmc_data$recruits, sd)) |> na.omit()

    harvest_rate <- data.frame(year = as.numeric(colnames(mcmc_data$harvest_rate)),
                               harvest_rate = sapply(mcmc_data$harvest_rate, median),
                               harvest_rate_sd = sapply(mcmc_data$harvest_rate, sd)) |> na.omit()
  })

  df <- left_join(ssb, recruits) |> left_join(rdev) |> left_join(harvest_rate)

  df$species <- species
  df$stock <- stock
  df$scenario <- scenario

  saveRDS(df, paste0("stock-specific/",spp,"/output/summary-", scenario, ".rds"))
}

