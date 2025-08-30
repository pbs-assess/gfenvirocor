# format MCMC output

format_mcmc <- function(
  data,
  species,
  stock,
  scenario = "base",
  age_recruited = out_sum$age_recruited,
  samples = 100,
  seed = 10
  ) {
  # data <- out

  set.seed(seed)

  for(s in 1:samples){
  sample_row <- sample(1:nrow(data),size=1)
  out <- data[sample_row,]

  rdev <- out[,grepl("Main_RecrDev", names(out))] |>
    rename_with(~str_replace(., "Main_RecrDev_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "rdev") |>
    mutate(year = as.numeric(year) - age_recruited)

  ssb <- out[,grepl("SSB_", names(out))] |>
    rename_with(~str_replace(., "SSB_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "ssb") |>
    mutate(year = as.numeric(year))

  recruits <- out[,grepl("Recr_", names(out))] |>
    rename_with(~str_replace(., "Recr_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "recruits") |>
    mutate(year = as.numeric(year) - age_recruited)

  ## NOTES on harvest rate.
  # Harvest rate U is bounded between 0 and 1 and represents the annual removal rate (i.e., Annual Catch_t = U_t*VulnerableBiomass_t). I think this is what you want.
  # Fishing mortality is is between 0 and infinity and represents the instantaneous fishing mortality rate. Catch is calculated with the Baranov catch equation.
  # The relationship between them is: U=1-exp(-F)
  # For the iscam outputs, you can just take mcmc$ut (I think the line is already there, commented out, mcmc_data$harvest_rate <- data$mcmc$ut[[1]][[main_commercial_gear]] and just need to change the header for hearvest rate: fheader<-paste0("ut1_gear",main_commercial_gear,"_")
  # For SS3, if they don't produce Ut, then just use the formula above to convert
  # TODO: check how this is used and which number we actually want.
  harvest_rate <- out[,grepl("F_", names(out))] |>
    rename_with(~str_replace(., "F_", "")) |>
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
  saveRDS(df,paste0("stock-specific/",spp,"/output/mcmc/", scenario, "/df", s,".RData"))

  }
}
