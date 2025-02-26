# format MCMC output

format_mcmc <- function(
  data,
  species,
  stock,
  scenario = "base",
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
    pivot_longer(cols = everything(), names_to = "year", values_to = "rdev")

  ssb <- out[,grepl("SSB_", names(out))] |>
    rename_with(~str_replace(., "SSB_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "ssb")

  recruits <- out[,grepl("Recr_", names(out))] |>
    rename_with(~str_replace(., "Recr_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "recruits")

  harvest_rate <- out[,grepl("F_", names(out))] |>
    rename_with(~str_replace(., "F_", "")) |>
    pivot_longer(cols = everything(), names_to = "year", values_to = "harvest_rate")

  df <- left_join(ssb, recruits) |> left_join(rdev) |> left_join(harvest_rate)
  df$species <- species
  df$stock <- stock
  df$scenario <- scenario
  df$year <- as.numeric(df$year)

  df <- filter(df, !is.na(year))

  dir.create(paste0("stock-specific/", spp, "/output/mcmc/"), showWarnings = FALSE)
  dir.create(paste0("stock-specific/", spp, "/output/mcmc/", scenario,"/"), showWarnings = FALSE)
  saveRDS(df,paste0("stock-specific/",spp,"/output/mcmc/", scenario, "/df", s,".RData"))

  }
}
