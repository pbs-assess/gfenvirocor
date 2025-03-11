if(!is.null(euphausids)){

load("data/Euphausiids/fit_ph_all_index_bioregion.Rdata")

ggplot(filter(index_bioregion,
              bioregion != "Offshore bioregion")) +
  geom_line(aes(year, log_est, colour = forcats::fct_rev(bioregion))) +
  facet_wrap(~model)

euphausiids <- index_bioregion |>
  filter(bioregion != "Offshore bioregion") |>
  rename(type = model) |>
  group_by(year, type) |> summarise(value = sum(est)) |> ungroup() |>
  group_by(type) |>
  mutate(
    value = value/1000000,
    time = seq_along(year),
    value_raw = value,
    mean = mean(value),
    sd = sd(value),
    value = (value - mean(value))/ sd(value)
  )

euph_regional <- index_bioregion |>
  rename(type = model) |>
  select(year, type, bioregion, est) |>
  group_by(type, bioregion) |>
  mutate(
    value = est/1000000,
    time = seq_along(year),
    value_raw = value,
    mean = mean(value),
    sd = sd(value),
    value = (value - mean(value))/ sd(value)
  ) |> ungroup()

ggplot(euphausiids) + geom_line(aes(year, value, colour = type))

epac <- filter(euphausiids, type == "E. pacifica")
tspin <- filter(euphausiids, type == "T. spinifera")
euph <- filter(euphausiids, type == "Total euphausiid")
euphN <- filter(euph_regional, type == "Total euphausiid"&bioregion == "Northern bioregion")|> select(-bioregion, -est)
euphS <- filter(euph_regional, type == "Total euphausiid"&bioregion == "Southern bioregion")|> select(-bioregion, -est)
euphO <- filter(euph_regional, type == "Total euphausiid"&bioregion == "Offshore bioregion")|> select(-bioregion, -est)

epac$type <- "Euphausiid index (E. pacifica)"
tspin$type <- "Euphausiid index (T. spinifera)"
euph$type <- "Euphausiid index (Total)"

euphN$type <- "Euphausiid index (Total QCS)"
euphS$type <- "Euphausiid index (Total WCVI)"
euphO$type <- "Euphausiid index (Total offshore)"
}


if(!is.null(copepod_regions)){

if("Northern Vancouver Island Shelf" %in% copepod_regions){

  # arbitrarily assigned month of Jan to these annual indices
cops.ns <- readxl::read_excel("data/WCVI Copepod Biomass Anomalies SOPO 2024 for Dana.xlsx", sheet = "Northern Vancouver Island Shelf") |>
  rename(year = yrs) |> mutate(month = 1)

cops.ns.boreal <- select(cops.ns, year, month, value = Cops.boreal)
cops.ns.south <- select(cops.ns, year, month, value = Cops.south)
cops.ns.subarctic <- select(cops.ns, year, month, value = Cops.subarctic)
}

if("Southern Vancouver Island Shelf" %in% copepod_regions){
cops.ss <- readxl::read_excel("data/WCVI Copepod Biomass Anomalies SOPO 2024 for Dana.xlsx", sheet = "Southern Vancouver Island Shelf") |>
  rename(year = yrs) |> mutate(month = 1)

cops.ss.boreal <- select(cops.ss, year, month, value = Cops.boreal)
cops.ss.south <- select(cops.ss, year, month, value = Cops.south)
cops.ss.subarctic <- select(cops.ss, year, month, value = Cops.subarctic)
}

  if("Northern Vancouver Island Offsh" %in% copepod_regions){
    cops.no <- readxl::read_excel("data/WCVI Copepod Biomass Anomalies SOPO 2024 for Dana.xlsx", sheet = "Northern Vancouver Island Offsh") |>
      rename(year = yrs) |> mutate(month = 1)

    cops.no.boreal <- select(cops.no, year, month, value = Cops.boreal)
    cops.no.south <- select(cops.no, year, month, value = Cops.south)
    cops.no.subarctic <- select(cops.no, year, month, value = Cops.subarctic)
  }

  if("Southern Vancouver Island Offsh" %in% copepod_regions){
    cops.so <- readxl::read_excel("data/WCVI Copepod Biomass Anomalies SOPO 2024 for Dana.xlsx", sheet = "Southern Vancouver Island Offsh") |>
      rename(year = yrs) |> mutate(month = 1)

    cops.so.boreal <- select(cops.so, year, month, value = Cops.boreal)
    cops.so.south <- select(cops.so, year, month, value = Cops.south)
    cops.so.subarctic <- select(cops.so, year, month, value = Cops.subarctic)
  }



  if("Southern Vancouver Island Shelf" %in% copepod_regions & "Northern Vancouver Island Shelf" %in% copepod_regions){
    cops.ss$region <- "Southern Vancouver Island Shelf"
    cops.ns$region <- "Northern Vancouver Island Shelf"

    cops.shelf <- bind_rows(cops.ns, cops.ss) |>  group_by(year, month) |> summarise_all(mean)

    cops.shelf.boreal <- select(cops.shelf, year, month, value = Cops.boreal)
    cops.shelf.south <- select(cops.shelf, year, month, value = Cops.south)
    cops.shelf.nonarctic <- cops.shelf |> mutate(value = Cops.south + Cops.boreal) |> select(year, month, value)
    cops.shelf.subarctic <- select(cops.shelf, year, month, value = Cops.subarctic)
  }
}

if(!is.null(herring_stocks)){
herring_recuits <- herring_recruitment |>
  filter(region %in% herring_stocks) |>
  group_by(year) |>
  mutate(value = sum(median),
         month = 1) |>
  select(year, month, value) |>
  distinct()

herring_ssb <- herring_spawning_biomass |>
  filter(region %in% herring_stocks) |>
  group_by(year) |>
  mutate(value = sum(median),
         month = 1) |>
  select(year, month, value) |>
  distinct()
}

if(!is.null(conspecific_ssb)){
ts <- readRDS(paste0("stock-specific/",spp,"/output/summary-", scenario, ".rds"))

conspecific_ssb <- ts |>
  mutate(value = ssb,
         month = 1) |>
  select(year, month, value) |>
  distinct()
}
