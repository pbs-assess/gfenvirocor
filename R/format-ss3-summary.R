#' format SS3 summary output
#'
#' @param output Summary output from SS3
#' @param species
#' @param stock
#'
#' @export
#'
#' @examples
format_ss3_summary <- function(
  output,
  species,
  stock
  ){
  out_sum <- output
  suppressWarnings({
  rdev <- data.frame(year = out_sum$RecDevs$Yr,
                   rdev = out_sum$RecDevs$Value,
                   rdev_sd = out_sum$RecDevs$Parm_StDev) |> na.omit()

  ssb <- data.frame(year = as.numeric(str_replace(out_sum$SSB_est$Label, "SSB_", "")),
                    ssb = out_sum$SSB_est$Value,
                    ssb_sd = out_sum$SSB_est$StdDev) |> na.omit()

  recruits <- data.frame(year = as.numeric(str_replace(out_sum$Rec_est$Label, "Recr_", "")),
                         recruits = out_sum$Rec_est$Value,
                         recruits_sd = out_sum$Rec_est$StdDev) |> na.omit()

  harvest_rate <- data.frame(year = as.numeric(str_replace(out_sum$FM_est$Label, "F_", "")),
                             harvest_rate = out_sum$FM_est$Value,
                             harvest_rate_sd = out_sum$FM_est$StdDev) |> na.omit()
  })

  df <- left_join(ssb, recruits) |> left_join(rdev) |> left_join(harvest_rate)

  df$species <- species
  df$stock <- stock
  df$scenario <- out_sum$model_name

  saveRDS(df, paste0("stock-specific/",spp,"/output/summary-", out_sum$model_name, ".rds"))
}
