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
  rdev <- data.frame(year = out_sum$RecDevs$Yr - out_sum$age_recruited,
                   rdev = out_sum$RecDevs$Value,
                   rdev_sd = out_sum$RecDevs$Parm_StDev) |> na.omit()

  ssb <- data.frame(year = as.numeric(str_replace(out_sum$SSB_est$Label, "SSB_", "")),
                    ssb = out_sum$SSB_est$Value,
                    ssb_sd = out_sum$SSB_est$StdDev) |> na.omit()

  recruits <- data.frame(year = as.numeric(str_replace(out_sum$Rec_est$Label, "Recr_", "")) - out_sum$age_recruited,
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

#' simplified SS3 summary output (Tom's version)
#'
#' @param output Summary output from SS3
#' @param name Name the SS3 model run
#'
#' @export
#'
#' @examples
extract_SS<-function(fit, name=NULL){
  if(class(fit) != "list")  fit = readRDS(fit)

  out<-list()
  if(is.null(name)){
    out$model_name <- basename(fit$inputs$dir)
  } else {
    out$model_name <- name
  }
  out$age_recruited <- fit$recruitment_dist$recruit_dist$Age

  # Estimates

  out$dep = fit$current_depletion

  # dimensions
  ts = fit$timeseries
  out$startyr = fit$startyr
  out$endyr = fit$endyr
  out$ages = fit$agebins
  out$yrs = ts$Yr

  # MLE time series
  out$SSB = ts$SpawnBio
  out$B = ts$Bio_all
  out$SSN = ts$mature_num
  out$ind = fit$cpue[,names(fit$cpue) %in% c("Fleet_name","Yr","Vuln_bio","Obs","Exp","Calc_Q","SE","Dev","Like")]
  out$FM = fit$exploitation[,c(1,4:ncol(fit$exploitation))]
  out$sel = fit$ageselex
  out$M = fit$Natural_Mortality
  out$catch = fit$catch[,names(fit$catch) %in% c("Fleet_name","Yr","Obs","Exp","se","F","Like")]
  NAA = fit$natage[,names(fit$natage)%in%c("Sex","Yr","Beg/Mid",0,out$ages)]
  out$NAA = NAA[NAA[,3]=="B",c(1:2,4:ncol(NAA))]

  out$npar = fit$N_estimated_parameters

  # Estimates with uncertainty
  dq=fit$derived_quants[,1:3]
  out$FM_est = dq[grepl("F_",dq[,1]),]
  out$SSB_est = dq[grepl("SSB_",dq[,1]),]
  out$Rec_est = dq[grepl("Recr_",dq[,1]),]
  out$Dep_est = dq[grepl("Bratio_",dq[,1]),]


  # Reference points
  out$Refs=list()
  out$Refs$SSB_MSY = dq[dq[,1]=="SSB_MSY",]
  out$Refs$Brel = dq[dq[,1]=="B_MSY/SSB_unfished",]
  out$Refs$R0 = dq[dq[,1] =="Recr_unfished",]
  out$Refs$SSB0 = dq[dq[,1]=="SSB_unfished",]
  out$Refs$B0 = dq[dq[,1]=="Totbio_unfished",]
  out$Refs$OFL =  dq[grepl("OFLCatch",dq[,1]),]
  out$Refs$MSY = dq[dq[,1]=="Ret_Catch_MSY",]

  # Objective function
  out$nll = fit$likelihoods_used
  out$nll_ft = fit$likelihoods_by_fleet

  out$AIC  <- 2 * (out$nll$values[1] + out$npar)
  out$CoVar = fit$CoVar
  # Retro
  #if(!is.null(Fit$peels))out$rho<-mohns_rho(Fit)
  out$RecDevs = fit$recruitpars
  #out$conv = fit$opt$convergence
  out$RunTime = fit$RunTime
  out$Nwarnings = fit$Nwarnings
  out$Warnings = fit$warnings
  out$maxgrad = fit$maximum_gradient_component
  out$sigmaR = fit$sigma_R_info

  out$par = fit$parameters
  out$par_h = fit$parameters_with_highest_gradients

  out$agedbase = fit$agedbase

  out$FleetNames = fit$FleetNames
  # object.size(out)/object.size(fit)
  out

}
