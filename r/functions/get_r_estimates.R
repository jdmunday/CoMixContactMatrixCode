
# Load polymod ------------------------------------------------------------



get_r_estimates = function(area_name, fit_type, samples, cap, week_range,  nwks=2, breaks=c(0,5,12,18,30,40,50,60,70,Inf), cms_all = NULL, suscvec=NULL, tranvec=NULL)
  {
  #if ((length(breaks) - 1) == 9){
  #  eigs_pmod <- qs::qread('outputs/regular/pmod/bs/eigs_pmod.qs')
  #}
  #else if ((length(breaks) - 1) == 3){
  #  eigs_pmod <- qs::qread('outputs/regular/pmod/bs/eigs_pmod3.qs')
  #}
  #else{
  #  print('No matching polymod matrix')
  #  return(0)
  #}
  
  if (is.null(cms_all)){
    
    # initialise empty containers --------------------------------------------------
    
    cms_all = list()
    cms = list()
    
    # Load in pre-calculated contact matrices--------------------------------------------------
    
    for(week in week_range){
      cms[[week]] = qs::qread(paste0('outputs/regular/',area_name, '/contact_matrices/', fit_type, samples,  '_ngrps', length(breaks) - 1,'_cap', cap, '_nwks', nwks, '_sr', week,'_scms.qs'))
      }
  
  
    cms_all = cms
  }
  else{
    print('using provided cms')
  }

  
  # Get susceptibility vector --------------------------------------------------
  cms_pmd = list()
  cms_p = list()
  
  # Load in pre-calculated contact matrices--------------------------------------------------
  
  for(week in c(50)){
    cms_p[[1]] = qs::qread('outputs/regular/england/contact_matrices/bs1000_ngrps9_cap50_nwks0_sr0_scms.qs')
  }
  cms_pmd = cms_p

  
  
  # Calculate Rs from eigs relative to polymod --------------------------------------------------

  eigs_pmod <- compare_Rs_stan(cms_pmd, breaks, weeks_range = 1:1, suscvec = suscvec, tranvec = tranvec)
  eigs_pmod = unlist(eigs_pmod)
  eigs <- compare_Rs_stan(cms_all, breaks, weeks_range = week_range, suscvec = suscvec, tranvec = tranvec)
  Rs = t(t(eigs) / eigs_pmod[1:samples]) * rnorm(samples, 2.6, 0.54)
  R_05s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs[X,]), 0.05)})
  R_75s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs[X,]), 0.75)})
  R_95s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs[X,]), 0.95)})
  R_25s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs[X,]), 0.25)})
  R_50s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs[X,]), 0.50)})



# Put values in data.tables -----------------------------------------------
  dates = data.table()
  for (week in week_range){
    weeks = c(week, week + 1)
    dates <- rbind(dates, parts[!panel %in% c("C", "D") & survey_round %in% weeks, .(start_date = min(date), end_date = max(date))])
  }
  
  dt_rs <- data.table(survey_round = week_range, 
                      R_50 = R_50s, 
                      R_05 = R_05s, 
                      R_95 = R_95s, 
                      R_25 = R_25s, 
                      R_75 = R_75s, 
                      est = "Cap 200", 
                      start_date = dates$start_date, 
                      end_date = dates$end_date)
  
  qs::qsave(dt_rs, file = paste0("outputs/regular/", area_name, "/dt_rs_", fit_type, samples, '_ngrps', length(breaks) - 1,'_cap', cap, '_nwks', nwks, "_sr", min(week_range), 'to', max(week_range), '.qs'))
  return(eigs)
}


