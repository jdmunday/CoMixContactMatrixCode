## Name: fit_neg_binom
## Description: 
## Input file: clean_participants.rds, clean_contacts.rds
## Functions:
## Output file: 

# the following code loops through the weeks to date and constructs a contact matrix using get_matrix(). 
# There is also a calculation of R assuming probability of infection on contact on 0.1


# Packages ----------------------------------------------------------------
library(data.table)
library(ggplot2)
library(viridis)


# Source user written scripts ---------------------------------------------

source('r/functions/get_minimal_data.R')
source('r/functions/functions.R')
source('r/functions/calc_cm.R')
source('r/functions/compare_Rs.R')

# Input data ----------------------------------------------------------------

# extract data with useful columns
data =  get_minimal_data()

# decant data into relevant containers
contacts =  data[[1]]
parts =  data[[2]]
parts = parts[between(date, lubridate::ymd('20200324'), lubridate::ymd('20200603'))]
contacts = contacts[between(date, lubridate::ymd('20200324'), lubridate::ymd('20200603'))]
ignore_puid = qs::qread('../comix/data/local_august.qs')
contacts = contacts[!(part_wave_uid %in% ignore_puid$part_wave_uid)]
parts = parts[!(part_wave_uid %in% ignore_puid$part_wave_uid)]


# set breaks and get population proportions 
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
#breaks = c(0,18,65,Inf)
popdata_totals = get_popvec(breaks, year_ = 2020)
week_range = c(1:11)
samples_ = 10
fit_with_ = 'bs'

# Filter data -------------------------------------------------------------
unique_wave_pid <- unique(parts$part_wave_uid)
contacts <- contacts[part_wave_uid %in% unique_wave_pid]

parts[,part_id := paste(as.character(part_id), survey_round, sep = '_')]
contacts[,part_id := paste(as.character(part_id), survey_round, sep = '_')]


regions =list(c("North East", "Yorkshire and The Humber"), c("North West"),  c("East Midlands", "West Midlands"), c("East of England"), c("South West"), c("South East"), c("Greater London"))
nations = list(unlist(regions), c('Scotland'), c('Wales'), c('Northern Ireland'))
nation_names = c("England", "Scotland", "Wales")[1]

weights = get_contact_age_weights()

#for (i in 1:length(regions)){
for (i in 1:length(nation_names)){
 
  parts_nation <- parts[area_3_name %in% nations[[i]]]
  
  unique_wave_pid <- unique(parts_nation$part_wave_uid)
  contacts_nation <- contacts[part_wave_uid %in% unique_wave_pid]
  print(nations[[i]])
  # detect cores-------------------------------------------------------------
  
  # calculate contact matrices-------------------------------------------------------------
  
  
  outfolder=paste0('outputs/regular/', nation_names[i], '/')
  if(!dir.exists(outfolder)){
    dir.create(outfolder, recursive = TRUE)
  }
  
  cms_max200 = calc_cm_general(parts_nation, contacts_nation, breaks, max_ = 50, popdata_totals, weeks_range = week_range, nwks="ALL", outfolder=outfolder, fitwith=fit_with_, samples=samples_, weights=NULL)
  
  }
 


# eigs_max200 <- compare_Rs_stan(cms_max200, breaks, weeks_range = week_range)
#  
#  
#  # Get dominant eigenvalue for POLYMOD -------------------------------------
#  
#  eigs_pmod = qs::qread('outputs/regular/pmod/bs/eigs_pmod.qs')
#  
#  # Calculate Rs from eigs relative to polymod --------------------------------------------------
#  
#  Rs_max200 = t(t(eigs_max200) / eigs_pmod[1:samples_]) * rnorm(samples_, 2.6, 0.54)
#  
#  R_05s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs_max200[X,]), 0.05)})
#  R_75s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs_max200[X,]), 0.75)})
#  R_95s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs_max200[X,]), 0.95)})
#  R_25s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs_max200[X,]), 0.25)})
#  R_50s = sapply(1:length(week_range), function(X){quantile(ecdf(Rs_max200[X,]), 0.50)})
#  
#  
#  
#  # Put values in data.tables -----------------------------------------------
#  
#  
#  dates = data.table()
#  for (week in week_range){
#    weeks = c(week, week + 1)
#    dates <- rbind(dates, parts[!panel %in% c("C", "D") & survey_round %in% weeks, .(start_date = min(date), end_date = max(date))])
#  }
#  
#  dt_rs_max200 <- data.table(survey_round = week_range, 
#                             R_50 = R_50s, 
#                             R_05 = R_05s, 
#                             R_95 = R_95s, 
#                             R_25 = R_25s, 
#                             R_75 = R_75s, 
#                             est = "Cap 100", 
#                             start_date = dates$start_date, 
#                             end_date = dates$end_date)
#  
#  qs::qsave(dt_rs_max200, paste0(outfolder, 'dt_rs_max200.qs'))
#  
#}
#
#
#
##ps = list()
##Rs = c()
#egs <- list()
#
## without weighting for weekend / weekday
#for(week in seq(1,33, 1)){
#  i = week 
#  #print(i)
#  weeks <- c(week, week +1)
#  if(i %in% c(1:6, 17,18))  weeks <- c(weeks, 700)
#  outs = get_matrix(conts_exchild, parts_exchild, weeks, breaks)
#  eg = data.table(outs[[1]])
#  
#  eg_props = symetricise_matrix(eg, popdata_totals, breaks)
#  egs[[i]] <- eg_props
#
#  ps[[i]] = local({
#    p1 = ggplot(eg_props, aes(age_group, age_group_cont, fill= aug_mean_sym)) + 
#    geom_tile() + scale_fill_viridis(limits=c(0,9)) +
#      theme(legend.position = "none") +
#      labs(title = i)
#    print(p1)})
#  q = 1
#  R = max(eigen(matrix(eg_props$aug_mean_sym, nrow = (length(breaks) - 1))*q)$values)
#  Rs[i] = R
#  print(R)
#  }
#
#
#ps

# legend <- get_legend(ps[[i]])
# plot_grid(plotlist=ps, legend)




# the following code constructs a contact matrix using get_matrix() for week 30 with different samples of age for each participant and contact. 
# There is also a calculation of R assuming probability of infection on contact on 0.1

#for(i in seq(10)){
#  #print(i)
#  
#  #print(week)
#  outs = get_matrix(conts, parts, 30, breaks)
#  eg = data.table(outs[[1]])
#  eg_props = symetricise_matrix(eg, popdata_totals, breaks)
#  ps[[i]] = local({
#    p1 = ggplot(eg_props, aes(age_group, age_group_cont, fill= aug_mean_sym)) + 
#      geom_tile() + scale_fill_viridis(limits=c(0,5))
#    print(p1)})
#  q = 1
#  R = max(eigen(matrix(eg_props$aug_mean_sym, nrow = (length(breaks) - 1))*q)$values)
#  Rs[i] = R
#  print(R)
#}
#
#part[ , table(survey_round, panel)]
#

