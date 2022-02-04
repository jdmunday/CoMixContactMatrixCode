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
library(doParallel)

# Source user written scripts ---------------------------------------------

source('r/functions/get_minimal_data.R')
source('r/functions/functions.R')
# source('r/functions/get_react_data.R')
source('r/functions/calc_cm.R')
source('r/functions/compare_Rs.R')

# Set up parallel computing environment -------------------------------------
ncores = detectCores() - 1
registerDoParallel(cores = ncores)

# Input data ----------------------------------------------------------------

# extract data with useful columns
data =  get_minimal_data()

# decant data into relevant containers
contacts =  data[[1]]
parts =  data[[2]]

start_date = lubridate::ymd('20200323')
end_date = lubridate::ymd('20210623')

parts = parts[between(date, start_date, end_date)]
contacts = contacts[between(date, start_date, end_date)]


# set breaks for age categories and get population proportions 
breaks = c(0,5,12,18,30,40,50,60,70,Inf)
#breaks = c(0,18,65,Inf)
max_ = 50 # upper limit for censoring/truncation
popdata_totals = get_popvec(breaks, year_ = 2020)
weeks_in_parts = sort(unique(parts$survey_round))
week_range = c(53,54,57:63) #c(1,11,19,24,34,37,39,42,51) #c(min(weeks_in_parts):max(weeks_in_parts))
#week_range = 34:51
nwk = rep(2,length(week_range)) #c(10,8,5,10,3,2,3,9,6)
samples_ = 1000
fit_with_ = 'bs'
trunc_flag_ = F # flag for whether or not to use truncation rather than uncorrected censoring
zi_ = T # flag for fitting zero-inflated negative binomial vs negative binomial

# Filter data -------------------------------------------------------------
unique_wave_pid <- unique(parts$part_wave_uid)
contacts <- contacts[part_wave_uid %in% unique_wave_pid]

parts[,part_id := paste(as.character(part_id), survey_round, sep = '_')]
contacts[,part_id := paste(as.character(part_id), survey_round, sep = '_')]

countries = list(c("uk"))
country_names = c("uk")
regions = list(c("North East", "Yorkshire and The Humber"), c("North West"),  c("East Midlands", "West Midlands"), c("East of England"), c("South West"), c("South East"), c("Greater London"))
nations = list(unlist(regions), c('Scotland'), c('Wales'), c('Northern Ireland'))
nation_names = c("England", "Scotland", "Wales")[1]

settings = c("home","school","work","other")

weights = get_contact_age_weights()

# for (i in 1:length(country_names)){
#for (i in 1:length(regions)){
for (i in 1:length(nation_names)){
  for (j in 1:length(settings)){
    # for (k in 1:length(week_range)){
    lcms = foreach(k=1:length(week_range)) %dopar% {
      print(k)
      
      # contacts_nation <- contacts[country %in% countries[[i]] & eval(parse(text=paste0("cnt_",settings[j])))]
      # unique_wave_pid <- unique(contacts_nation$part_wave_uid)
      # 
      # parts_nation <- parts[part_wave_uid %in% unique_wave_pid]
      
      # parts_nation <- parts[area_3_name %in% regions[[i]]]
      parts_nation <- parts[area_3_name %in% nations[[i]]]
      # parts_nation <- parts[country %in% countries[[i]]]
      
      unique_wave_pid <- unique(parts_nation$part_wave_uid)
      contacts_nation <- contacts[part_wave_uid %in% unique_wave_pid]
      
      print(nations[[i]])
      # print(countries[[i]])
      
      
      # calculate contact matrices-------------------------------------------------------------
      
      
      outfolder=paste0('outputs/setting_specific/', nation_names[i], '/')
      # outfolder=paste0('outputs/setting_specific/', country_names[i], '/')
      if(!dir.exists(outfolder)){
        dir.create(outfolder, recursive = TRUE)
      }
      
      # cms_max50 = calc_cm_general(parts_nation, contacts_nation, breaks, max_ = max_, popdata_totals, weeks_range = week_range[k], nwks=nwk[k], outfolder=outfolder, fitwith=fit_with_, samples=samples_, weights=NULL, trunc_flag=trunc_flag_, zi=zi_, setting=settings[j])
      calc_cm_general(parts_nation, contacts_nation, breaks, max_ = max_, popdata_totals, weeks_range = week_range[k], nwks=nwk[k], outfolder=outfolder, fitwith=fit_with_, samples=samples_, weights=NULL, trunc_flag=trunc_flag_, zi=zi_, setting=settings[j])
      
    }
    
  }
  
}
