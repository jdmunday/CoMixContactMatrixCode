library(data.table)
library(cowplot)
library(ggplot2)
library(patchwork)

source('r/presentation/plot_comparison_cms.R')
source('r/functions/get_r_estimates.R')
source('r/functions/get_minimal_data.R')

country_names = c("uk")
settings = c("home","school","work","other")

breaks = c(0,5,12,18,30,40,50,60,70,Inf)
week_range = c(53,54,57:63) #c(1,11,19,24,34,37,39,42,51)
nwk = rep(2,length(week_range)) #c(10,8,5,10,3,2,3,9,6)
samples_ = 1000
fit_with_ = 'bs'
max_ = 50
trunc_flag_ = F
zi_ = T

# periods = c('1. Lockdown 1', 
#             '2. Lockdown 1 easing', 
#             '3. Relaxed restrictions', 
#             '4. School reopening', 
#             '5. Lockdown 2', 
#             '6. Lockdown 2 easing', 
#             '7. Christmas',
#             '8. Lockdown 3', 
#             '9. Lockdown 3 + schools')
periods = as.character(week_range)

nation_names = c("England", "Scotland", "Wales")[1]

for (i in 1:length(nation_names)){
# for (i in 1:length(country_names)){
  
  outfolder=paste0('outputs/setting_specific/', nation_names[i], '/')
  # outfolder=paste0('outputs/setting_specific/', country_names[i], '/')
  filename_primer = paste0(outfolder, 'contact_matrices/', fit_with_, samples_, '_ngrps', length(breaks) - 1, '_cap', max_)
  fnms = character(length(week_range))
  for (k in 1:length(week_range)){
    fnms[k] =  paste0(filename_primer, '_nwks', nwk[k],'_sr', week_range[k])
  }
  
  for (j in 1:length(settings)){
    
    fnms_setting <- paste0(fnms, '_', settings[j])
    if (zi_){
      fnms_setting <- paste0(fnms_setting,"_zi")
    }
    if (trunc_flag_){
      fnms_setting <- paste0(fnms_setting,"_trunc")
    }
    fnms_setting <- paste0(fnms_setting,'_scms.qs')
    
    all_egs = get_all_egs(filenames = fnms_setting, periods = periods, breaks = breaks)
    
    # compared_egs = plot_full_comparison(all_egs, periods = periods[1:3], scale1 = 1.7, scale2 = 1.2, orient='lower')
    # 
    # mat3 = compared_egs[[1]] + plot_spacer() + plot_spacer() + 
    #   compared_egs[[4]] + compared_egs[[5]] + plot_spacer() + 
    #   compared_egs[[7]] + compared_egs[[8]] + compared_egs[[9]]
    # 
    # compared_egs = plot_full_comparison(all_egs, periods =  periods[4:6], scale1 = 4.5, scale2 = 3., orient='lower')
    # 
    # mat4 = compared_egs[[1]] + plot_spacer() + plot_spacer() + 
    #   compared_egs[[4]] + compared_egs[[5]] + plot_spacer() + 
    #   compared_egs[[7]] + compared_egs[[8]] + compared_egs[[9]]
    # 
    # compared_egs = plot_full_comparison(all_egs, periods =  periods[7:9], scale1 = 4.5, scale2 = 3., orient='lower')
    # 
    # mat5 = compared_egs[[1]] + plot_spacer() + plot_spacer() + 
    #   compared_egs[[4]] + compared_egs[[5]] + plot_spacer() + 
    #   compared_egs[[7]] + compared_egs[[8]] + compared_egs[[9]]
    # 
    # ggsave('compare_mat_LD1_plus.pdf', mat3, width=20, height=20)
    # ggsave('compare_mat_LD2_plus.pdf', mat4, width=20, height=20)
    # ggsave('compare_mat_LD3_plus.pdf', mat5, width=20, height=20)
    
    
    all_mats = plot_all_cms(all_egs = all_egs, periods = periods) #, title='A')
    
    # dates = data.table::transpose(
    #   data.table(
    #     c('20200324', '20200603'), 
    #     c('20200603', '20200729'), 
    #     c('20200729', '20200904'),
    #     c('20200904', '20201024'), 
    #     c('20201105', '20201202'),
    #     c('20201202', '20201219'), 
    #     c('20201219', '20210102'), 
    #     c('20210105', '20210308'), 
    #     c('20210308', '20210330')))
    # colnames(dates) = c('start', 'end')
    # 
    # dates[,start := lubridate::ymd(start)]
    # dates[,end := lubridate::ymd(end)]
    # dates[,periods := periods]
    # dates[,period_num:=1:length(periods)]
    # 
    # 
    # 
    # eigs = data.table()
    # eigs_ld1 = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 1:1, nwks=11))
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 11:11, nwks=8)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 19:19, nwks=5)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 24:24, nwks=8)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 33:33, nwks=5)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 37:37, nwks=2)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 39:39, nwks=3)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 41:41, nwks=9)/eigs_ld1)
    # eigs = rbind(eigs,get_r_estimates('England', 'bs', 1000, 50, 50:50, nwks=2)/eigs_ld1)
    # 
    # 
    # suscvec_davies = c(0.4,
    #                    0.4,
    #                    0.4,
    #                    0.79,
    #                    0.86,
    #                    0.8,
    #                    0.82,
    #                    0.88,
    #                    0.74)
    # 
    # tranvec_davies = c(0.645,
    #                    0.645,
    #                    0.605,
    #                    0.635,
    #                    0.665,
    #                    0.7,
    #                    0.745,
    #                    0.815,
    #                    0.845)
    # 
    # eigs_cvd = data.table()
    # eigs_cvd_ld1 = get_r_estimates('England', 'bs', 1000, 50, 1:1,   nwks=11, suscvec = suscvec_davies, tranvec = tranvec_davies)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 11:11, nwks=8,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 19:19, nwks=5,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 24:24, nwks=8,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 33:33, nwks=5,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 37:37, nwks=2,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 39:39, nwks=3,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 41:41, nwks=9,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # eigs_cvd = rbind(eigs_cvd,get_r_estimates('England', 'bs', 1000, 50, 50:50, nwks=2,  suscvec = suscvec_davies, tranvec = tranvec_davies)/eigs_cvd_ld1)
    # 
    # dates[,eigs_05s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs[X,]), 0.05)}))]
    # dates[,eigs_75s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs[X,]), 0.75)}))]
    # dates[,eigs_95s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs[X,]), 0.95)}))]
    # dates[,eigs_25s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs[X,]), 0.25)}))]
    # dates[,eigs_50s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs[X,]), 0.50)}))]
    # dates[,eigs_cvd_05s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs_cvd[X,]), 0.05)}))]
    # dates[,eigs_cvd_75s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs_cvd[X,]), 0.75)}))]
    # dates[,eigs_cvd_95s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs_cvd[X,]), 0.95)}))]
    # dates[,eigs_cvd_25s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs_cvd[X,]), 0.25)}))]
    # dates[,eigs_cvd_50s := c(1,sapply(1:(8), function(X){quantile(ecdf(eigs_cvd[X,]), 0.50)}))]
    # 
    # dates[, stringency:=c("Lockdown","Easing","Relaxed","Relaxed + schools","Lockdown + schools","Easing","Relaxed","Lockdown","Lockdown + schools")]
    # 
    # color_list <- c("Equal" = "orange", "COVID-like" = "pink")
    # 
    # dates_plot = ggplot(dates) + 
    #   geom_rect(aes(xmin=start, xmax=end, ymin=0.4, ymax=0.9, fill=stringency), alpha=1.)+
    #   geom_pointrange(aes(x= start + 0.5 * (end - start), y=eigs_50s, ymin=eigs_05s, ymax=eigs_95s, color='Equal'))+
    #   geom_pointrange(aes(x= start + 0.5 * (end - start), y=eigs_cvd_50s, ymin=eigs_cvd_05s, ymax=eigs_cvd_95s, color='COVID-like'))+
    #   scale_fill_brewer(palette='Pastel1')+
    #   geom_text(aes(x=start + 0.5 * (end - start), y=0.65, label=period_num), color='white', size=9)+ 
    #   scale_x_date(breaks='month', date_labels = "%b '%y",expand = expansion(0), name='')+
    #   scale_y_continuous(name='Relative change \nin eigenvalue', limits=c(0.4,3.5), breaks=seq(1.,3.4,0.5))+
    #   ggtitle('B')+
    #   scale_color_manual(name='Transmissibility', values=color_list)+
    #   theme(
    #     axis.line.x=element_blank(),
    #     panel.grid.major.y = element_line(colour='grey')
    #   )
    # layout = '
    # A
    # A
    # A
    # A
    # A
    # A
    # B'
    layout = '
    A
    A
    A
    A
    A
    A'
    
    # all_mats_dates = all_mats + dates_plot + 
    #   plot_layout(design = layout)
    # dates[,'R_inc_Equal' := paste0(round(eigs_50s,2), ' (', round(eigs_05s,2), ' - ', round(eigs_95s,2), ')')]
    # dates[,'R_inc_COVID' := paste0(round(eigs_cvd_50s,2), ' (', round(eigs_cvd_05s,2), ' - ', round(eigs_cvd_95s,2), ')')]
    # 
    # dates[,Date := paste0(format(start, "%d %b %Y"), ' - ', format(end, "%d %b %Y"))]
    # 
    # dates_pres = dates[,c('Date', 'periods', 'R_inc_Equal', 'R_inc_COVID')]
    # 
    # names(dates_pres) = c('Dates', 'Periods', 'Eigenvalue')
    # dates_pres
    # 
    # write.csv(dates_pres, 'periods_eigenvalues.csv')
    # 
    # ggsave('all_mats.pdf', all_mats_dates, width=20, height=20)
    figname_primer <- paste0(outfolder,"contact_matrices/all_mats_",settings[j])
    # figname_primer <- paste0("all_mats_",settings[j])
    if (zi_){
      figname_primer <- paste0(figname_primer,"_zi")
    }
    if (trunc_flag_){
      figname_primer <- paste0(figname_primer,"_trunc")
    }
    ggsave(paste0(figname_primer,'.pdf'), all_mats, width=20, height=20)
  }
}

