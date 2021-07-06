library(ggplot2)
library(data.table)
library(viridis)


get_all_egs = function(                        
                                              filenames = list( "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks11_sr1_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks8_sr11_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks5_sr19_scms.qs", 
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks8_sr24_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks5_sr33_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks2_sr37_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks3_sr39_scms.qs",
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks9_sr41_scms.qs", 
                                                                 "outputs/regular/England/contact_matrices/bs1000_ngrps9_cap50_nwks4_sr50_scms.qs"
                                                ),
                                                periods = c('1. Lockdown 1', 
                                                            '2. Lockdown 1 easing', 
                                                            '3. Relaxed restrictions', 
                                                            '4. School reopening', 
                                                            '5. Lockdown 2', 
                                                            '6. Lockdown 2 easing', 
                                                            '7. Christmas',
                                                            '8. Lockdown 3',
                                                            '9. Lockdown 3 + schools'),
                        
                                                breaks = c(0,5,12,18,30,40,50,60,70,Inf)
                      ){
  
  all_egs = data.table()
  i = 1
  for( fn in filenames){
    print(fn)
    
    
    #dts = parts[!panel %in% c("C", "D") & survey_round %in% weeks, .(start_date = min(date), end_date = max(date))]
    
    cms = qs::qread(fn)
    levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
    age_lab <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    
    # Get columns of age-groups to put into mapply
    eg = expand.grid(sort(levs),sort(levs))
    
    
    
    eg = data.table(eg)
    eg[, Var1 := factor(Var1, levels = levs, labels = age_lab)]
    eg[, Var2 := factor(Var2, levels = levs, labels = age_lab)]
    
    eg[,cms := rowMeans(cms)]
    
    eg[,sr:=periods[i]]
    
    #eg[, daterange := paste0(dts$start_date, ' - ', dts$end_date)]
    
    
    all_egs = rbind(all_egs, eg)
    
    i = i + 1
  }
  
  all_egs
}
  
  
plot_cms_comparison = function(all_egs, pair_sr, region=''){
  theme_set(cowplot::theme_cowplot(font_size = 20) + theme(strip.background = element_blank()))
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  age_lab <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
  
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  
  eg = data.table(eg)
  eg[, Var1 := factor(Var1, levels = levs, labels = age_lab)]
  eg[, Var2 := factor(Var2, levels = levs, labels = age_lab)]
  
  eg[, cms := all_egs[sr == pair_sr[1]]$cms - all_egs[sr == pair_sr[2]]$cms ]
  
  eg[,sr:='Comparison']
  
  #eg[, daterange := paste0(dts$start_date, ' - ', dts$end_date)]
  

  all_egs = rbind(all_egs, eg)
  all_egs[,cms_lab := round(cms, 1)]
  p1 = ggplot(all_egs[sr %in% pair_sr], aes(Var1, Var2, fill= cms, label=cms_lab)) + 
    geom_tile()+
    geom_text(color='white', size=5)+
    facet_wrap( ~sr, ncol=1) +
    scale_fill_viridis(discrete=FALSE, name='Mean \ncontacts', begin=0, end=1., limits = c(0,4.))+ 
    ylab('Contact age group') +
    xlab('Participant age group') +
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 90))
  
  
  
  
  p2 = ggplot(all_egs[sr =='Comparison'], aes(Var1, Var2, fill= cms, label=cms_lab)) + 
    geom_tile()+
    geom_text(color='white', size=5)+
    labs(subtitle =paste0('Difference (', pair_sr[1], ' - ', pair_sr[2], ')'))+
    #facet_wrap( ~sr) +
    scale_fill_gradient2(low = "#1a0066", mid = "#C0C0C0",
                         high = "#a6ff4d", midpoint = 0, space = "lab", name='', limits=c(-3.2, 3.2)) + 
    ylab('Contact age group') +
    xlab('Participant age group') +
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank(),
          plot.subtitle = element_text(hjust = 0.5),
          axis.text.x = element_text(angle = 90))
  
  library(patchwork)
  
  layout = "
  AACC
  AABB
  AABB
  AADD"
  
  pall = p1 + p2 + plot_spacer() + plot_spacer() + plot_layout(design=layout)
  
  ggsave(filename=paste0('../comix/compare_', region, '_', pair_sr[1], pair_sr[2], '.png'), plot=pall,width =15, height=12)
  
  
}


plot_all_cms = function(all_egs, region='', periods, breaks = c(0,5,12,18,30,40,50,60,70,Inf), title=''){
  theme_set(cowplot::theme_cowplot(font_size = 20) + theme(strip.background = element_blank()))
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  age_lab <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
  

  all_egs[,cms_lab := round(cms, 1)]
  p1 = ggplot(all_egs[sr %in% periods], aes(Var1, Var2, fill= cms, label=cms_lab)) + 
    geom_tile()+
    geom_text(color='white', size=5)+
    facet_wrap( ~sr) +
    scale_fill_viridis(discrete=FALSE, name='Mean \ncontacts', begin=0, end=1., limits = c(0.02,4.5), trans='log', breaks=c(0.02, 0.05, 0.1, 0.2, 0.5, 1.0, 2., 4.), na.value='black')+ 
    guides(fill=guide_colorbar(barheight=30))+
    ylab('Contact age group') +
    xlab('Participant age group') +
    ggtitle(title)+
    theme(axis.line=element_blank(),
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank(), 
          axis.ticks.x=element_blank(), 
          axis.ticks.y=element_blank(),
          axis.text.x = element_text(angle = 90))
  
  
  
  

  
  #ggsave(filename=paste0('../comix/compare_', region, '_', pair_sr[1], pair_sr[2], '.png'), plot=pall,width =15, height=12)
  p1
  
}



augment_cms = function(cmss, swapouts  = 2, breaks = c(0,5,12,18,30,40,50,60,70,Inf))
  {
  
  all_egs = data.table()
  i = 1

  cms1 = cmss[[1]]
  cms2 = cmss[[2]]
  
  
  levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
  age_lab <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
  
  # Get columns of age-groups to put into mapply
  eg = expand.grid(sort(levs),sort(levs))
  eg = data.table(eg)

  
  boolean_switch = matrix(eg[,Var1 == levs[swapouts] | Var2 == levs[swapouts]], nrow=81, ncol=1000)

  cms_aug = cms2 * data.table(boolean_switch) +  cms1 * (-(data.table(boolean_switch) -1))
  
  cms_aug
}

  
plot_full_comparison = function(all_egs, periods =c('Lockdown 1', 'Relaxed restrictions', 'Lockdown 2', 'Lockdown 3', 'Christmas'), scale1=1.5, scale2=1., orient='lower', breaks = c(0,5,12,18,30,40,50,60,70,Inf))  {
  plot_list = list()
  all_egs[,comparison:=0]
  all_egs[,cms_lab := round(cms, 1)]
  period_comps = expand.grid(periods, periods)
  dim1 = length(periods)
  
  if (orient == 'lower'){
    yaxistrue = 1 + seq(0, (dim1^2 - dim1), dim1)
    xaxistrue = seq(dim1^2 - (dim1 - 1), dim1^2)
    cb_scale = 1
    cb_comp = dim1 + 1 
    cb_pos = 'left'
    xpos = 'bottom'
    ypos = 'left'
  }

  if (orient == 'upper'){
  yaxistrue = seq(dim1, (dim1^2), dim1)
  xaxistrue = 1:dim1
  cb_scale = dim1^2
  cb_comp = dim1^2 - dim1 
  cb_pos = 'right'
  xpos = 'top'
  ypos = 'right'
  }
  #xaxistrue = c(1, 4)
  #yaxistrue = c(1,4)
  
  choose_axis = function(chooser=yaxistrue, i=1, angle=NULL, default='lower'){
    
    if(i %in% chooser & orient == default ){
      element_text(angle = angle)
    }
    else{
      element_blank()
    }
  }
  choose_cb = function(bool){
    if(bool){
      guide_colorbar()
    }
    else{
    FALSE
    }
    }
  
  i = 1
  for(pc_i in 1:dim(period_comps)[1]){
    levs <- unique(unlist(cut(seq(0,120),breaks, right=FALSE), use.names = FALSE))
    age_lab <- c("0-4", "5-11", "12-17", "18-29", "30-39", "40-49", "50-59", "60-69", "70+")
    
    pc = period_comps[pc_i,]
    
    # Get columns of age-groups to put into mapply
    eg = expand.grid(sort(levs),sort(levs))
    
    eg = data.table(eg)
    eg[, Var1 := factor(Var1, levels = levs, labels = age_lab)]
    eg[, Var2 := factor(Var2, levels = levs, labels = age_lab)]
    
    if(pc[1] != pc[2]){
      eg[, cms := all_egs[sr == pc[1]$Var1]$cms - all_egs[sr == pc[2]$Var2]$cms ]
      eg[,sr:=paste0(pc[1]$Var1, ' - ' , pc[2]$Var2)]
      plot_list[[i]] = local({
        eg[,cms_lab := round(cms, 1)]
        p1 = 
          ggplot(eg, aes(Var1, Var2, fill= cms, label=cms_lab)) + 
          geom_tile()+
          geom_text(color='white', size=5)+
          labs(subtitle = paste0(pc[1]$Var1, ' - ' , pc[2]$Var2))+
          scale_fill_gradient2(low = "#1a0066", mid = "#C0C0C0",
                               high = "#a6ff4d", midpoint = 0, 
                               space = "lab", name='Difference \nin contacts', limits=c(-scale2, scale2), guide = choose_cb(i == cb_comp)) + 
          scale_x_discrete(position=xpos) + 
          scale_y_discrete(position=ypos) + 
          xlab('Participant age group') +
          ylab('Contact age group') +
          theme(axis.line=element_blank(),
                panel.background=element_blank(),
                panel.border=element_blank(),
                panel.grid.major=element_blank(),
                panel.grid.minor=element_blank(),
                plot.background=element_blank(), 
                axis.ticks.x=element_blank(), 
                axis.ticks.y=element_blank(), 
                legend.position = cb_pos,  
                axis.text.x.top = choose_axis(chooser=xaxistrue, i=i, angle=90, default = 'upper'), 
                axis.text.y.right = choose_axis(chooser=yaxistrue, i=i, default = 'upper'),
                axis.title.x.top = choose_axis(chooser=xaxistrue, i=i, default = 'upper'), 
                axis.title.y.right = choose_axis(chooser=yaxistrue, i=i, default = 'upper'), 
                axis.text.x.bottom = choose_axis(chooser=xaxistrue, i=i, angle=90, default = 'lower'), 
                axis.text.y.left = choose_axis(chooser=yaxistrue, i=i, default = 'lower'),
                axis.title.x.bottom = choose_axis(chooser=xaxistrue, i=i, default = 'lower'), 
                axis.title.y.left = choose_axis(chooser=yaxistrue, i=i, default = 'lower'), 
                plot.subtitle = element_text(hjust=0.5))
        
        print(p1)
      })
      
    }
    else{
      eg[, cms := all_egs[sr == pc[1]$Var1]$cms ]
      eg[,sr:= paste(pc[1], ' ')]
      plot_list[[i]] = local({eg[,cms_lab := round(cms, 1)]
      p1 = 
        ggplot(eg, aes(Var1, Var2, fill= cms, label=cms_lab)) + 
        geom_tile()+
        geom_text(color='white', size=5)+
        labs(subtitle = pc[1]$Var1)+
        scale_fill_viridis(discrete=FALSE, name='Mean \ncontacts', begin=0, end=1., limits = c(0,scale1), guide=choose_cb(i==cb_scale))+  
        scale_x_discrete(position=xpos) + 
        scale_y_discrete(position=ypos) + 
        xlab('Participant age group') +
        ylab('Contact age group') +
        theme(axis.line=element_blank(),
              panel.background=element_blank(),
              panel.border=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              plot.background=element_blank(), 
              axis.ticks.x=element_blank(), 
              axis.ticks.y=element_blank(), 
              legend.position = cb_pos, 
              axis.text.x.top = choose_axis(chooser=xaxistrue, i=i, angle=90, default = 'upper'), 
              axis.text.y.right = choose_axis(chooser=yaxistrue, i=i, default = 'upper'),
              axis.title.x.top = choose_axis(chooser=xaxistrue, i=i, default = 'upper'), 
              axis.title.y.right = choose_axis(chooser=yaxistrue, i=i, default = 'upper'), 
              axis.text.x.bottom = choose_axis(chooser=xaxistrue, i=i, angle=90, default = 'lower'), 
              axis.text.y.left = choose_axis(chooser=yaxistrue, i=i, default = 'lower'),
              axis.title.x.bottom = choose_axis(chooser=xaxistrue, i=i, default = 'lower'), 
              axis.title.y.left = choose_axis(chooser=yaxistrue, i=i, default = 'lower'), 
              plot.subtitle = element_text(hjust=0.5))
      
      print(p1)
    })
    }
    
    
    
    eg[,comparison:=i]
    
    #eg[, daterange := paste0(dts$start_date, ' - ', dts$end_date)]
    
    
    all_egs = rbind(all_egs, eg)
    
    
 
    
   
    i = i+1
    
    
    
  }
  
  
  
  plot_list
  
}
