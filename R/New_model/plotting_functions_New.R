plot_env_responses = function(plant,cex_main=1,...){
  # print(plant$id)
  # print(plant$get_params())
  # recover()
  plant$develop_n(1000)
  par(mfrow=c(3,3))
  # plot 1: TT vs temperature
  Temps = seq(-3,35,length=50)
  plot(Temps,sapply(Temps,function(temp) plant$TT_fun(temp,1)),type='l',
       main = 'TT_inc',
       cex.lab = cex_main,cex.main = cex_main,...)

  # plot 2: Ve vs temperature
  Temps = seq(-5,20,length=50)
  plot(Temps,sapply(Temps,function(temp) plant$Vern_fun(temp,1)),type='l',
       main = 'Ve',
       cex.lab = cex_main,cex.main = cex_main,...)

  # plot 3: Photoperiod_effect vs Daylength
  Daylength = seq(8,20,length=50)
  plot(Daylength,sapply(Daylength,function(dayl) plant$Signal_fun(dayl,1)),type='l',
       main = 'Phot',
       cex.lab = cex_main,cex.main = cex_main,...)


  # plot 4: Photoperiod_effect vs Daylength
  Temps = seq(-3,35,length=50)
  plot(Temps,sapply(Temps,function(temp) plant$HighTemp_fun(temp,1)),type='l',
       main = 'HighTemp activation',ylim = c(0,1),
       cex.lab = cex_main,cex.main = cex_main,...)


  # plot 4: vern timecourse
  plot(plant$get_Vern(),type='l',
       main = 'Vern timecourse',
       cex.lab = cex_main,cex.main = cex_main,...)

  # plot 5: FT timecourse
  plot(plant$get_FT_signal() * plant$get_params()['FT_vs_GA'],type='l',
       main = 'FT timecourse',
       cex.lab = cex_main,cex.main = cex_main,...)

  # plot 6: GA timecourse
  plot(plant$get_GA_signal(),type='l',
       main = 'GA timecourse',
       cex.lab = cex_main,cex.main = cex_main,...)

#
#   # plot 4: Photoperiod_effect vs Daylength
#   Daylength = seq(8,20,length=50)
#   FT_max = Calc.FT.max(plant_state,list(Daylength=Daylength,Noon=T))
#   plot(Daylength,FT_max,type='l',ylim=c(0,max(FT_max,na.rm=T)),
#        main = 'FT_max',
#        cex.lab = cex_main,cex.main = cex_main,...)
#   abline(h=1);abline(v=16)
#
#   # plot 5: FLC x FLM interaction
#   K_repr = plant_state$params$K_repr
#   n_repr = plant_state$params$n_repr
#   FLM = seq(0,plant_state$params$FLMx,length=10)
#   FLC = seq(0,plant_state$params$FLCx,length=10)
#   activity = sapply(FLC,function(x) 1/(1+((FLM + x) / K_repr)^n_repr))
#   activity = activity/min(activity)
#   plot(FLM,rep(NA,length(FLM)),main = 'FLC x FLM repression',ylim = c(1,max(activity)),
#        type='l',cex.lab = cex_main,cex.main = cex_main,...)
#   apply(activity,1,function(x) lines(FLM,x))
#
#   # plot 6: HT repression
#   params = plant_state$params
#   temps = seq(0,40,length=100)
#   degr = Calc.HTfun(temps,params)(seq_along(temps))
#   repressionHT = 1/(1+degr)
#   K_repr = plant_state$params$K_repr
#   n_repr = plant_state$params$n_repr
#   FLMx = plant_state$params$FLMx
#   FLCx = plant_state$params$FLCx
#   activity = 1/(1+((FLMx*repressionHT + FLCx) / K_repr)^n_repr)
#   activity = activity / activity[which(temps>=22)[1]]
#   plot(temps,activity,type='l',ylim=c(0,max(activity)),xlim=c(0,30),
#        cex.lab = cex_main,cex.main = cex_main,...)
#   abline(h=1)
#   abline(v=27)
#   # # temps = environ_data[['RI_01']]$Grnd.Tmp
#   # repression_HT = Calc.HighTempRepression(list(params = params),list(Grnd.Tmp = temps))
#   # plot(repression_HT,ylim=c(0,1),type='l',
#   # 	cex.lab = cex_main,cex.main = cex_main,...)
#
#   # plot 7: FT hill function
#   FT_hill_g = plant_state$params$FT_hill_g
#   total_signal_FT = colSums(plant_state$exp_FT_by_tissue_time*plant_state$active_tissues)
#   FT = total_signal_FT #/ (FT_hill_g + total_signal_FT)
#   plot(total_signal_FT,FT,
#        cex.lab = cex_main,cex.main = cex_main,...)
#   abline(v = total_signal_FT[plant_state$predicted_transition_days])
#
#   # plot 8: FT_by_leaf
#   day = quantile(plant_state$predicted_transition_days,.5,type=1)
#   signal_FT = plant_state$exp_FT_by_tissue_time[,day]*plant_state$active_tissues[,day]
#   plot(signal_FT,
#        cex.lab = cex_main,cex.main = cex_main,...)
#
#
#   # plot 9: GA
#   relative_importance_GA_FT = plant_state$params$relative_importance_GA_FT
#   total_signal_FT = colSums(plant_state$exp_FT_by_tissue_time*plant_state$active_tissues) 	# Leaf_inc already incorporated into active_tissues
#   total_signal_GA = plant_state$GA_activity*relative_importance_GA_FT
#   plot(total_signal_FT, type='l',ylim=c(0,max(c(total_signal_FT,total_signal_GA))),
#        cex.lab = cex_main,cex.main = cex_main,...)
#   lines(total_signal_GA,col=2)
}
