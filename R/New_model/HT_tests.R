library(AtFloweringModel)
library(ggplot2)
library(QuickShinyApp)


Temps = seq(0,30,length=10)
temp = 22
env_lists = lapply(Temps,function(temp) {
  dayl = 16
  env = data.frame(Year = 0, Date = 0, Hour.in.Day = 1:24,Hrs.light = 0,Daylength = dayl, Grnd.Tmp = temp, PAR = 0)
  env$Hrs.light[env$Hour.in.Day >= (24-dayl+1)] = 1
  env$Hrs.light[ceiling(24-dayl)] = dayl %% 1
  e = new(Environ,as.list(env))
  for(i in 1:199) e$repeat_last_day()
  out = as.data.frame(e$make_env_list())
  rm(list='e')
  return(out)
})
do_plot = function(HT_base,F_b,stat) {
  base_params['HT_base'] = HT_base
  base_params['F_b'] = F_b
  base_params['log10_Signal_threshold'] = base_params['log10_Signal_threshold'] / F_b / HT_base
  res = do.call(rbind,lapply(1:length(Temps),function(i) {
    temp = Temps[i]
    id = sprintf('Temp_%0.2f',temp)
    genotype = 'WT'
    environ = sprintf('0.2f',temp)
    plant = new(New_Plant,id,genotype,environ,param_transformations(base_params),as.list(env_lists[[i]]))
    DTB = plant$get_predicted_bolting_day()
    TLN = plant$get_numLeaves()[DTB]
    return(data.frame(Temps = temp,TLN = TLN,DTB = DTB))
  }))

  #  res
  if(stat == 'TLN') {
    ggplot(res,aes(x=Temps,y=TLN)) + geom_line() + expand_limits(y=0)
  } else if(stat == 'DTB') {
    ggplot(res,aes(x=Temps,y=DTB)) + geom_line() + expand_limits(y=0)
  }
}


slider_params = list(
  HT_base = c(0.8,0,1),
  F_b = c(1,0,1),
  stat = c('TLN','DTB')
)

call = 'do_plot(HT_base,F_b,stat)'
run_shiny(call,slider_params)
