library(AtFloweringModel)
library(ggplot2)


Temp = 23
Dayls = seq(6,20,length=50)
dayl = 6
env_lists = lapply(Dayls,function(dayl) {
  env = data.frame(Year = 0, Date = 0, Hour.in.Day = 1:24,Hrs.light = 0,Daylength = dayl, Grnd.Tmp = Temp, PAR = 0)
  env$Hrs.light[env$Hour.in.Day >= (24-dayl+1)] = 1
  env$Hrs.light[ceiling(24-dayl)] = dayl %% 1
  e = new(Environ,as.list(env))
  for(i in 1:199) e$repeat_last_day()
  out = as.data.frame(e$make_env_list())
  rm(list='e')
  return(out)
})

# plot(env_lists[[5]]$Hrs.light,type='l',col=2)

base_params2 = c(
  Tmin = 3,
  Topt = 20,
  T10 = (10-3)/(20-3),
  Tmax = 50,
  F_b = 1,
  k=-5.17485563597551,	#same vernalization model
  w=2.22568888596679,
  xi=0.995904471970916,
  T_vmin = -3.5,
  T_vmax = 6,
  Vsat = 960,
  Pday = 1,
  Pnight = 0,
  FT_LD_vs_SD = 100,
  FT_base = 0,
  FT_vs_GA = 1/674.1914,#/(2*10293.23)*17,
  # GA_vs_FT = 1,
  Signal_threshold = 6946975/674.1914,
  Bolting_threshold = 0,
  TT_germ = 0,#17*5*24,
  TT_phyllochron = 20*24
)
i=2
res = do.call(rbind,lapply(1:length(Dayls),function(i) {
  dayl = Dayls[i]
  id = sprintf('Dayl_%0.2f',dayl)
  genotype = 'WT'
  environ = sprintf('0.2f',dayl)
  plant = new(New_Plant,id,genotype,environ,base_params2,as.list(env_lists[[i]]))
  # plant$get_predicted_bolting_day()
  # plant$get_numLeaves()
  # plant$get_cumTT()
  DTB = plant$get_predicted_bolting_day()
  TLN = plant$get_numLeaves()[DTB]
  # return(data.frame(dayl,max(plant$get_FT_signal()),max(plant$get_GA_signal())*a/2))
  return(data.frame(Dayl = dayl,TLN = TLN))
}))

#  res
ggplot(res,aes(x=Dayl,y=TLN)) + geom_line() + ylim(c(0,70))

# find where FT_signal and GA_signal are equal at bolting

dayl = Dayls[24]
plant = new(New_Plant,'id','genotype','environ',base_params,as.list(env_lists[[match(dayl,Dayls)]]))
obj_fun = function(l10_FT_vs_GA,dayl){
  plant$update_params(c(FT_vs_GA = 10^(l10_FT_vs_GA)))
  DTB = plant$get_predicted_bolting_day()
  # print(c(FT_vs_GA,DTB,plant$get_FT_signal()[DTB],plant$get_GA_signal()[DTB]))
  return(abs(10^(l10_FT_vs_GA)*plant$get_FT_signal()[DTB] - plant$get_GA_signal()[DTB]))
}
res = optimize(obj_fun,c(-10,10))
res
plant$get_params()
plant$update_params(c(FT_vs_GA = 10^(res$minimum)))
DTB = plant$get_predicted_bolting_day()
plant$get_FT_signal()[DTB] * plant$get_params()['FT_vs_GA']
plant$get_GA_signal()[DTB]
plant$update_params(c(FT_vs_GA = 1/674.1914))

