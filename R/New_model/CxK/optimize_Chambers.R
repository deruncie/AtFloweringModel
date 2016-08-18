library(AtFloweringModel)
library(ggplot2)

source('loadData_Chambers.R')

fit_data_individuals = full_data_individuals#[full_data_individuals$plant %in% plants,]

source('prep_Chambers.R')

# divide between optimization and validation
Validation_Plant_list = Plant_list[grep('RI_',names(Plant_list))]
Plant_list = Plant_list[names(Plant_list) %in% names(Validation_Plant_list) == F]
#
# subdata = subset(fit_data_individuals,!grepl('RI',ExpID))
# subdata$Group = sapply(as.character(subdata$Treatment),function(x) strsplit(x,'_')[[1]][1])
# ggplot(subset(subdata,FRI == 3 ),aes(x=RLN,y=DTB)) + geom_point() + facet_wrap(~Group)

base_coefs = c(base_params,init_coefs_genotypes)

init = base_coefs[c('Col::FRI','Kas::FRI','Col::FLM','Col::MAF2','Kas::GA5','Kas::GA5_bolt','SVP','log10_FT_vs_GA','log10_Signal_threshold')] #,'HT_base','Tmin'
# init = base_coefs[c('Col::F_b','FRI::F_b','log10_FT_vs_GA','log10_Signal_threshold','Tmin')]
# init['log10_FT_vs_GA'] = log10(7.101957e-03 )
# init['log10_Signal_threshold'] = log10(10^3.499362 ) #/ init['HT_base']
#
for(i in 1:length(init)) init[i] = runif(1,0,1)
init['log10_Signal_threshold'] = rnorm(1,7,3)
# init['Dayl_FT_equal_GA'] = runif(1,8,20)

# init[2] = 1

count = 0

coef_mat = c()
res = optim(init,obj_fun,control = list(trace=9,maxit = 200));res
res = optim(res$par,obj_fun,control = list(trace=9,maxit = 1000));res
obj_fun(res$par)
obj_fun(init)

require(pso)
trace = 9
maxit = 200
lower = sapply(names(init),function(p) {
  if(grepl('::',p)) p = strsplit(p,'::')[[1]][2]
  param_ranges[p,1]
})
upper = sapply(names(init),function(p) {
  if(grepl('::',p)) p = strsplit(p,'::')[[1]][2]
  param_ranges[p,2]
})
count = 0
coef_mat = c()
res = psoptim(init,obj_fun,param_names = names(init),control=list(trace=trace,maxit=maxit),lower=lower,upper=upper)
res = psoptim(res$par,obj_fun,par_names = par_names,base_params = base_params,data = sub_data,control=list(trace=trace,maxit=maxit)  ,lower=lower,upper=upper)



plant = Plant_list[['gi-2::FIBR_HalleFall']]
plant = Plant_list[['vin 3-1::FIBR_CologneSpring']]
plant$get_params()

results = do.call(rbind,mclapply(Plant_list,function(plant) {
  # plant$update_coefs(res$par)
  pred = plant$get_predicted_bolting_PTT()
  obs = plant$get_observed_bolting_PTTs()
  return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs)))
},mc.cores = NCORES))

# observed vs predicted plot
ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
res
