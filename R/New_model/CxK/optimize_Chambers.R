library(AtFloweringModel)
library(ggplot2)

source('loadData_New.R')

# select plants
genotypes = c('Col','Col FRI','vin 3-1', 'fve-3')#,'vin3-4 FRI','tfl2-6'),'gi-2'
plantings = fit_plantings[c(1:8,9:10,11:21)]
plants = unlist(c(sapply(genotypes,function(gen) paste(gen,plantings,sep='::'))))
fit_data_individuals = full_data_individuals[full_data_individuals$plant %in% plants,]

source('prep_model_New.R')

# divide between optimization and validation
Validation_Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[11:21]]
Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[1:10]]


base_coefs = c(base_params,init_coefs_genotypes)

init = base_coefs[c('Col::F_b','FRI::F_b','Dayl_FT_equal_GA','log10_Signal_threshold','Tmin')] #,'HT_base'
init = base_coefs[c('Col::F_b','FRI::F_b','log10_FT_vs_GA','log10_Signal_threshold','Tmin')]
# init['log10_FT_vs_GA'] = log10(7.101957e-03 )
init['log10_Signal_threshold'] = log10(10^3.499362 ) #/ init['HT_base']
#
for(i in 1:length(init)) init[i] = runif(1,0,1)
init['log10_Signal_threshold'] = rnorm(1,7,3)
# init['Dayl_FT_equal_GA'] = runif(1,8,20)

coef_mat = c()
res = optim(init,obj_fun,control = list(trace=9,maxit = 200))
reres = optim(res$par,obj_fun,control = list(trace=9,maxit = 1000));res
obj_fun(res$par)

inplant = Plant_list[['gi-2::FIBR_HalleFall']]
plant = Plant_list[['vin 3-1::FIBR_CologneSpring']]
plant$get_params()

results = do.call(rbind,lapply(Plant_list,function(plant) {
  # plant$update_coefs(res$par)
  pred = plant$get_predicted_bolting_PTT()
  obs = plant$get_observed_bolting_PTTs()
  return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs)))
}))

# observed vs predicted plot
ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
res
