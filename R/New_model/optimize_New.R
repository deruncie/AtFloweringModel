library(AtFloweringModel)
library(ggplot2)
source('prep_model_New.R')


base_coefs = c(base_params,init_coefs_genotypes)

# init = base_coefs[c('Col::F_b','FRI::F_b','fve::F_b','Dayl_FT_equal_GA','log10_Signal_threshold')]
init = base_coefs[c('Col::F_b','FRI::F_b','fve::F_b','log10_FT_vs_GA','log10_Signal_threshold')]
# init['log10_FT_vs_GA'] = log10(7.101957e-03 )

# for(i in 1:length(init)) init[i] = runif(1,0,1)
# init['log_Signal_threshold'] = rnorm(1,7,3)

res = optim(init,obj_fun,control = list(trace=9,maxit = 1000))
res = optim(res$par,obj_fun,control = list(trace=9,maxit = 1000));res
obj_fun(res$par)

plant = Plant_list[['gi-2::FIBR_HalleFall']]
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
