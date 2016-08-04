library(AtFloweringModel)
library(ggplot2)
source('prep_model.R')


base_coefs = c(base_params,init_coefs_genotypes)

init = base_coefs[c('D_SD','Col::F_b','FRI::F_b','fve::F_b','log_Signal_threshold')]

for(i in 1:length(init)) init[i] = runif(1,0,1)
init['log_Signal_threshold'] = rnorm(1,7,3)

res = optim(init,obj_fun,control = list(trace=9))
res = optim(res$par,obj_fun,control = list(trace=9));res
obj_fun(res$par,Plant_list)

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


# to change a single paramter, and then re-optimize threshold:
a=sapply(Plant_list,function(plant) plant$update_coefs(c(T_base = 3)))
init = c(Signal_threshold = 2.3)
f = sprintf('function(x) obj_fun(c(%s = x),Plant_list = Plant_list)',names(init))
res = optimise(f = eval(parse(text = f)),interval = pmin(param_range_list[[names(init)]],10))
res

obs = do.call(rbind,lapply(Plant_list,function(plant) {
  obs = (plant$get_observed_bolting_PTTs())#^(-0.05)
  # return(data.frame(Genotype = plant$gen, Treatment = plant$environ,Mean = mean(obs), SD = sd(obs)))
  return(data.frame(Genotype = plant$gen, Treatment = plant$environ,obs = obs))
}))
# with(obs,cor(Mean,SD))
# ggplot(obs,aes(x=Mean,y=SD)) + geom_point(aes(color=Treatment))
library(car)
lm1 = lm(obs~Genotype:Treatment,obs)
res = powerTransform(lm1)
coef(res)


par(mfrow=c(1,2))
plot(lm1,which=c(1,2))
par(mfrow=c(1,1))
