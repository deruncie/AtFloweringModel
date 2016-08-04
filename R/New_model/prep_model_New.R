library(AtFloweringModel)

source('loadData.R')

genotypes = c('Col','Col FRI','vin 3-1', 'fve-3','gi-2')#,'vin3-4 FRI','tfl2-6')
plantings = fit_plantings[c(1:8,9:10,11:21)]
plants = unlist(c(sapply(genotypes,function(gen) paste(gen,plantings,sep='::'))))
fit_data_individuals = full_data_individuals[full_data_individuals$plant %in% plants,]

parameter_matrix_file = 'Genotype_parameter_matrix_Wilczek_init.csv'
genotype_parameter_values = 'Genotype_parameter_values_Wilczek_init.csv'

design_matrix_genotype_list = Build.design_matrix_genotype_list(parameter_matrix_file,genotypes)
param_range_list = Build.param_ranges(parameter_matrix_file)
param_transformation_list = Build.param_transformations(names(param_range_list))

genotype_coefs = colnames(design_matrix_genotype_list[[1]])

# select initial value for each parameter (only numeric ones)
base_params = as.list(sapply(names(param_range_list),function(x) as.numeric(param_range_list[[x]][3])))
base_params = unlist(base_params[!is.na(base_params)])
base_params = sapply(names(base_params),function(param) param_transformation_list[[param]](base_params[[param]]))
# base_params[!is.na(as.numeric(base_params))] = as.numeric(base_params[!is.na(as.numeric(base_params))])

init_coefs_genotypes = read.csv(genotype_parameter_values,h=T,check.names=F,stringsAsFactors=F,row.names=1)
init_coefs_genotypes = init_coefs_genotypes[genotype_coefs,]
names(init_coefs_genotypes) = genotype_coefs

wilczek_results = read.csv('~/Documents/Arabidopsis/Compendium_data/individual_experiments/data/FIBR/Amity_data/Wilczek_bolting_data.txt')
wilczek_results$Genotype = as.character(wilczek_results$Genotype)
wilczek_results$Genotype[wilczek_results$Genotype == "Col-FRI-Sf2"] = 'Col FRI'
wilczek_results$Genotype[wilczek_results$Genotype == "vin3-1"] = "vin 3-1"
wilczek_results = subset(wilczek_results,Genotype %in% genotypes)
wilczek_results$ID = with(wilczek_results,paste0(Genotype,'::FIBR_',Planting))

genotype = 'vin 3-1'
genotype = 'Col'
env = 'FIBR_CologneFall'

Plant_list = list()
plant_index = c()
for(genotype in genotypes){
  design_matrix_genotype = design_matrix_genotype_list[[genotype]]
  # param_ranges = t(sapply(rownames(design_matrix_genotype_list[[genotype]]),function(param) param_range_list[[param]]))
  param_ranges = t(sapply(names(base_params),function(param) param_range_list[[param]]))
  param_transformations = param_transformation_list[rownames(param_ranges)]
  for(env in plantings){
    id = paste(genotype,env,sep='::')
    # plant_index = rbind(plant_index,data.frame(Plant = id, Genotype = genotype, Treatment = env))
    index = fit_data_individuals$Genotype == genotype & fit_data_individuals$Treatment == env
    if(sum(index) == 0) next

    plant = new(Wilczek_Plant,id,genotype,env,base_params,as.list(environ_data[[env]]))
    plant$set_genotype_info(param_ranges,design_matrix_genotype,param_transformations)
    plant$update_coefs(init_coefs_genotypes)
    plant$add_bolting_days(fit_data_individuals$DTB[index])
    # plant$develop_n(251)
    # load('~/Desktop/plant.RData')
    #
    # plot(plant$get_Vern())
    # points(old_plant$Vern*(1-0.402)+0.402,col=2)
    # plot(diff(plant$get_Vern()),diff(old_plant$Vern*(1-0.402)+0.402));abline(0,1)
    #
    # plot(plant$get_size()-old_plant$active_tissues[,251])


    # plant$get_params()

    # PTT = colSums(matrix(with(environ_data[[env]],pmax(0,(Grnd.Tmp-3)*Hrs.light)),nr=24))
    # D = 0.626 + matrix(with(environ_data[[env]],(pmax(pmin(Daylength,14),10) - 10)/(14-10)*(1-0.626)),nr=24)[1,]
    # ts = cumsum(0.263*PTT*D)#[seq(12,length(D),by=24)]
    # plot(ts)
    # plant$develop_n(length(ts))
    # plot(ts,plant$get_Total_signal())
    #
    # plot(PTT,plant$get_size())

    # index = match(id,wilczek_results$ID)
    # plant$develop_n(wilczek_results$Mean_DTB[index]+1)
    # plant$get_predicted_bolting_day()
    # plant$add_bolting_days(wilczek_results$Mean_DTB[index])
    # plant$get_observed_bolting_PTTs()
    new_plant_index = data.frame(Plant = id, Genotype = genotype, Treatment = env)
    # new_plant_index$pred_DTB = plant$get_predicted_bolting_day()

    # # plant$add_bolting_days(wilczek_results$Mean_DTB[index])
    # new_plant_index$newMPTU = plant$get_Total_signal()[wilczek_results$Mean_DTB[index]+1]
    # new_plant_index$oldMPTU = wilczek_results$MPTU[index]
    # new_plant_index$oldMPTU2 = wilczek_results$new_MPTU[index]


    plant_index = rbind(plant_index,new_plant_index)
    Plant_list[[id]] = plant
  }
}

Validation_Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[11:21]]
Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[1:10]]


# plant$get_params()
#
# library(ggplot2)
# # ggplot(plant_index,aes(x=oldMPTU,y=newMPTU)) + geom_point(aes(color=Genotype))
# ggplot(plant_index,aes(x=oldMPTU,y=newMPTU)) + geom_point(aes(color=Treatment))
# with(plant_index,plot(oldMPTU,newMPTU))
#
# Plant_list = lapply(Plant_list,function(plant) {
#   plant$update_coefs(c(T_base = runif(1,0,5)))
#   plant$develop_n(250)
#   plant
#   # plot(plant$get_Total_signal())
# })
# plot(Plant_list[[1]]$get_Total_signal())
#
#
# cp = function(plant){
#   plant$develop_n(10)
# }
# cp(plant)

