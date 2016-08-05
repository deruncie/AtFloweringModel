library(AtFloweringModel)

source('loadData_New.R')
load('../../data/Seaton_FT_max_fun.Robj')

genotypes = c('Col','Col FRI','vin 3-1', 'fve-3','gi-2')#,'vin3-4 FRI','tfl2-6')
plantings = fit_plantings[c(1:8,9:10,11:21)]
plants = unlist(c(sapply(genotypes,function(gen) paste(gen,plantings,sep='::'))))
fit_data_individuals = full_data_individuals[full_data_individuals$plant %in% plants,]

parameter_matrix_file = 'Genotype_parameter_matrix_New_init.csv'
genotype_parameter_values = 'Genotype_parameter_values_New_init.csv'

design_matrix_genotype_list = Build.design_matrix_genotype_list(parameter_matrix_file,genotypes)
param_range_list = Build.param_ranges(parameter_matrix_file)

genotype_coefs = colnames(design_matrix_genotype_list[[1]])

# select initial value for each parameter (only numeric ones)
base_params = as.list(sapply(names(param_range_list),function(x) as.numeric(param_range_list[[x]][3])))
base_params = unlist(base_params[!is.na(base_params)])

init_coefs_genotypes = read.csv(genotype_parameter_values,h=T,check.names=F,stringsAsFactors=F,row.names=1)
init_coefs_genotypes = init_coefs_genotypes[genotype_coefs,]
names(init_coefs_genotypes) = genotype_coefs

param_transformations = function(params){
  log_pars = names(params)[substr(names(params),1,5) == 'log10']
  for(p in log_pars){
    params[sub('log10_','',p)] = 10^params[p]
  }
  if(!is.na(params['TLN_SD'])){
    # set threshold based on GA signal in SDs at 20C
    params['Signal_threshold'] = 8*17*params['TLN_SD']
  }
  if(!is.na(params['Dayl_FT_equal_GA'])) {
    # set FT_vs_GA based on the daylength when the FT and GA signals are equal at the transition
    # print('find Dayl')
    params['FT_vs_GA'] = find_FT_vs_GA(params)
    params = params[names(params) != 'Dayl_FT_equal_GA']
  }
  if(!is.na(params['D_TtB'])){
    # set based on size of a plant that developed for 12 days in 10h at 20C (Pouteau et al 2009)
    cumPTT_transition = 12*10*(20-3)
    params['Bolting_threshold'] = cumPTT_transition * (params['D_TtB']*10*(20-3))
  }
  if(!is.na(params['T_vmax'])){
    for(p in c('T_vmin','k','xi','w')){
      if(! p %in% names(params)) params[p] = base_params[p]
      params = Calc.Ve_params(params)
    }
  }
  return(params)
}

genotype = 'vin 3-1'
genotype = 'Col'
env = 'FIBR_CologneFall'

Plant_list = list()
plant_index = c()
for(genotype in genotypes){
  design_matrix_genotype = design_matrix_genotype_list[[genotype]]
  param_ranges = t(sapply(names(base_params),function(param) param_range_list[[param]]))
  for(env in plantings){
    id = paste(genotype,env,sep='::')
    index = fit_data_individuals$Genotype == genotype & fit_data_individuals$Treatment == env
    if(sum(index) == 0) next

    plant = new(New_Plant,id,genotype,env,param_transformations(base_params),as.list(environ_data[[env]]))
    plant$set_genotype_info(param_ranges,design_matrix_genotype,param_transformations)
    plant$update_coefs(init_coefs_genotypes)
    plant$add_bolting_days(fit_data_individuals$DTB[index])
    new_plant_index = data.frame(Plant = id, Genotype = genotype, Treatment = env)
    plant_index = rbind(plant_index,new_plant_index)
    Plant_list[[id]] = plant
  }
}

Validation_Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[11:21]]
Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[1:10]]
