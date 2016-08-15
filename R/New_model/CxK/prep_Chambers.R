library(AtFloweringModel)

# impute genotypes for target genes:
# using R/Qtl
require(qtl)

genotypes = as.character(unique(subset(full_data_individuals,Paper=='Alex_chamber')$Genotype))
target_alleles = imputed_genotypes[genotypes,c('FRI','FLM-BsrI','MAF2KE','GA5')] #,'PIE1'
colnames(target_alleles) = c('FRI','FLM','MAF2','GA5') #,'PIE1'

GSPs = colnames(target_alleles)
design_matrix_genotype_list = lapply(genotypes,function(gen) {
  design_matrix = matrix(0,nr = length(GSPs),ncol = 2*length(GSPs))

})

parameter_matrix_file = 'Genotype_parameter_matrix_CxK_init.csv'
genotype_parameter_values = 'Genotype_parameter_values_CxK_init.csv'

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
  if(!is.na(params['FRI'])){
    params['F_n'] = 1-params['FRI']
  }
  if(!is.na(params['FLM']) | !is.na(params['MAF2']) | !is.na(params['SVP'])){
    params['HT_base'] = 1 - params['SVP']  - params['FLM'] - params['MAF2']
  }
  if(!is.na(params['GA5'])){
    params['GA_strength'] = params['GA5']
  }
  if(!is.na(params['TLN_SD']) & is.na(params['Signal_threshold'])){
    # set threshold based on GA signal in SDs at 20C
    params['Signal_threshold'] = 8*17*params['TLN_SD']
  }
  if(!is.na(params['Dayl_FT_equal_GA']) & is.na(params['FT_vs_GA'])) {
    # set FT_vs_GA based on the daylength when the FT and GA signals are equal at the transition
    # print('find Dayl')
    params['FT_vs_GA'] = find_FT_vs_GA(params)
    # params = params[names(params) != 'Dayl_FT_equal_GA']
  }
  if(!is.na(params['D_TtB'])){
    # set based on size of a plant that developed for 12 days in 10h at 20C (Pouteau et al 2009)
    cumPTT_transition = 12*10*(20-3)
    # params['Bolting_threshold'] = cumPTT_transition * (params['D_TtB']*10*(20-3)) # if model is size * PTT
    params['Bolting_threshold'] = (params['D_TtB']*10*(20-3)) # if model is just PTT
    # params['Bolting_threshold'] = (params['D_TtB']*10) # if model is just Daylength
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
    new_plant_index = data.frame(Plant = id, Genotype = genotype, Treatment = env,fit_data_individuals[index,][1,c('Temp','Dayl','Vern_length','Background')])
    plant_index = rbind(plant_index,new_plant_index)
    Plant_list[[id]] = plant
  }
}
