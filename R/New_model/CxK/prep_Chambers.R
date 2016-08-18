library(AtFloweringModel)

# impute genotypes for target genes:
# using R/Qtl
require(qtl)
parameter_matrix_file = 'Genotype_parameter_matrix_CxK_init.csv'
genotype_parameter_values = 'Genotype_parameter_values_CxK_init.csv'

genotypes = unique(c(sort(as.character(subset(full_data_individuals,Paper %in% c('Alex_chamber'))$Genotype)),
                     sort(as.character(subset(full_data_individuals,Paper %in% c('Repeated_planting_RILs'))$Genotype))))
target_alleles = imputed_genotypes[genotypes,c('FRI','FLM-BsrI','MAF2KE','PIE1','GA5')]
colnames(target_alleles) = c('FRI','FLM','MAF2','PIE1','GA5')

# select initial value for each parameter (only numeric ones)
param_range_list = Build.param_ranges(parameter_matrix_file)
base_params = as.list(sapply(names(param_range_list),function(x) as.numeric(param_range_list[[x]][3])))
base_params = unlist(base_params[!is.na(base_params)])

init_coefs_genotypes = read.csv(genotype_parameter_values,h=T,check.names=F,stringsAsFactors=F,row.names=1)
GSPs = rownames(init_coefs_genotypes)
Genes = init_coefs_genotypes$Gene
init_coefs_genotypes = c(t(init_coefs_genotypes[,-1]))
genotype_coefs = c(rbind(paste('Col',GSPs,sep='::'),paste('Kas',GSPs,sep='::')))
names(init_coefs_genotypes) = genotype_coefs

design_matrix_genotype_list = lapply(genotypes,function(gen) {
  design_matrix = matrix(0,nr = length(GSPs),ncol = length(init_coefs_genotypes),dimnames = list(GSPs,genotype_coefs))
  genotypes = target_alleles[gen,]
  for(i in 1:length(GSPs)){
    if(genotypes[Genes[i]] == 3) design_matrix[GSPs[i],paste('Col',GSPs[i],sep='::')] = 1
    if(genotypes[Genes[i]] == 1) design_matrix[GSPs[i],paste('Kas',GSPs[i],sep='::')] = 1
  }
  design_matrix
})
names(design_matrix_genotype_list) = genotypes


param_transformations = function(params,old_params = c()){
  all_params = c(params,old_params)  # extracting from this will take new param value if it exists, otherwise old value
  log_pars = names(params)[substr(names(params),1,5) == 'log10']
  for(p in log_pars){
    params[sub('log10_','',p)] = 10^params[p]
  }
  if(!is.na(params['FRI'])){
    params['F_b'] = 1-params['FRI']
  }
  if(!is.na(params['FLM']) || !is.na(params['MAF2']) || !is.na(params['SVP'])){
    params['HT_base'] = 1 - all_params['SVP']  - all_params['FLM'] - all_params['MAF2']
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
  if(!is.na(params['D_TtB']) || !is.na(params['GA5_bolt'])){
    # set based on size of a plant that developed for 12 days in 10h at 20C (Pouteau et al 2009)
    cumPTT_transition = 12*10*(20-3)
    # params['Bolting_threshold'] = cumPTT_transition * (params['D_TtB']*10*(20-3)) # if model is size * PTT
    params['Bolting_threshold'] = (all_params['D_TtB']*10*(20-3))/all_params['GA5_bolt'] # if model is just PTT
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

fit_data_individuals = cbind(fit_data_individuals,target_alleles[as.character(fit_data_individuals$Genotype),])
