# specify how genotypes related to parameters.
# There are the model parameters, and then the genotype-specific parameters, with particular mappings between them, so that genotypes can share particular parameters
Build.design_matrix_genotype_list = function(file,genotypes){
  if(!class(genotypes) == 'character') stop('genotypes must be a character vector (i.e. not factor')
  genotype_parameters = read.csv(file,h=T,check.names=F,stringsAsFactors=F,row.names=1)[,-c(1:3)]
  if(!all(genotypes %in% colnames(genotype_parameters))) stop('genotypes missing from parameter file')
  genotype_parameters = genotype_parameters[,genotypes]

  # genotype_parameters = genotype_parameters[which(apply(genotype_parameters,1,function(x) length(unique(x))>1)),]
  genotype_parameters = genotype_parameters[rowSums(!genotype_parameters == '')>0,]
  parameter_list = unlist(lapply(1:nrow(genotype_parameters),function(i) unique(paste(genotype_parameters[i,],rownames(genotype_parameters)[i],sep='::'))))

  template_matrix = matrix(0,nr = nrow(genotype_parameters),ncol = length(parameter_list),dimnames = list(rownames(genotype_parameters),parameter_list))

  design_matrix_genotype_list = list()
  for(genotype in genotypes){
    design_matrix_genotype_list[[genotype]] = template_matrix
    for(param in rownames(genotype_parameters)){
      design_matrix_genotype_list[[genotype]][param,paste(genotype_parameters[param,genotype],param,sep="::")]=1
    }
  }
  return(design_matrix_genotype_list)
}

# The model parameters need to be constrained to valid ranges.
Build.param_ranges = function(file){
  param_ranges = read.csv(file,h=T,check.names=F,stringsAsFactors=F,row.names=1)[,1:3]
  param_range_list = lapply(1:nrow(param_ranges),function(i){
  data = param_ranges[i,]
  if(all(!is.na(as.numeric(data)))) {
    data = as.numeric(data)
    data[2] = data[2] + 0.1
  }
  data
  })
  names(param_range_list) = rownames(param_ranges)
  return(param_range_list)
}

# # Optimization is more efficient if all parameters are on the same scale.
# Build.param_transformations = function(param_names) {
#   param_transformation = list()
#   for(param in param_names) param_transformation[[param]] = function(x) x
#   param_transformation[['Vsat']] = function(x) 1e3 * x
#   param_transformation[['Signal_threshold']] = function(x) 1e3 * x
#   return(param_transformation)
# }

Fix_param_ranges = function(new_params, param_ranges){
  penalty = 0
  penalty = penalty + -1e6*pmin(new_params - param_ranges[,1],0)	# penalty for param < param_min
  penalty = penalty +  1e6*pmax(new_params - param_ranges[,2],0)	# penalty for param > param_max

  new_params = pmax(new_params,param_ranges[,1])
  new_params = pmin(new_params,param_ranges[,2])

  return(list(new_params = new_params, penalty = penalty))
}

# Calc_genotype_params = function(new_coefs, design_matrix_genotype, param_ranges, param_transformation){
#   # calculates the params for a genotype given a mapping from genotype to params (design_matrix_genotype)
#   # design_matrix_genotype is p x g for p parameters and g genotypes with each row listing the genotypes that affect that parameter
#   # rownames of this matrix give the names of the parameters in play
#   # this implies a linear mapping of genotypes to coefficients
#   #param_ranges constrains each parameter to a limited range (can include Inf)
#   # parameter values outside of this range contribute to a model penalty for optimization, and parameters are reset to appropriate ranges
#   # p x 2 matrix with columns param_min and param_max
#   # recover()
#   if(length(new_coefs) == 0) return(list(new_params = c(),penalth = 0))
#
#   new_params = design_matrix_genotype %*% new_coefs
#   names(new_params) = rownames(design_matrix_genotype)
#
#   # constrain parameters to valid ranges
#   # new_param_ranges = t(sapply(names(new_params),function(x) param_ranges[[x]]))
#   result = Fix_param_ranges(new_params, param_ranges)
#   new_params = result$new_params[,1]
#
#   # apply transformation to each parameter
#   result$new_params = sapply(names(new_params),function(param_name) param_transformation[[param_name]](new_params[[param_name]]))
#   return(result)
# }


Calc_genotype_params = function(new_coefs, design_matrix_genotype){
  # calculates the params for a genotype given a mapping from genotype to params (design_matrix_genotype)
  # design_matrix_genotype is p x g for p parameters and g genotypes with each row listing the genotypes that affect that parameter
  # rownames of this matrix give the names of the parameters in play
  # this implies a linear mapping of genotypes to coefficients
  #param_ranges constrains each parameter to a limited range (can include Inf)
  # parameter values outside of this range contribute to a model penalty for optimization, and parameters are reset to appropriate ranges
  # p x 2 matrix with columns param_min and param_max
  # recover()
  if(length(new_coefs) == 0) return(c())

  new_params = design_matrix_genotype %*% new_coefs
  names(new_params) = rownames(design_matrix_genotype)
  return(new_params)
}

Update_coefs_genotype = function(
  new_coefs,
  old_params,
  design_matrix_genotype,
  param_ranges,
  param_transformation
  ) {
  penalty = 0
  # some coefficients are directly elements of plant_state$params. Others are genotype-specific
  # recover()
  # for the generic ones, check that the ranges are OK. If some are out of the valid range, set penalty > 0

  # constant parameters
  # recover()
  genotype_specific_coefs = new_coefs[names(new_coefs) %in% colnames(design_matrix_genotype)]
  design_matrix_genotype = design_matrix_genotype[,names(genotype_specific_coefs),drop=F]
  design_matrix_genotype = design_matrix_genotype[rowSums(abs(design_matrix_genotype))>0,,drop=F]
  genotype_specific_params = c(Calc_genotype_params(genotype_specific_coefs,design_matrix_genotype[,names(genotype_specific_coefs)]))
  names(genotype_specific_params) = rownames(design_matrix_genotype)
  const_params = new_coefs[names(new_coefs) %in% colnames(design_matrix_genotype) == F]

  new_params = c(const_params,genotype_specific_params)


  if(length(new_params) > 0){
    new_params = new_params[names(new_params) %in% rownames(param_ranges)]
    result = Fix_param_ranges(new_params = unlist(new_params), matrix(param_ranges[names(new_params),],nr=length(new_params)))
    new_params = result$new_params
    penalty = penalty + sum(result$penalty)
    # new_params = sapply(names(new_params),function(param) param_transformation[[param]](new_params[[param]]))
  }
  new_params = param_transformation(new_params)


  # only return params that have changed
  new_params = new_params[new_params != old_params[names(new_params)]]
  return(list(new_params = new_params, penalty = penalty))
}

weighted_CV = function(obs,pred,N){
  sqrt(sum((obs - pred)^2*N)/sum(obs^2*N))
}

obj_fun = function(new_coefs){
  r = do.call(rbind,lapply(Plant_list,function(plant) {
    plant$update_coefs(new_coefs)
    pred = plant$get_predicted_bolting_PTT()
    obs = plant$get_observed_bolting_PTTs()
    return(data.frame(pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs)))
  }))

  penalty = sum(sapply(Plant_list,function(plant) plant$get_penalty()))
  # return(weighted_CV(r$obs,r$pred,r$N) + penalty)
  out = weighted_CV(1/r$obs,1/r$pred,r$N) + penalty
  if(is.na(out)) recover()
  if(out == Inf) recover()
  # print(c(out,new_coefs))
  return(out)
}



FT_per_leaf = function(Dayl,FT_LD_vs_SD) (FT_max_fun(Dayl)/FT_max_fun(6))^(log(FT_LD_vs_SD)/(log(FT_max_fun(16)/FT_max_fun(8))))
