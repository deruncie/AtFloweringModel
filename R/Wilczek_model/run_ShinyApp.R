#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
source('plotting_functions.R')
source('prep_model.R',chdir=T)
current_model = 'Wilczek'

slider_functions = list(
  Choose_model = c('Wilczek','New'),
  Optimize_threshold = c(TRUE,FALSE),
  DO_recover = c(FALSE,TRUE)
)

generic_params = c('T_base','D_SD','CSDL','CLDL','Pnight','T_vmax')
switches = c()

slider_generic_params = lapply(generic_params,function(param) param_range_list[[param]][c(3,1,2)])
names(slider_generic_params) = generic_params

slider_switches = lapply(switches,function(param) param_range_list[[param]][c(3,1,2)])
names(slider_switches) = switches

slider_genotype_params = lapply(genotype_coefs,function(coef){
  generic_coef = strsplit(coef,'::',fixed=T)[[1]][[2]]
  return(unlist(c(init_coefs_genotypes[coef],param_range_list[[generic_coef]][1:2])))
})
names(slider_genotype_params) = sub('::','_',genotype_coefs)

slider_plants = list(
  Genotype = c(genotypes,'All'),
  Environment = c('All',fit_plantings),
  Color_by = c('Genotype','Environment')
)

slider_params = list(slider_generic_params = slider_generic_params,
                     slider_genotype_params = slider_genotype_params,
                     slider_functions       = slider_functions,
                     slider_switches        = slider_switches,
                     slider_plants          = slider_plants)



# Run the application
source('../AtFlowerModel_shiny/Model_shinyApp.R',local=T)
shinyApp(ui = ui, server = server)

