#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
source('../plotting_functions_New.R')
source('loadData_Chambers.R')

fit_data_individuals = full_data_individuals#[full_data_individuals$plant %in% plants,]
source('prep_Chambers.R')
# # select plants
Validation_Plant_list = Plant_list[grep('RI_',names(Plant_list))]
Plant_list = Plant_list[names(Plant_list) %in% names(Validation_Plant_list) == F]
plantings = unique(as.character(plant_index$Treatment))

current_model = 'New'

slider_functions = list(
  Optimize_threshold = c(TRUE,FALSE)[2:1],
  DO_recover = c(FALSE,TRUE)
)

generic_params = c('Tmin','T10','Topt','Tmax',
                   'TT_germ','TT_phyllochron',
                   'SVP',
                   'Pnight',
                   'TLN_SD','Dayl_FT_equal_GA','D_TtB',
                   # 'log10_FT_vs_GA','log10_Signal_threshold','log10_Bolting_threshold',
                   'T_vmax')
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
  Genotype2 = c(genotypes),
  Treatment = c('All',plantings),
  Color_by = c('Genotype','Treatment','TRT_class','FRI','FLM','MAF2','GA5','KCK'),
  Stat = c('PTB','DTB')
)

slider_params = list(slider_generic_params = slider_generic_params,
                     slider_genotype_params = slider_genotype_params,
                     slider_functions       = slider_functions,
                     slider_switches        = slider_switches,
                     slider_plants          = slider_plants)



coef_mat = c()
# Run the application
source('../../AtFlowerModel_shiny/Model_shinyApp.R',local=T)
shinyApp(ui = ui, server = server)

