#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(ggplot2)
source('plotting_functions_New.R')
source('loadData_New.R')

# # select plants
genotypes = c('Col','Col FRI','vin 3-1', 'fve-3','flm','flm FRI')#,'tfl2-6') ,'gi-2','vin3-4 FRI'
# genotypes = c('Col','Col FRI','gi-2','vin3-4 FRI','vin 3-1')#,'tfl2-6')
# select plantings
# plantings = fit_plantings[c(1:8,9:10,11:21)]
plantings = fit_plantings[c(grep('ACE2',fit_plantings),grep('FIBR',fit_plantings))] #,grep('Vern',fit_plantings) #,grep('FIBR',fit_plantings)
plantings = plantings[!grepl('D_V',plantings)]
# plantings = fit_plantings[grep('Vern',fit_plantings)] #,


plants = unlist(c(sapply(genotypes,function(gen) paste(gen,plantings,sep='::'))))
fit_data_individuals = full_data_individuals[full_data_individuals$plant %in% plants,]

training_plants = plants[grep('FIBR',plants)]
validation_plants = plants[plants %in% training_plants == F]


# # # divide between optimization and validation
# # Validation_Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[11:21]]
# # Plant_list = Plant_list[plant_index$Treatment %in% fit_plantings[1:10]]
#
# # divide between optimization and validation
# # Validation_Plant_list = Plant_list[grep('Con',plant_index$Treatment)]
# # Validation_Plant_list = Plant_list[grep('FIBR',plant_index$Treatment)]
# Validation_Plant_list = Plant_list[grep('ACE2',plant_index$Treatment)]
# # Validation_Plant_list = Plant_list[grep('RI',plant_index$Treatment)]
# # Plant_list = Plant_list[grep('Vern',plant_index$Treatment)]
# Plant_list = Plant_list[names(Plant_list) %in% names(Validation_Plant_list) == F]

source('prep_model_New.R')

training_plants = training_plants[training_plants %in% names(Plant_list)]
validation_plants = validation_plants[validation_plants %in% names(Plant_list)]

Validation_Plant_list = Plant_list[validation_plants]
Plant_list = Plant_list[training_plants]


current_model = 'New'

slider_functions = list(
  Optimize_threshold = c(TRUE,FALSE),
  DO_recover = c(FALSE,TRUE)
)

generic_params = c('Tmin','T10','Topt','Tmax',
                   'TT_germ','TT_phyllochron',
                   'HT_min',
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
  Color_by = c('Genotype','Treatment','Dayl','Temp','Vern_length'),
  Stat = c('PTB','DTB')
)

slider_params = list(slider_generic_params = slider_generic_params,
                     slider_genotype_params = slider_genotype_params,
                     slider_functions       = slider_functions,
                     slider_switches        = slider_switches,
                     slider_plants          = slider_plants)



coef_mat = c()
# Run the application
source('../AtFlowerModel_shiny/Model_shinyApp.R',local=T)
shinyApp(ui = ui, server = server)

