#### Should RI_3 be included? What about RI_2 ####

library(gdata)
data = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_data_individuals.csv')
data$Treatment = data$ExpID
load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
# load('../../data/Seaton_FT_max_fun.Robj')

# pull out data for training
data = data[!is.na(data$DTB),]
plantings = names(environ_data)
# genotypes = c('Col','Col FRI','vin 3-1','vin3-4 FRI','gi-2','fve-3')#,'Kas-1',"hua2-3 FRI","phyB-1",'co-2','hua2-3','gi-6','Ler-1','tfl2-6')

# Full data, but not flm
fit_plantings = c(plantings[grep('FIBR',plantings)],plantings[grep('RI',plantings)],plantings[grep('ACE',plantings)],plantings[grep('Vern',plantings)],plantings[grep('CologneRP',plantings)])
fit_plantings = fit_plantings[fit_plantings %in% c('RI_Fall_2005','RI_Spring_2006')==F]
i = data$Paper %in% c('FIBR','Repeated_planting','Liana_ACE','Liana_VernLength_TBA','Amity_Science') & !is.na(data$DTB) & data$Treatment %in% fit_plantings #data$Genotype %in% genotypes &
cols = match(fit_plantings,plantings)
full_data_individuals = drop.levels(data[i,])
full_data_individuals$plant = paste(full_data_individuals$Genotype,full_data_individuals$Treatment,sep='::')
full_data_individuals$Treatment = factor(full_data_individuals$Treatment)

rm(data)

environ_data = environ_data[fit_plantings]

# remove stratification time from environ lists
for(env in fit_plantings){
	i = full_data_individuals$Treatment == env

	# Also, set DTB for plants that bolted after the end of the environ_data to the last day of the environ_data
	nDays = length(environ_data[[env]][[1]])/24
	# print(sum(full_data_individuals$DTB[i][full_data_individuals$DTB[i] > nDays]))
	# full_data_individuals$DTB[i][full_data_individuals$DTB[i] > nDays] = nDays

}




#### Should RI_3 be included? What about RI_2 ####

# library(gdata)
# data = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_data_individuals.csv')
# data$Treatment = data$ExpID
# load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')

# # pull out data for training
# data = data[!is.na(data$DTB),]
# plantings = names(environ_data)
# # genotypes = c('Col','Col FRI','vin 3-1','vin3-4 FRI','gi-2','fve-3')#,'Kas-1',"hua2-3 FRI","phyB-1",'co-2','hua2-3','gi-6','Ler-1','tfl2-6')

# # Full data, but not flm
# fit_plantings = c(plantings[grep('FIBR',plantings)],plantings[grep('RI',plantings)],plantings[grep('ACE',plantings)],plantings[grep('Vern',plantings)],plantings[grep('CologneRP',plantings)])
# fit_plantings = fit_plantings[fit_plantings %in% c('RI_Fall_2005','RI_Spring_2006')==F]
# i = data$Genotype %in% genotypes & data$Paper %in% c('FIBR','Repeated_planting','Liana_ACE','Liana_VernLength_TBA','Amity_Science') & !is.na(data$DTB) & data$Treatment %in% fit_plantings
# cols = match(fit_plantings,plantings)
# full_data_individuals = drop.levels(data[i,])
# full_data_individuals$plant = paste(full_data_individuals$Genotype,full_data_individuals$Treatment,sep='::')
# full_data_individuals$Treatment = factor(full_data_individuals$Treatment)

# rm(data)

# environ_data = environ_data[fit_plantings]

# # remove stratification time from environ lists
# for(env in fit_plantings){
# 	i = full_data_individuals$Treatment == env

# 	# Also, set DTB for plants that bolted after the end of the environ_data to the last day of the environ_data
# 	nDays = length(environ_data[[env]][[1]])/24
# 	# print(sum(full_data_individuals$DTB[i][full_data_individuals$DTB[i] > nDays]))
# 	# full_data_individuals$DTB[i][full_data_individuals$DTB[i] > nDays] = nDays

# }

# # load FT response function from Seaton et al 2015
# load('../Seaton_model_FT/Seaton_FT_max_fun.Robj')


# correct daylengths of CologneFall and ValenciaFall?
# environ_data$FIBR_CologneFall$Daylength[1:(14*24)] = 16
# environ_data$FIBR_ValenciaFall$Daylength[1:(14*24)] = 16
