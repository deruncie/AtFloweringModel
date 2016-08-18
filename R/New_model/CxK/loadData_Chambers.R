#### Should RI_3 be included? What about RI_2 ####

library(gdata)
require(qtl)
data = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_data_individuals.csv')
data$Treatment = data$ExpID
load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
load('../../../data/Seaton_FT_max_fun.Robj')


raw_genotype_file = '~/Box Sync/DER_projects/Leaves_vs_flowers/Reenas_data/Col Kas QTL expt data/CxK_genotypes.csv'
raw_map_file = '~/Box Sync/DER_projects/Leaves_vs_flowers/Reenas_data/Col Kas QTL expt data/Geno Position 165 markers cM, bp.xlsx'
genotype_data = read.csv(raw_genotype_file,na = '-',check.names = F,stringsAsFactors = F)
genotype_data$Line = toupper(genotype_data$Line)
Lines = genotype_data$Line
map_data = read.xls(raw_map_file,stringsAsFactors=F)[,1:5]

# Key genes
target_genes = list(
  FRI = c(4,269026), # already with marker FRI
  FLM = c(1,28955637), # already with marker FLM-BsrI
  MAF2 = c(5,25982254), # already with marker MAF2KE
  PIE1 = c(3,4065042),
  GA5 = c(4,12990884)
)

# genotype_data's FLM marker is called FLM-BsrI
map_data$Marker[map_data$Marker == 'FLM'] = 'FLM-BsrI'

# make rotated geno_data (CSVsr)
geno_data_r = data.frame(map_data[,c('Marker','Chr','cM.Pos')],t(genotype_data[,match(map_data$Marker,colnames(genotype_data))]))
colnames(geno_data_r)[-c(1:3)] = Lines
colnames(geno_data_r)[2:3] = rep('',2)
colnames(geno_data_r)[1] = 'Line'

add_gene = function(gene,chr,Pos){
  target = geno_data_r[1,]
  target$Line = gene
  target[1,2] = chr
  Chr3 = subset(map_data,Chr==chr)
  pos = max(which(Chr3$Mb < Pos))
  target[1,3] = (Pos - Chr3$Mb[pos])/(Chr3$Mb[pos+1] - Chr3$Mb[pos]) * Chr3$cM.Int[pos+1] + Chr3$cM.Pos[pos]
  target[1,-c(1:3)] = NA
  return(target)
}
geno_data_r = rbind(geno_data_r,add_gene('PIE1',3,4065042))
geno_data_r = rbind(geno_data_r,add_gene('GA5',4,12990884))

geno_data_r$CS3879 = 'B'
geno_data_r$CS3880 = 'A'
Lines = c(Lines,'CS3879','CS3880')
extra_genotype_data = genotype_data[,colnames(genotype_data) %in% map_data$Marker == F]
write.csv(geno_data_r,file='geno_data_r.csv',row.names=F,na = '-')

# make rotated pheno_data (CSVsr)
pheno_data_r = data.frame(Line = genotype_data$Line,ID = 1:nrow(genotype_data))
write.table(t(pheno_data_r),file='pheno_data_r.csv',row.names=T,col.names = F,sep=',')


genotype_data = read.cross(format = 'csvsr',
                           file='geno_data_r.csv',
                           phefile = 'pheno_data_r.csv'
                           )
genotype_data = jittermap(genotype_data)

genotype_data = argmax.geno(genotype_data,map.function = 'morgan')
orig_genotypes = pull.geno(genotype_data)
rownames(orig_genotypes) = genotype_data$pheno$Line
imputed_genotypes = pull.argmaxgeno(genotype_data)
rownames(imputed_genotypes) = genotype_data$pheno$Line

# pull out data for training
data = data[!is.na(data$DTB),]
plantings = names(environ_data)
# genotypes = c('Col','Col FRI','vin 3-1','vin3-4 FRI','gi-2','fve-3')#,'Kas-1',"hua2-3 FRI","phyB-1",'co-2','hua2-3','gi-6','Ler-1','tfl2-6')

# Full data, but not flm
fit_plantings = c(plantings[grep('REF',plantings)],plantings[grep('PHO',plantings)],plantings[grep('VER',plantings)],plantings[grep('TEM',plantings)],plantings[grep('RI',plantings)])
i = data$Paper %in% c('FIBR','Repeated_planting_RILs','Liana_ACE','Liana_VernLength_TBA','Amity_Science','Alex_chamber') & !is.na(data$DTB) & data$Treatment %in% fit_plantings #data$Genotype %in% genotypes &
cols = match(fit_plantings,plantings)
full_data_individuals = drop.levels(data[i,])
full_data_individuals$plant = paste(full_data_individuals$Genotype,full_data_individuals$Treatment,sep='::')
full_data_individuals$Treatment = factor(full_data_individuals$Treatment)

rm(data)

environ_data = environ_data[fit_plantings]

