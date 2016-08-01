load('/Users/der7/Documents/Arabidopsis/Compendium_data/full_environ_data_list.Robj')
env = environ_data[[1]]
a = new(Environ,as.matrix(env)[rep(1:nrow(env),1000),])
#
a$get_PAR(1,'Noon')
a$get_PAR(1,'Midnight')
a$get_GrndTmp(1,'Noon')
a$get_GrndTmp(1,'Sundown')
library(microbenchmark)
microbenchmark(
sapply(1:1000,function(x) a$get_Daylength(x)),
sapply(1:1000,function(x) env$Daylength[24*(x-1)+12])
)
