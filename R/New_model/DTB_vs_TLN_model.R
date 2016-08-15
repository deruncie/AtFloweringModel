## ------------------------------------------------------------------------

dataInd = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_data_individuals.csv')
# dataMean = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_lab_data_means.csv')

## ----warning=F,fig.height=5,fig.width=8----------------------------------
require(ggplot2)
data = droplevels(subset(dataInd,grepl('ACE2',ExpID) & grepl('Con',ExpID) & grepl('NV',ExpID) & !is.na(DTB + TLN)))
data$TempMean = as.numeric(substr(data$Temp,1,2))
ggplot(data,aes(x=TLN,y=DTB)) + geom_point(aes(color=interaction(Dayl,Temp))) + geom_smooth(aes(color=interaction(Dayl,Temp)),method=lm,se=F)

## ------------------------------------------------------------------------
TLN_data = read.csv('~/Box Sync/DER_projects/Leaves_vs_flowers/Pouteau/Pouteau_transfers_09.csv')
# ggplot(data,aes(x=Day,y=TLN)) + geom_point(aes(color=Trt))

lm1 = lm(TLN~0+Day + I(Day^2),subset(TLN_data,Trt=='light' & Day < 27))  # quadratic, no intercept
# lm1 = lm(TLN~0+Day,subset(TLN_data,Trt=='light' & Day < 27))           # linear, no intercept
# lm1 = lm(TLN~0+Day,subset(TLN_data,Trt=='light' & Day < 27))           # linear,  intercept


summary(lm1)
x = seq(0,30,length=10)
with(subset(TLN_data,Trt=='light'),plot(Day,TLN,xlim = c(0,30),ylim = c(0,40)))
lines(x,predict(lm1,newdata=data.frame(Day=x)))

inverse_fun = function(TLN,coefs){
  # quadratic, no intercept
  a = coefs[1]
  b = coefs[2]
  return(sqrt(TLN/b + a^2/(4*b^2)) - a/(2*b))
  
  #linear, no intercept
  # a = coefs[1]
  # return(TLN/a)
  # 
  # # linear, intercept
  # a = coefs[1]
  # b = coefs[2]
  # return((TLN-b)/a)
}

## ------------------------------------------------------------------------
ratio_22_vs_12 = 1.5#(22-3)/(12-3)
time_from_TLN = function(TLN,temp,lm1,ratio_22_vs_12){
  # Pouteau grew her plants at 20C
  Tb = (22 - 12*ratio_22_vs_12) / (1-ratio_22_vs_12)
  alpha = coef(lm1)[1] / (20-Tb)
  beta = coef(lm1)[2] / (20-Tb)^2
  
  a = (temp - Tb)*alpha
  b = (temp - Tb)^2*beta
  # (a - coef(lm1)[1])/coef(lm1)[1]
  # (b - coef(lm1)[2])/coef(lm1)[2]
  
  return(inverse_fun(TLN,c(a,b)))
}
pred_Transition = function(data,ratio_22_vs_12,fraction_TLN = 1) {
  for(temp in unique(data$TempMean)){
    i = data$TempMean == temp
    data$pred_Transition[i] = time_from_TLN(fraction_TLN*data$TLN[i],temp,lm1,ratio_22_vs_12)
  }
  return(data)
}
do_plot = function(ratio_22_vs_12,fraction_TLN=1) {
  TLN = seq(min(data$TLN),max(data$TLN))
  new_data = data.frame(TLN = rep(TLN,each = 2),TempMean = rep(c(12,22)))
  data = pred_Transition(data,ratio_22_vs_12,fraction_TLN)
  new_data = pred_Transition(new_data,ratio_22_vs_12,fraction_TLN)
  p = ggplot(data,aes(x=TLN,y=DTB)) + geom_point(aes(color=factor(TempMean))) + facet_grid(~Dayl) #+ geom_smooth(aes(color=interaction(Dayl,TempMean)),method=lm,se=F)
  p = p + geom_line(data = new_data,aes(x=TLN,y=pred_Transition,color=factor(TempMean),group = TempMean))
  print(p)
}
data = pred_Transition(data,ratio_22_vs_12,fraction_TLN=1)
do_plot(ratio_22_vs_12)

## ------------------------------------------------------------------------
plot2 = function(ratio_22_vs_12,fraction_TLN=1){
  data = pred_Transition(data,ratio_22_vs_12,fraction_TLN)
  data$time_to_bolt = (data$DTB - data$pred_Transition)*data$Dayl
  p = ggplot(data,aes(x=TLN,y=time_to_bolt)) + geom_point(aes(color=factor(TempMean))) + facet_grid(~Dayl) + geom_hline(yintercept = 160)
  print(p)
  return(data)
}
data = plot2(ratio_22_vs_12,fraction_TLN=1)

## ------------------------------------------------------------------------
library(QuickShinyApp)

plot3 = function(ratio_22_vs_12){
  Tb = (22 - 12*ratio_22_vs_12) / (1-ratio_22_vs_12)
  Temps = seq(-10,30)
  plot(Temps,pmax(0,(Temps-Tb)/(20-Tb)),'l')
  lines(Temps,pmax(0,(Temps-3)/(20-3)),col=2)
  abline(v=c(12,22))
}
slider_params = list(
  ratio_22_vs_12 = c(1.5,1.1,3.1),
  fraction_TLN = c(1,0,1)
)
call = list(plot1 = 'do_plot(ratio_22_vs_12,fraction_TLN)',
            plot2 = 'plot2(ratio_22_vs_12,fraction_TLN)',
            plot3 = 'plot3(ratio_22_vs_12)')
run_shiny(call,slider_params)


