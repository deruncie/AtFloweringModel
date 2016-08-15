require(ggplot2)
require(QuickShinyApp)

dataInd = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_data_individuals.csv')
dataMean = read.csv('/Users/der7/Documents/Arabidopsis/Compendium_data/Compendium_lab_data_means.csv')

data = droplevels(subset(dataInd,grepl('ACE2',ExpID) & grepl('Con',ExpID) & grepl('NV',ExpID)))


data$TempMean = as.numeric(substr(data$Temp,1,2))
ggplot(data,aes(x=TLN,y=DTB)) + geom_point(aes(color=interaction(Dayl,Temp))) + geom_smooth(aes(color=interaction(Dayl,Temp)),method=lm,se=F)
plant = Plant_list[[1]]
D_TtB=2
scale = 8
do_plot = function(D_TtB,scale,data){
  cumPTT_transition = 22*10*(20-3)
  Bolting_threshold = cumPTT_transition * (D_TtB*10*(20-3))
  # Bolting_threshold = plant$get_params()['Bolting_threshold']*1
  TT_germ = plant$get_params()['TT_germ']
  TT_phyllochron = plant$get_params()['TT_phyllochron']
  # recover()
  with(data,plot(TLN,DTB,col = interaction(Dayl,Temp),cex=.2))

  TTs = c(plant$TT_fun(12,1),plant$TT_fun(22,1))
  # TTs = c(22,25)

  new_data = data.frame(Dayl = rep(c(8,16),each=2),Temp = rep(c('12Con','22Con'),2),TLN = rep(seq(10,80),each = 4))
  new_data = new_data[order(new_data$Dayl,new_data$Temp),]

  # new_data = data
  new_data$PTU_per_day = new_data$Dayl * TTs[grepl('22',new_data$Temp)+1]
  new_data$TT_per_day = 24 * TTs[grepl('22',new_data$Temp)+1]
  new_data$TT_transition = new_data$TLN*TT_phyllochron
  new_data$Day_emerge = TT_germ/new_data$TT_per_day
  new_data$Day_transition = new_data$TLN*TT_phyllochron/new_data$TT_per_day
  new_data$Day_transition = sqrt((new_data$TLN - 8)*TT_phyllochron/new_data$PTU_per_day/2)*scale
  scale = 8
  # new_data$Day_transition = sqrt(2*(new_data$TLN-8)*TT_phyllochron^2/(new_data$PTU_per_day))*scale # * new_data$TT_per_day
  # with(new_data,plot(Day_transition,TLN))

  new_data$PTU_transition = (new_data$Day_transition - new_data$Day_emerge) * new_data$PTU_per_day
  # ggplot(new_data,aes(x=TLN,y=PTU_transition)) + geom_point(aes(color=interaction(Dayl,Temp)))
  # ggplot(new_data,aes(x=LLB,y=PTU_transition)) + geom_point(aes(color=interaction(Dayl,Temp)))
  # ggplot(new_data,aes(x=TLN,y=LLB)) + geom_point(aes(color=interaction(Dayl,Temp)))
  # # new_data$PTU_bolt = (new_data$DTB - new_data$Day_emerge) * new_data$PTU_per_day
  # new_data$PTU_ttoB = new_data$PTU_bolt - new_data$PTU_transition
  # ggplot(new_data,aes(x=TLN,y=PTU_ttoB)) + geom_point(aes(color=interaction(Dayl,Temp)))
  # ggplot(new_data,aes(x=Day_transition,y=DTB)) + geom_point(aes(color=interaction(Dayl,Temp)))

  # new_datata$pred_PTU_bolting = new_data$PTU_transition + Bolting_threshold/new_data$PTU_transition
  new_data$Bolt_days = (Bolting_threshold/new_data$PTU_transition)/new_data$PTU_per_day
  # ggplot(new_data,aes(x=TLN,y=Bolt_days)) + geom_point(aes(color=interaction(Dayl,Temp)))
  # ggplot(new_data,aes(x=Day_transition,y=TLN)) + geom_point(aes(color=interaction(Dayl,Temp)))
  new_data$pred_DTB = new_data$Day_transition + new_data$Bolt_days
  new_data$color = interaction(new_data$Dayl,new_data$Temp)
  with(new_data,tapply(1:nrow(new_data),list(Dayl,Temp),function(x) lines(TLN[x],pred_DTB[x],col= color[x])))
}

slider_params = list(
  D_TtB = c(4,0,160.1),
  scale = c(4,1,20.1)
)

call = 'do_plot(D_TtB,scale,subset(data,grepl("Con",Temp)))'
run_shiny(call,slider_params)


do_plot = function(a_,b_,B0_,Tb,Db,L_exp,t1_exp,show_t1,show_t2){
  # alpha = 10^alpha_
  B0 = 10^B0_
  new_data = data.frame(Dayl = rep(c(8,16)-Db,each=2),TempMean = rep(c(12,22)-Tb,2),L = rep(seq(10,100),each = 4))
  new_data = new_data[order(new_data$Dayl,new_data$TempMean),]
  with(data,plot(TLN,DTB,col = interaction(Dayl,Temp),cex=.2,ylim = c(0,max(DTB))))
  with(new_data, {
    # recover()
    a = TempMean*a_/(20-Tb)
    b = TempMean^2*b_/(20-Tb)^2
    # L = seq(30,100)
    # t1 = (L/(alpha*TempMean))^(2^L_exp)
    t1 = sqrt(L/b - (1/2*a/b)^2) - a/(2*b)
    t2 = B0/(Dayl^2*TempMean^2*t1^t1_exp)
    t2 = B0/(Dayl^2*TempMean^2*t1^t1_exp)
    points(L,show_t1*t1 + show_t2*t2,pch=21,bg = interaction(new_data$Dayl,new_data$TempMean),col=0)
  })
}
slider_params = list(
  # alpha = c(0,-10,10.1),
  a_ = c(0.5,0,2.1),
  b_ = c(0.04,0,0.21),
  B0 = c(5.6,-10,10.1),
  Tb = c(3,-10,11.9),
  Db = c(6,0,7.9),
  L_exp = c(0,-2,2.1),
  t1_exp = c(1,-2,2.1),
  show_t1 = c(1,0,1),
  show_t2 = c(0,0,1)
)

call = 'do_plot(a_,b_,B0,Tb,Db,L_exp,t1_exp,show_t1,show_t2)'
run_shiny(call,slider_params)
