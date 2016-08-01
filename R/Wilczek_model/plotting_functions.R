plot_env_responses = function(plant_state,gen,env,cex_main=1,...){
	# recover()
	par(mfrow=c(3,3))
	# plot 1: TT vs temperature
	Temps = seq(-3,35,length=50)
	plot(Temps,Calc.ThermalTimeIncrement(plant_state,list(Grnd.Tmp=Temps)),type='l',
		main = 'TT_inc',
		cex.lab = cex_main,cex.main = cex_main,...)

	# plot 2: Ve vs temperature
	Temps = seq(-5,20,length=50)
	plot(Temps,Calc.VernEffectiveHours(plant_state,list(Grnd.Tmp=Temps)),type='l',
		main = 'Ve',
		cex.lab = cex_main,cex.main = cex_main,...)
	
	# plot 3: Photoperiod_effect vs Daylength
	Daylength = seq(8,20,length=50)
	plot(Daylength,Calc.DSD(plant_state,list(Daylength=Daylength)),ylim=c(0,1),type='l',
		main = 'Phot',
		cex.lab = cex_main,cex.main = cex_main,...)

	# plot 4: Photoperiod_effect vs Daylength
	Daylength = seq(8,20,length=50)
	FT_max = Calc.FT.max(plant_state,list(Daylength=Daylength,Noon=T))
	plot(Daylength,FT_max,type='l',ylim=c(0,max(FT_max,na.rm=T)),
		main = 'FT_max',
		cex.lab = cex_main,cex.main = cex_main,...)
	abline(h=1);abline(v=16)

	# plot 5: FLC x FLM interaction
	K_repr = plant_state$params$K_repr
	n_repr = plant_state$params$n_repr
	FLM = seq(0,plant_state$params$FLMx,length=10)
	FLC = seq(0,plant_state$params$FLCx,length=10)
	activity = sapply(FLC,function(x) 1/(1+((FLM + x) / K_repr)^n_repr))
	activity = activity/min(activity)
	plot(FLM,rep(NA,length(FLM)),main = 'FLC x FLM repression',ylim = c(1,max(activity)),
		type='l',cex.lab = cex_main,cex.main = cex_main,...)
	apply(activity,1,function(x) lines(FLM,x))

	# plot 6: HT repression
	params = plant_state$params
	temps = seq(0,40,length=100)
	degr = Calc.HTfun(temps,params)(seq_along(temps))
	repressionHT = 1/(1+degr)
	K_repr = plant_state$params$K_repr
	n_repr = plant_state$params$n_repr
	FLMx = plant_state$params$FLMx
	FLCx = plant_state$params$FLCx
	activity = 1/(1+((FLMx*repressionHT + FLCx) / K_repr)^n_repr)
	activity = activity / activity[which(temps>=22)[1]]
	plot(temps,activity,type='l',ylim=c(0,max(activity)),xlim=c(0,30),
		cex.lab = cex_main,cex.main = cex_main,...)
	abline(h=1)
	abline(v=27)
	# # temps = environ_data[['RI_01']]$Grnd.Tmp
	# repression_HT = Calc.HighTempRepression(list(params = params),list(Grnd.Tmp = temps))
	# plot(repression_HT,ylim=c(0,1),type='l',
	# 	cex.lab = cex_main,cex.main = cex_main,...)

	# plot 7: FT hill function
	FT_hill_g = plant_state$params$FT_hill_g
	total_signal_FT = colSums(plant_state$exp_FT_by_tissue_time*plant_state$active_tissues)
	FT = total_signal_FT #/ (FT_hill_g + total_signal_FT)
	plot(total_signal_FT,FT,
		cex.lab = cex_main,cex.main = cex_main,...)
	abline(v = total_signal_FT[plant_state$predicted_transition_days])

	# plot 8: FT_by_leaf
	day = quantile(plant_state$predicted_transition_days,.5,type=1)
	signal_FT = plant_state$exp_FT_by_tissue_time[,day]*plant_state$active_tissues[,day]
	plot(signal_FT,
		cex.lab = cex_main,cex.main = cex_main,...)


	# plot 9: GA
	relative_importance_GA_FT = plant_state$params$relative_importance_GA_FT		
	total_signal_FT = colSums(plant_state$exp_FT_by_tissue_time*plant_state$active_tissues) 	# Leaf_inc already incorporated into active_tissues
	total_signal_GA = plant_state$GA_activity*relative_importance_GA_FT
	plot(total_signal_FT, type='l',ylim=c(0,max(c(total_signal_FT,total_signal_GA))),
		cex.lab = cex_main,cex.main = cex_main,...)
	lines(total_signal_GA,col=2)
}

plot_prediction_densities = function(plot_plant_states,Signal_threshold,Time_SD = 0.1,from=0,cex_main=1,...){
	# make a plot of predicted bolting dates for each plant in simulation_plant_states
	# plot should be a set of horizontal histograms on the same axes that line up day 0 on the left, and go to the maximum bolting date on the right
	# the actual bolting dates should be marked

	n_plants = length(plot_plant_states)
	plant_names = names(plot_plant_states)
	plant_names = sub('::','\n',plant_names,fixed=T)

	densities = lapply(plot_plant_states,function(plant_state) {
		data.frame(x=1:plant_state$num_days,
				   y=Calc.BoltingDensity(plant_state,Signal_threshold,Time_SD)
		)
	})
	range_predictions = c(0,max(unlist(lapply(densities,function(x) x$x))))
	range_densities = c(0,max(unlist(lapply(densities,function(x) x$y))))
	par(mfrow=c(1,1))
	par(mar=c(5,7,1,1)+.1)
	plot(NA,NA,xlim = range_predictions,ylim=c(0,n_plants),xlab = 'DTB',ylab = '',yaxt='n',...)
	cex.axis = 0.7
	dots = list(...)
	if('cex.axis' %in% names(dots)) cex.axis = 0.7*dots$cex.axis
	axis(2,at=(1:n_plants)-.75,labels = plant_names,las=2,cex.axis=cex.axis)
	abline(h=(1:n_plants)-1)

	for(i in 1:length(densities)){
		den = densities[[i]]
		lines(den$x,(i-1)+den$y/range_densities[2])
		plant_ID = names(plot_plant_states)[i]
		observed_DTB = plot_plant_states[[i]]$observed_bolting_days
		points(observed_DTB,rep(i-1,length(observed_DTB)))
	}
}

text_pos = function(x,y,nr,nc){
	# full device is (0,1) x (0,1)
	return(c(x=(x-1)/nc,y=(nr-y)/nr))
}

plot_table = function(table,color,...){
	n_rows = nrow(table)

	nc = floor(n_rows / 4)
	nr = ceiling(n_rows / nc)

	current_row = 1
	current_col = 1
	pos = c()
	for(i in 1:n_rows){		
		pos = rbind(pos,text_pos(current_col,current_row,nr,nc))
		current_row = current_row + 1
		if(current_row > nr) {
			current_row = 1
			current_col = current_col + 1
		}
	}
	plot(NA,NA,xlim=c(0,1),ylim=c(0,1),bty='n',xaxt='n',yaxt='n',xlab='',ylab='')
	text(pos[,1],pos[,2],label=apply(table,1,paste,collapse='   '),pos = 4,offset = 1,...)
	points(pos[,1],pos[,2],col=factor(color),...)
}

# X = matrix(1:30,10)
# plot_table(X,1:10)

plot_ID_lines = function(predicted,observed,ID){
	sapply(1:length(levels(ID)),function(i) {
		index = ID==levels(ID)[i]
		mean_obs = tapply(observed[index],predicted[index],mean,na.rm=T)
		mean_obs = mean_obs[order(as.numeric(names(mean_obs)))]
		lines(as.numeric(names(mean_obs)),mean_obs,col=i)
	})
}

plot_prediction_means = function(plot_plant_states,Signal_threshold,colorBy = 'ID',cex_main=1,...){
	# recover()
	results_list = lapply(plot_plant_states,function(plant_state){
		data.frame(
			Genotype = plant_state$genotype,
			Environment = plant_state$env,
			Observed_DTB = plant_state$observed_bolting_days, 
			Predicted_DTB = min(Calc.DaysToEvent(plant_state,Pred.TimeToBolt(plant_state,Signal_threshold)),plant_state$num_days,na.rm=T),
			Observed_PTU_to_B = plant_state$TimeToBolt, 
			Predicted_PTU_to_B = min(Pred.TimeToBolt(plant_state,Signal_threshold),max(plant_state$timescale),na.rm=T),
			stringsAsFactors=T
		)
	})
	results = do.call(rbind,results_list)
	# recover()
	results$ID = interaction(results$Genotype,results$Environment)
	RMSE_DTB = sqrt(mean((results$Predicted_DTB-results$Observed_DTB)^2,na.rm=T))
	RMSE_PTU = sqrt(mean((results$Predicted_PTU_to_B-results$Observed_PTU_to_B)^2,na.rm=T))
	R2_DTB = cor(results$Predicted_DTB,results$Observed_DTB)^2
	R2_PTU = cor(results$Predicted_PTU_to_B,results$Observed_PTU_to_B)^2
	par(mfrow=c(2,2))

	results_tab = do.call(rbind,lapply(levels(results[[colorBy]]),function(id) {
		i = results[[colorBy]] == id
		data.frame(
			RMSE_DTB = sqrt(mean((results$Predicted_DTB-results$Observed_DTB)[i]^2,na.rm=T)),
			RMSE_PTU = sqrt(mean((results$Predicted_PTU_to_B-results$Observed_PTU_to_B)[i]^2,na.rm=T))
		)}))
	results_tab = cbind(ID = levels(results[[colorBy]]),results_tab)

	cex.axis = 1
	dots = list(...)
	if('cex.axis' %in% names(dots)) cex.axis = 1*dots$cex.axis

	plot(results$Predicted_DTB,results$Observed_DTB,col = factor(results[[colorBy]])
		,main = sprintf('DTB RMSE=%0.2f R2=%0.2f',RMSE_DTB,R2_DTB)
		,cex.main = cex_main,
		,xlab = 'Predicted',ylab = 'Observed'
		,cex.lab = cex_main,cex.axis = cex.axis,...)
	abline(0,1)
	plot_ID_lines(predicted = results$Predicted_DTB,observed = results$Observed_DTB,results[[colorBy]])

	plot_table(results_tab[,1:2],1:nrow(results_tab),cex=cex_main)
	# plot(NA,NA,xlim=c(0,1),ylim=c(0,1),bty='n',xaxt='n',yaxt='n',xlab=NULL,ylab=NULL,...)
	# legend('center',legend = levels(results[[colorBy]]),pch=1,col=1:100,box.lwd=0,ncol=5,cex = cex_main)

	plot(results$Predicted_PTU_to_B,results$Observed_PTU_to_B,col = factor(results[[colorBy]])
		,main = sprintf('DTB RMSE=%0.2f R2=%0.2f',RMSE_PTU,R2_PTU),
		,cex.main = cex_main,
		,xlab = 'Predicted',ylab = 'Observed'
		,cex.lab = cex_main,cex.axis = cex.axis,...)
	abline(0,1)
	plot_ID_lines(predicted = results$Predicted_PTU_to_B,observed = results$Observed_PTU_to_B,results[[colorBy]])

	plot_table(results_tab[,c(1,3)],1:nrow(results_tab),cex=cex_main)
}


plot_prediction_boxes = function(plot_plant_states,Signal_threshold,colorBy = 'ID',cex_main=1,...){
	# recover()
	results_list = lapply(plot_plant_states,function(plant_state){
		data.frame(
			Genotype = plant_state$genotype,
			Environment = plant_state$env,
			Observed_DTB = plant_state$observed_bolting_days, 
			Predicted_DTB = min(Calc.DaysToEvent(plant_state,Pred.TimeToBolt(plant_state,Signal_threshold)),plant_state$num_days,na.rm=T),
			Observed_PTU_to_B = plant_state$TimeToBolt, 
			Predicted_PTU_to_B = min(Pred.TimeToBolt(plant_state,Signal_threshold),max(plant_state$timescale),na.rm=T),
			stringsAsFactors=T
		)
	})
	results = do.call(rbind,results_list)
	results$Environment = factor(results$Environment)
	# recover()
	# if(colorBy == 'Genotype'){
	# 	results$ID = interaction(results$Genotype,results$Environment)
	# } else {
	# 	results$ID = interaction(results$Environment,results$Genotype)		
	# }
	RMSE_DTB = sqrt(mean((results$Predicted_DTB-results$Observed_DTB)^2,na.rm=T))
	RMSE_PTU = sqrt(mean((results$Predicted_PTU_to_B-results$Observed_PTU_to_B)^2,na.rm=T))
	R2_DTB = cor(results$Predicted_DTB,results$Observed_DTB)^2
	R2_PTU = cor(results$Predicted_PTU_to_B,results$Observed_PTU_to_B)^2
	par(mar=c(20,3,3,1))
	genotypes = as.numeric(factor(sapply(results_list,function(x) unique(x$Genotype))))+1
	env = sapply(results_list,function(x) unique(x$Environment))	
	if(colorBy == 'Genotype'){
		results_list = results_list[order(genotypes)]	
		colors = genotypes[order(genotypes)]
	} else {
		results_list = results_list[order(env)]	
		colors = as.numeric(factor(env[order(env)]))+1
	}
	boxplot(lapply(results_list,function(x) log2(x$Observed_PTU/x$Predicted_PTU)),las=2,col = colors,
		main = sprintf('PTU RMSE=%0.2f R2=%0.2f',RMSE_PTU,R2_PTU),cex.main = cex_main,ylim=c(-1,1),...)
	abline(h=0)
}
