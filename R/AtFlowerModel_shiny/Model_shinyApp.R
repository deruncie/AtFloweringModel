
library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  headerPanel("At Flowering Model"),

  fluidRow(
    column(2,
           tabsetPanel(
             tabPanel('generic_params',
                      lapply(names(slider_params[['slider_generic_params']]),function(param) {
                        if(is.numeric(slider_params[['slider_generic_params']][[param]])){
                          sliderInput(param,param,
                                      min = slider_params[['slider_generic_params']][[param]][2],
                                      max = slider_params[['slider_generic_params']][[param]][3]+0.1,
                                      value = slider_params[['slider_generic_params']][[param]][[1]])
                        } else{
                          selectInput(param,param,choices = slider_params[['slider_generic_params']][[param]])
                        }
                      })
             ),
             tabPanel('genotype_params',
                      lapply(names(slider_params[['slider_genotype_params']]),function(param) {
                        if(is.numeric(slider_params[['slider_genotype_params']][[param]])){
                          sliderInput(param,param,
                                      min = slider_params[['slider_genotype_params']][[param]][2],
                                      max = slider_params[['slider_genotype_params']][[param]][3]+0.1,
                                      value = slider_params[['slider_genotype_params']][[param]][[1]])
                        } else{
                          selectInput(param,param,choices = slider_params[['slider_genotype_params']][[param]])
                        }
                      })
             ),
             tabPanel('switches',
                      lapply(names(slider_params[['slider_switches']]),function(param) {
                        if(is.numeric(slider_params[['slider_switches']][[param]])){
                          sliderInput(param,param,
                                      min = slider_params[['slider_switches']][[param]][2],
                                      max = slider_params[['slider_switches']][[param]][3]+0.1,
                                      value = slider_params[['slider_switches']][[param]][[1]])
                        } else{
                          selectInput(param,param,choices = slider_params[['slider_switches']][[param]])
                        }
                      })
             ),
             tabPanel('functions',
                      lapply(names(slider_params[['slider_functions']]),function(param) {
                        if(is.numeric(slider_params[['slider_functions']][[param]])){
                          sliderInput(param,param,
                                      min = slider_params[['slider_functions']][[param]][2],
                                      max = slider_params[['slider_functions']][[param]][3]+0.1,
                                      value = slider_params[['slider_functions']][[param]][[1]])
                        } else{
                          selectInput(param,param,choices = slider_params[['slider_functions']][[param]])
                        }
                      })
             ),
             tabPanel('plants',
                      lapply(names(slider_params[['slider_plants']]),function(param) {
                        if(is.numeric(slider_params[['slider_plants']][[param]])){
                          sliderInput(param,param,
                                      min = slider_params[['slider_plants']][[param]][2],
                                      max = slider_params[['slider_plants']][[param]][3]+0.1,
                                      value = slider_params[['slider_plants']][[param]][[1]])
                        } else{
                          selectInput(param,param,choices = slider_params[['slider_plants']][[param]])
                        }
                      })
             ),
             tabPanel('Graphics',
                      sliderInput('plotHeight','plotHeight',min = 200,max = 2000,value = 650),
                      sliderInput('cex_axes','cex_axes',min = 0.01,max = 5,value = 1.66),
                      sliderInput('cex_main','cex_main',min = 0.01,max = 5,value = 2)
             )
           )
    ),

    column(10,
           tabsetPanel(
             # tabPanel('plot_densities', uiOutput("plot_densities.ui")),
             tabPanel('plot_training_dots', uiOutput("plot_training_dots.ui")),
             tabPanel('plot_validation_dots', uiOutput("plot_validation_dots.ui")),
             tabPanel('plot_training_boxes', uiOutput("plot_training_boxes.ui")),
             tabPanel('plot_validation_boxes', uiOutput("plot_validation_boxes.ui")),
             tabPanel('plot_env_responses', uiOutput("plot_env_responses.ui")),
             tabPanel('plot_obs_transition_days', uiOutput("plot_obs_transition_days.ui")),
             tabPanel('plot_GxE', uiOutput("plot_GxE.ui")),
             tabPanel('plot_DTB_vs_TLN', uiOutput("plot_DTB_vs_TLN.ui"))
           )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # recover()

  # variable to adjust height of plot
  plotHeight = reactive({input$plotHeight})

  # variable to adjust size of axis text
  cex_axes = reactive({input$cex_axes})

  # variable to adjust size of axis text
  cex_main = reactive({input$cex_main})

  current_model_reactive = reactive({
    if(current_model == input$Choose_model) return(current_model)
    current_model <<- switch(input$Choose_model,
                             Wilczek = {
                               # source('Wilczek_model/pred_model.R',chdir = TRUE)
                               'Wilczek'
                             },
                             New = {
                               # source('New_model/pred_New.R',chdir = TRUE)
                               'New'
                             }
    )
    for(param in names(input)){
      if(param %in% names(param_range_list)){
        updateSliderInput(session, inputId = param, value = param_range_list[[param]][3])
      }
    }
    for(param in names(init_coefs_genotypes)){
      alt_param = sub('::','_',param)
      if(alt_param %in% names(input)){
        new_value = init_coefs_genotypes[param][1]
        names(new_value) = NULL
        updateSliderInput(session, inputId = alt_param,
                          value =  new_value)
      }
    }
    return(current_model)
    # for(i in 1:length(slider_functions)){
    # 	fn = names(slider_functions)[i]
    # 	eval(parse(text=sprintf('%s <<- %s',fn,input[[fn]])))
    # }
  })

  current_plant_states_reactive = reactive({
    # recover()
    # model = current_model_reactive()
    # extract coefs from sliders
    new_coefs = sapply(c(names(slider_generic_params),names(slider_switches),names(slider_genotype_params)),function(param) input[[param]])
    names(new_coefs)[match(names(slider_genotype_params),names(new_coefs))] = genotype_coefs
    # new_coefs[is.na(new_coefs)] = 0

    new_coefs = param_transformations(new_coefs)
    print(new_coefs)
    print('')
    # update coefs per plant
    res = sapply(Plant_list,function(plant) plant$update_coefs(new_coefs))

    if(input$Optimize_threshold){
      print('optimizing')
      # re-optimize threshold
      init = c(log10_Signal_threshold = Plant_list[[1]]$get_params()['log10_Signal_threshold'])
      names(init) = 'log10_Signal_threshold'
      f = sprintf('function(x) obj_fun(c(%s = x))',names(init))
      res = optimise(f = eval(parse(text = f)),interval = param_range_list[[names(init)]])
    }

    if(!is.null(input$DO_recover) && input$DO_recover == T) recover()

    return()
  })

  validation_plant_states_reactive = reactive({
    # model = current_model_reactive()

    # extract coefs from sliders
    new_coefs = sapply(c(names(slider_generic_params),names(slider_switches),names(slider_genotype_params)),function(param) input[[param]])
    names(new_coefs)[match(names(slider_genotype_params),names(new_coefs))] = genotype_coefs

    # update coefs per plant
    new_coefs['log10_Signal_threshold'] = Plant_list[[1]]$get_params()['log10_Signal_threshold']
    new_coefs = param_transformations(new_coefs)
    print(new_coefs)
    res = sapply(Validation_Plant_list,function(plant) plant$update_coefs(new_coefs))

    if(!is.null(input$DO_recover) && input$DO_recover == T) recover()
    return()
  })

  output$plot_env_responses = renderPlot({
    # recover()
    # will make 4 plots: TT vs Temp, Ve vs Temp, Photoperiod_effect vs Daylength, FT_max vs Daylength

    # select plant:

    Genotype = input$Genotype
    Treatment = input$Treatment

    # choose plant to print
    gen = Genotype
    env = Treatment
    if(gen == 'All') gen = genotypes[1]
    if(env == 'All') env = plantings[1]
    plant_id = paste(gen,env,sep='::')

    new_coefs = sapply(c(names(slider_generic_params),names(slider_switches),names(slider_genotype_params)),function(param) input[[param]])
    names(new_coefs)[match(names(slider_genotype_params),names(new_coefs))] = genotype_coefs
    new_coefs = param_transformations(new_coefs)
    if(plant_id %in% names(Plant_list)) plant = Plant_list[[paste(gen,env,sep='::')]]
    if(plant_id %in% names(Validation_Plant_list)) plant = Validation_Plant_list[[paste(gen,env,sep='::')]]
    plant$update_coefs(new_coefs)

    plot_env_responses(plant,cex_main = cex_main())
  })
  output$plot_env_responses.ui = renderUI({
    plotOutput('plot_env_responses',height = plotHeight())
  })


  output$plot_obs_transition_days = renderPlot({
    # recover()
    current_plant_states_reactive()
    # new_coefs = sapply(c(names(slider_generic_params),names(slider_switches),names(slider_genotype_params)),function(param) input[[param]])
    # names(new_coefs)[match(names(slider_genotype_params),names(new_coefs))] = genotype_coefs
    #
    # new_coefs = param_transformations(new_coefs)
    # print(new_coefs)
    # print('')

    genotype = input$Genotype
    all_of_genotype = Plant_list[grep(paste0(genotype,'::'),names(Plant_list))]
    # all_of_genotype = c(all_of_genotype,Validation_Plant_list[grep(paste0(genotype,'::'),names(Validation_Plant_list))])

    # lapply(all_of_genotype,function(plant) plant$update_coefs(new_coefs))

    n = length(all_of_genotype)
    plantings = sub(paste0(genotype,'::'),'',names(all_of_genotype))
    sowing_dates = sapply(environ_data[plantings],function(x) x$Date[1])
    transition_days = sapply(all_of_genotype,function(plant) mean(plant$get_predicted_transition_from_observed_bolting()))
    print(transition_days)
    # print(td2)
    print('')
    bolting_days = sapply(all_of_genotype,function(plant) mean(plant$get_observed_bolting_days()))
    max_date = max(sapply(environ_data[plantings],function(x) max(x$Date)))
    base_date = as.POSIXlt('2005-12-31')
    mths <- seq(as.POSIXlt("2006-01-01", tz="GMT"),
                as.POSIXlt("2007-12-31", tz="GMT"),
                by="month")
    dt = 60*60*24
    # first_of_month = as.Date(c(sapply(1:12,function(x) sprintf('2005-%02d-01',x)),sapply(1:12,function(x) sprintf('2006-%02d-01',x))))
    op = par(mar=c(3,8,3,1)+.1)
    plot(mths,rep(1,length(mths)),type='n',ylim = c(1,n),ylab = '',xlab = '',yaxt = 'n',xaxt='n')
    o = order(sowing_dates)
    axis(2,at=1:n,plantings[o],las=2,cex.axis=.7)
    axis.POSIXct(side=1, at=mths,format = '%b',las=2)
    arrows(base_date+sowing_dates[o]*dt,1:n,base_date+sowing_dates[o]*dt+transition_days[o]*dt,lwd=6,col=1,length=0)
    arrows(base_date+sowing_dates[o]*dt+transition_days[o]*dt,1:n,base_date+sowing_dates[o]*dt+bolting_days[o]*dt,lwd=6,col=2,length=0)
    abline(v=mths,lwd=.2)

    pred_transition = sapply(all_of_genotype,function(plant) plant$get_predicted_transition_day())
    pred_bolting = sapply(all_of_genotype,function(plant) plant$get_predicted_bolting_day())
    arrows(base_date+sowing_dates[o]*dt,1:n-0.3,base_date+sowing_dates[o]*dt+pred_transition[o]*dt,lwd=6,col=1,length=0)
    arrows(base_date+sowing_dates[o]*dt+pred_transition[o]*dt,1:n-0.3,base_date+sowing_dates[o]*dt+pred_bolting[o]*dt,lwd=6,col=2,length=0)
    abline(v=mths,lwd=.2)

    par(op)
  })
  output$plot_obs_transition_days.ui = renderUI({
    plotOutput('plot_obs_transition_days',height = plotHeight())
  })

  output$plot_DTB_vs_TLN = renderPlot({
    # recover()
    current_plant_states_reactive()
    res_training = do.call(rbind,lapply(Plant_list,function(plant) data.frame(Type = 'Training',
                                                                              Genotype = plant$gen,
                                                                              Treatment = plant$environ,
                                                                              DTB = plant$get_predicted_bolting_day(),
                                                                              TLN = plant$get_numLeaves()[plant$get_predicted_transition_day()])))
    res_validation = do.call(rbind,lapply(Validation_Plant_list,function(plant) data.frame(Type = 'Validation',
                                                                                           Genotype = plant$gen,
                                                                                           Treatment = plant$environ,
                                                                                           DTB = plant$get_predicted_bolting_day(),
                                                                                           TLN = plant$get_numLeaves()[plant$get_predicted_transition_day()])))
    res = rbind(res_training,res_validation)
    res$ColorBy = res$Treatment
    if(input$Color_by == 'Genotype') res$ColorBy = res$Genotype
    if(input$Color_by == 'Dayl') res$ColorBy = c('SD','LD')[(grepl('LD',res$Treatment)+1)]
    if(input$Color_by == 'Temp') res$ColorBy = c('12','22')[(grepl('22',res$Treatment)+1)]
    if(input$Color_by == 'Vern_length') res$ColorBy = c('NV','V')[(grepl('_V',res$Treatment)+1)]
    ggplot(res,aes(x=TLN,y=DTB)) + geom_point(aes(color = ColorBy)) + facet_grid(~Type)
  })
  output$plot_DTB_vs_TLN.ui = renderUI({
    plotOutput('plot_DTB_vs_TLN',height = plotHeight())
  })

  output$plot_GxE = renderPlot({
    # recover()
    current_plant_states_reactive()
    validation_plant_states_reactive()

    gen1 = input$Genotype
    gen2 = input$Genotype2

    get_stat = function(gen,planting,Plant_list,stat,type) {
      id = paste0(gen,'::',planting)
      if(id %in% names(Plant_list) == F) return(NA)
      if(stat == 'PTB' & type == 'pred') return(Plant_list[[id]]$get_predicted_bolting_PTT())
      if(stat == 'DTB' & type == 'pred') return(Plant_list[[id]]$get_predicted_bolting_day())
      if(stat == 'PTB' & type == 'obs') return(mean(Plant_list[[id]]$get_observed_bolting_PTTs()))
      if(stat == 'DTB' & type == 'obs') return(mean(Plant_list[[id]]$get_observed_bolting_days()))
    }

    plantings = unique(sapply(names(Plant_list),function(x) strsplit(x,'::')[[1]][2]))
    validation_plantings = unique(sapply(names(Validation_Plant_list),function(x) strsplit(x,'::')[[1]][2]))
    stat = input$Stat
    pred_train = sapply(plantings,function(x) get_stat(gen1,x,Plant_list,stat,'pred') - get_stat(gen2,x,Plant_list,stat,'pred'))
    obs_train = sapply(plantings,function(x) get_stat(gen1,x,Plant_list,stat,'obs') - get_stat(gen2,x,Plant_list,stat,'obs'))
    pred_validation = sapply(validation_plantings,function(x) get_stat(gen1,x,Validation_Plant_list,stat,'pred') - get_stat(gen2,x,Validation_Plant_list,stat,'pred'))
    obs_validation = sapply(validation_plantings,function(x) get_stat(gen1,x,Validation_Plant_list,stat,'obs') - get_stat(gen2,x,Validation_Plant_list,stat,'obs'))

    op = par(mfrow=c(2,2))
    ranges_train = range(c(0,obs_train,pred_train),na.rm=T)
    plot(obs_train,pred_train,main = 'Training',xlim = ranges_train,ylim = ranges_train);abline(0,1)
    ranges_validation = range(c(0,obs_validation,pred_validation),na.rm=T)
    plot(obs_validation,pred_validation,main = 'Validation',xlim = ranges_validation,ylim = ranges_validation);abline(0,1)

    par(mar=c(10,3,3,1))
    o = order(obs_train)
    plot(obs_train[o],ylim = ranges_train,xaxt='n',xlab = '')
    points(pred_train[o],col=2)
    axis(1,at=1:length(obs_train),names(obs_train)[o],las=2,cex.axis=.5*cex_axes())


    par(mar=c(10,3,3,1))
    o = order(obs_validation)
    plot(obs_validation[o],ylim = ranges_validation,xaxt='n',xlab = '')
    points(pred_validation[o],col=2)
    axis(1,at=1:length(obs_validation),names(obs_validation)[o],las=2,cex.axis=.5*cex_axes())

    par(op)

  })
  output$plot_GxE.ui = renderUI({
    plotOutput('plot_GxE',height = plotHeight())
  })

  # output$plot_densities = renderPlot({
  #   # recover()
  #   current_plant_states = current_plant_states_reactive()
  #   Signal_threshold = optim_result_reactive()$Signal_threshold
  #   Time_SD = optim_result_reactive()$Time_SD
  #   Genotype = optim_result_reactive()$Genotype
  #   Environment = optim_result_reactive()$Environment
  #   plants = NULL
  #   if(Genotype == 'All' & Environment != 'All') {
  #     plants = paste(genotypes,Environment,sep='::')
  #     plants = plants[plants %in% names(current_plant_states)]
  #   }
  #   if(Genotype != 'All' & Environment == 'All') {
  #     plants = paste(Genotype,fit_plantings,sep='::')
  #     plants = plants[plants %in% names(current_plant_states)]
  #   }
  #   if(Genotype == 'All' & Environment == 'All'){
  #     plants = names(current_plant_states)
  #   }
  #   plot_plant_states = current_plant_states[plants]
  #   plot_prediction_densities(plot_plant_states,Signal_threshold,Time_SD,cex.axis=cex_axes(),cex_main = cex_main())
  # })														# reactiveUi adjusts height of plot.
  # output$plot_densities.ui = renderUI({
  #   plotOutput('plot_densities',height = plotHeight())
  # })


  output$plot_training_dots = renderPlot({
    current_plant_states_reactive()

    results = do.call(rbind,lapply(Plant_list,function(plant) {
      # plant$update_coefs(res$par)
      if(input$Stat == 'PTB') {
        pred = plant$get_predicted_bolting_PTT()
        obs = plant$get_observed_bolting_PTTs()
      }
      if(input$Stat == 'DTB') {
        pred = plant$get_predicted_bolting_day()
        obs = plant$get_observed_bolting_days()
      }
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs),subset(plant_index,Plant == plant$id)))
    }))

    results$ColorBy = as.factor(results[[input$Color_by]])
    ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=ColorBy,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area() + ggtitle(obj_fun(c(X=0)))
    # # observed vs predicted plot
    # if(input$Color_by == 'Genotype') {
    #   ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Genotype,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    # } else {
    #   ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    # }


  })														# reactiveUi adjusts height of plot.
  output$plot_training_dots.ui = renderUI({
    plotOutput('plot_training_dots',height = plotHeight())
  })

  output$plot_validation_dots = renderPlot({
    validation_plant_states_reactive()
    results = do.call(rbind,lapply(Validation_Plant_list,function(plant) {
      if(input$Stat == 'PTB') {
        pred = plant$get_predicted_bolting_PTT()
        obs = plant$get_observed_bolting_PTTs()
      }
      if(input$Stat == 'DTB') {
        pred = plant$get_predicted_bolting_day()
        obs = plant$get_observed_bolting_days()
      }
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs),subset(plant_index,Plant == plant$id)))
    }))

    results$ColorBy = as.factor(results[[input$Color_by]])
    if(is.null(results$ColorBy[1])) results$ColorBy = results$Treatment
    ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=ColorBy,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area() + ggtitle(obj_fun(c(X=0),plants = Validation_Plant_list))
    # # observed vs predicted plot
    # if(input$Color_by == 'Genotype') {
    #   ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Genotype,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    # } else {
    #   ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    # }

  })														# reactiveUi adjusts height of plot.
  output$plot_validation_dots.ui = renderUI({
    plotOutput('plot_validation_dots',height = plotHeight())
  })


  output$plot_training_boxes = renderPlot({
    current_plant_states_reactive()

    results = do.call(rbind,lapply(Plant_list,function(plant) {
      pred = plant$get_predicted_bolting_PTT()
      obs = plant$get_observed_bolting_PTTs()
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,Resid = obs - pred))
    }))

    # observed vs predicted plot
    ggplot(results,aes(y = Resid,x = Genotype)) + geom_boxplot(aes(color = Treatment)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))

  })														# reactiveUi adjusts height of plot.
  output$plot_training_boxes.ui = renderUI({
    plotOutput('plot_training_boxes',height = plotHeight())
  })

  output$plot_validation_boxes = renderPlot({
    validation_plant_states_reactive()

    results = do.call(rbind,lapply(Validation_Plant_list,function(plant) {
      pred = plant$get_predicted_bolting_PTT()
      obs = plant$get_observed_bolting_PTTs()
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,Resid = obs - pred))
    }))

    # observed vs predicted plot
    ggplot(results,aes(y = Resid,x = Genotype)) + geom_boxplot(aes(color = Treatment)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  })														# reactiveUi adjusts height of plot.
  output$plot_validation_boxes.ui = renderUI({
    plotOutput('plot_validation_boxes',height = plotHeight())
  })

}
