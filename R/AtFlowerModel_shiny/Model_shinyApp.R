
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
                                      max = slider_params[['slider_generic_params']][[param]][3],
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
                                      max = slider_params[['slider_genotype_params']][[param]][3],
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
                                      max = slider_params[['slider_switches']][[param]][3],
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
                                      max = slider_params[['slider_functions']][[param]][3],
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
                                      max = slider_params[['slider_plants']][[param]][3],
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
             tabPanel('plot_env_responses', uiOutput("plot_env_responses.ui"))
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
    Environment = input$Environment

    # choose plant to print
    gen = Genotype
    env = Environment
    if(gen == 'All') gen = genotypes[1]
    if(env == 'All') env = fit_plantings[1]

    new_coefs = sapply(c(names(slider_generic_params),names(slider_switches),names(slider_genotype_params)),function(param) input[[param]])
    names(new_coefs)[match(names(slider_genotype_params),names(new_coefs))] = genotype_coefs
    new_coefs = param_transformations(new_coefs)
    plant = Plant_list[[paste(gen,env,sep='::')]]
    plant$update_coefs(new_coefs)

    plot_env_responses(plant,cex_main = cex_main())
  })
  output$plot_env_responses.ui = renderUI({
    plotOutput('plot_env_responses',height = plotHeight())
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
      pred = plant$get_predicted_bolting_PTT()
      obs = plant$get_observed_bolting_PTTs()
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs)))
    }))

    # observed vs predicted plot
    if(input$Color_by == 'Genotype') {
      ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Genotype,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    } else {
      ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    }


  })														# reactiveUi adjusts height of plot.
  output$plot_training_dots.ui = renderUI({
    plotOutput('plot_training_dots',height = plotHeight())
  })

  output$plot_validation_dots = renderPlot({
    validation_plant_states_reactive()
    results = do.call(rbind,lapply(Validation_Plant_list,function(plant) {
      pred = plant$get_predicted_bolting_PTT()
      obs = plant$get_observed_bolting_PTTs()
      return(data.frame(Genotype = plant$gen, Treatment = plant$environ,pred = pred,obs = mean(obs),sd = sd(obs),N = length(obs)))
    }))

    # observed vs predicted plot
    if(input$Color_by == 'Genotype') {
      ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Genotype,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    } else {
      ggplot(results,aes(x=obs,y=pred)) + geom_point(aes(color=Treatment,size=N)) + geom_abline(intercept = 0,slope=1) + scale_size_area()
    }

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
