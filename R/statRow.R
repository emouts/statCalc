statRowUI <- function(id, pokemon, stat.id){
  ns <- NS(id)
  
  stat.long <- stat.names.long[stat.id]
  
  # set up the layout of the row
  fluidRow(
    column(width = 1, align = "left", tags$strong(stat.names.long[stat.id])),
    column(width = 1, customStaticButtonConstant(params.list[[pokemon]]$base.stats[stat.id])),
    column(width = 1, customStaticButtonConstant(ivStarter)),
    column(width = 1, textOutput(ns("av"), container = customStaticButtonOutput)),
    column(width = 1, statUpdateButtonUI(ns("stat"))),
    column(width = 1, textOutput(ns("last_update"), container = customStaticButtonOutputLight)),
    column(width = 1, textOutput(ns("range"), container = customStaticButtonOutput))
  )
}

statRowServer <- function(id, pokemon, stat.id, level.in, nature.id, happiness, level.out){
  stopifnot({
    !is.reactive(pokemon)
    !is.reactive(stat.id)
    is.reactive(level.in)
    is.reactive(nature.id)
    is.reactive(happiness)
    is.reactive(level.out)
  })
  
  moduleServer(id, function(input, output, session){
    
    # whenever level, nature, or happiness change, update the expected stat
    stat.noAVs <- reactive(statCalcFun(pokemon = pokemon,
                                       stat = stat.id,
                                       iv = ivStarter,
                                       av = 0,
                                       level = level.in(),
                                       nature = nature.id(),
                                       happiness = happiness()))
    
    stat.button.outs <- statUpdateButtonServer("stat", stat.noAVs)
    
    # keep track of the last level when the stat was seen by the runner
    last.update.level <- eventReactive(stat.button.outs$changed(), level.in())
    observeEvent(last.update.level(), {
      output[["last_update"]] <- renderText(paste("Level ", last.update.level()))
    })
    
    # whenever the stat updates, update the AV (if the stat change was due
    # to a change in level, nature, or happiness, this does nothing)
    av <- eventReactive(stat.button.outs$changed(), 
                        stat.button.outs$value() - stat.noAVs())
    output[["av"]] <- renderText(av())
    
    
    # calculate the stat range at the target level; the calculation takes into
    # account the difference between the target level and last update level
    stat.range.min <- reactive(
      statCalcFun(
        pokemon = pokemon,
        stat = stat.id,
        iv = ivStarter,
        av = av(),
        level = level.out(),
        nature = nature.id(),
        happiness = happiness()
      )
    )
    stat.range.max <- reactive(
      stat.range.min() + level.out() - last.update.level()
    )
    stat.range.mean <- reactive(
      round(stat.range.min() + (stat.range.max() - stat.range.min()) / 6)
    )
    stat.range.render <- reactive({
      if(stat.range.min() < stat.range.max()){
        paste0(stat.range.min(), " - ", stat.range.max(), " [", stat.range.mean(), "]")
      }else{
        paste0("[", stat.range.min(), "]")
      }
    })
    output[["range"]] <- renderText(stat.range.render())
    
    # return the reactive AV value
    av
    
  }
  )
}

statRowApp <- function(){
  shinyApp(
    ui = fluidPage(statRowUI("hp_row", "eevee", 1)), 
    server = function(input, output, server){
      statRowServer("hp_row", "eevee", 1, reactive(6), reactive(1), reactive(1), reactive(10))
    }
  )
}