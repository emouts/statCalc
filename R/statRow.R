#' @import shiny

statRowUI <- function(id, stat.id){
  ns <- NS(id)
  
  stat.short <- stat.names.short[stat.id]
  stat.long <- stat.names.long[stat.id]
  
  fluidRow(
    column(width = 1, numericInput(
      ns("base"), paste("Base", stat.long), 
      value = base.stats[stat.id], min = base.stats[stat.id], max = base.stats[stat.id]
    )),
    column(width = 1, numericInput(
      ns("iv"), paste(stat.long, "IV"), 
      value = 31, min = 31, max = 31)),
    column(width = 1, numericInput(
      ns("av"), paste(stat.long, "AV"), 
      value = 0, min = 0, max = 0)),
    column(width = 1, numericInput(
      ns("stat"), paste(stat.long, "Stat"), 
      value = statCalcFun(stat = stat.id), min = 1)),
    column(width = 1),
    column(width = 2, textOutput(ns("range"), inline = TRUE))
  )
}

statRowServer <- function(id, stat.id, level.in, nature.id, happiness, level.out){
  stopifnot({
    !is.reactive(stat.id)
    is.reactive(level.in)
    is.reactive(nature.id)
    is.reactive(happiness)
    is.reactive(level.out)
  })
  
  moduleServer(
    id,
    function(input, output, session){
      
      stat.short <- stat.names.short[stat.id]
      
      observeEvent({level.in(); nature.id(); happiness()}, {
        freezeReactiveValue(input, "stat")
        updateNumericInput(
          inputId = "stat",
          value = statCalcFun(
            stat = stat.id,
            iv = input[["iv"]],
            av = input[["av"]],
            level = level.in(),
            nature = nature.id(),
            happiness = happiness()
          )
        )
        
      }, ignoreInit = TRUE)
      
      observeEvent(input[["stat"]], {
        new.av.value <- 
          input[["av"]] +
          input[["stat"]] - 
          statCalcFun(
            stat = stat.id,
            iv = input[["iv"]],
            av = input[["av"]],
            level = level.in(),
            nature = nature.id(),
            happiness = happiness()
          )
        if(new.av.value != input[["av"]]){
          freezeReactiveValue(input, "av")
          updateNumericInput(
            inputId = "av",
            value = new.av.value, 
            min = new.av.value, 
            max = new.av.value
          )
        }
      }, ignoreInit = TRUE)
      
      stat.range.min <- reactive(
        statCalcFun(
          stat = stat.id,
          iv = input[["iv"]],
          av = input[["av"]],
          level = level.out(),
          nature = nature.id(),
          happiness = happiness()
        )
      )
      stat.range.max <- reactive(
        stat.range.min() +
          level.out() - level.in()
      )
      stat.range.mean <- reactive(
        round(stat.range.min() + (stat.range.max() - stat.range.min()) / 6)
      )
      stat.range.render <- reactive(
        if(stat.range.min() < stat.range.max()){
          paste0(stat.range.min(), " - ", stat.range.max(), " [", stat.range.mean(), "]")
        }else{
          paste0("[", stat.range.min(), "]")
        }
      )
      output[["range"]] <- renderText(stat.range.render())
                                      
    }
      )
}