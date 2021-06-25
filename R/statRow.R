#' @title Module for each stat row
#' @description Each stat is considered independent of the other stats (the 
#' calculation does not take into account the total AVs gained across all stats).
#' The stat is calculated from the level, nature and happiness of the pokemon.
#' User updates to the stat value affect the IV-AV possible combinations,
#' which in turn affect the projection of the stat at a later level.
#' @import shiny

statRowUI <- function(id, stat.id){
  ns <- NS(id)
  
  stat.long <- stat.names.long[stat.id]
  
  # set up the layout of the row
  fluidRow(
    column(width = 1, numericInput(
      ns("base"), paste("Base", stat.long), value = base.stats[stat.id], 
      min = base.stats[stat.id], max = base.stats[stat.id])),
    column(width = 1, numericInput(
      ns("iv"), paste(stat.long, "IV"), 
      value = 31, min = 31, max = 31)),
    column(width = 1, numericInput(
      ns("av"), paste(stat.long, "AV"), 
      value = 0, min = 0, max = 0)),
    column(width = 1, numericInput(
      ns("stat"), paste(stat.long, "Stat"), 
      value = statCalcFun(stat = stat.id), min = 1)),
    # this button verifies that the runner saw the expected stat at that level,
    # distinguishing from the runner not seeing the stat at all
    column(width = 1, actionButton(ns("set_stat"), "I saw this", icon("check"))),
    column(width = 1, textOutput(ns("last_update"), inline = TRUE)),
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
      
      # whenever level, nature, or happiness change, update the expected stat
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
      
      # whenever the stat updates, update the AV (if the stat change was due
      # to a change in level, nature, or happiness, this does nothing)
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
      
      # keep track of the last level when the stat was seen by the runner
      last.update.level <- eventReactive({input[["set_stat"]]; input[["av"]]}, level.in())
      observeEvent(last.update.level(), {
        output[["last_update"]] <- renderText(paste("Last updated at level", last.update.level()))
      })
      
      # calculate the stat range at the target level; the calculation takes into
      # account the difference between the target level and last update level
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
      
    }
  )
}