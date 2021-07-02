inputUpdateButtonUI <- function(id){
  ns <- NS(id)
  
  tagList(
    shinyWidgets::actionBttn(
      inputId = ns("minus"),
      label = NULL,
      icon = icon("minus"),
      style = "material-circle",
      color = "primary",
      size = "xs"
    ),
    shinyWidgets::actionBttn(
      inputId = ns("set"),
      label = NULL,
      style = "jelly",
      color = "primary",
      size = "sm"
    ),
    shinyWidgets::actionBttn(
      inputId = ns("plus"),
      label = NULL,
      icon = icon("plus"),
      style = "material-circle",
      color = "primary",
      size = "xs"
    )
  )
}

inputUpdateButtonServer <- function(id, initial.value, increment = 1){
  stopifnot({
    !is.reactive(initial.value)
    !is.reactive(increment)
  })
  
  moduleServer(id, function(input, output, session){
    stat.value <- reactive(initial.value + (input$plus - input$minus) * increment)
    observeEvent(stat.value(), updateActionBttn("set", label = stat.value()))
    stat.value
  })
}

inputUpdateButtonApp <- function(){
  shinyApp(
    ui = fluidPage(fluidRow(inputUpdateButtonUI("level"))), 
    server = function(input, output, server){
      inputUpdateButtonServer("level", 5)
    }
  )
}
