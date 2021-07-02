statUpdateButtonUI <- function(id){
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

statUpdateButtonServer <- function(id, initial.value){
  stopifnot({is.reactive(initial.value)})
  
  moduleServer(id, function(input, output, session){
    stat.changed <- reactive(input$minus | input$set | input$plus)
    stat.value <- reactive({initial.value() + input$plus - input$minus})
    observeEvent(stat.value(), updateActionBttn("set", label = stat.value()))
    list(value = stat.value, changed = stat.changed)
  })
}

statUpdateButtonApp <- function(){
  shinyApp(
    ui = fluidPage(fluidRow(statUpdateButtonUI("hp"))), 
    server = function(input, output, server){
      statUpdateButtonServer("hp", reactive(23))
    }
  )
}
