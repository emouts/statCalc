html_dependency_shinyWidgets <- function() {
  htmltools::htmlDependency(
    name = "shinyWidgets",
    version = utils::packageVersion("shinyWidgets"),
    src = c(href = "shinyWidgets", file = "assets"),
    package = "shinyWidgets",
    script = "shinyWidgets-bindings.min.js",
    stylesheet = "shinyWidgets.min.css",
    all_files = FALSE
  )
}

attachShinyWidgetsDep <- function(tag, widget = "bttn") {
  dep <- html_dependency_shinyWidgets()
  if (widget == "bttn") {
    dep <- list(
      dep,
      shinyWidgets::html_dependency_bttn()
    )
  }
  htmltools::attachDependencies(tag, dep, append = TRUE)
}

customStaticButton <- function(
  ..., 
  label = NULL,
  icon = NULL,
  style = "material-circle", 
  color = "primary", 
  size = "sm", 
  block = FALSE,
  no_outline = TRUE
){
  tagBttn <- tags$button(
    ...,
    type = "button",
    class = "action-button bttn",
    class = paste0("bttn-", style), 
    class = paste0("bttn-", size), 
    class = paste0("bttn-", color), 
    list(icon, label), 
    class = if (block) "bttn-block",
    class = if (no_outline) "bttn-no-outline"
  )
  attachShinyWidgetsDep(tagBttn, "bttn")
}

customStaticButtonConstant <- function(label, ...){
  tagBttn <- customStaticButton(label = label,
                                style = "bordered",
                                color = "primary",
                                ...)
  attachShinyWidgetsDep(tagBttn, "bttn")
}

customStaticButtonOutput <- function(...){
  tagBttn <- customStaticButton(...,
                                style = "material-flat",
                                color = "success")
  attachShinyWidgetsDep(tagBttn, "bttn")
}

customStaticButtonOutputLight <- function(...){
  tagBttn <- customStaticButton(...,
                                style = "bordered",
                                color = "success")
  attachShinyWidgetsDep(tagBttn, "bttn")
}

customStaticButtonApp <- function(){
  shinyApp(
    ui = fluidPage(fluidRow(
      column(1, textOutput("general", container = customStaticButton)),
      column(1, customStaticButtonConstant("A constant")),
      column(1, textOutput("output", container = customStaticButtonOutput)),
      column(1, textOutput("output_light", container = customStaticButtonOutputLight))
    )),
    server = function(input, output, server){
      output$general <- renderText("Value")
      output$output <- renderText("An output")
      output$output_light <- renderText("A less important output")
    }
  )
}