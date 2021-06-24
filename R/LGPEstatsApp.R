#' @import shiny
#' @export

LGPEstatsApp <- function(...){
    
    ui <- function(request){
        
        fluidPage(
            
            titlePanel("LGPE Eevee stat calculator"),
            
            fluidRow(
                column(width = 1,
                       numericInput("level_in", "Level", 
                                    value = initial.level, min = 1, max = 100)),
                column(width = 1, selectInput("nature", "Nature", nature.names)),
                column(width = 1),
                column(width = 1,
                       numericInput("happiness", "Happiness modifier", 
                                    value = 1, min = 1, max = 1.1, step = 0.01)),
                column(width = 1),
                column(width = 1,
                       numericInput("level_out", "Level", 
                                    value = initial.level + 5, min = 1, max = 100))
            ),
            
            statRowUI("hp_row", 1),
            statRowUI("atk_row", 2),
            statRowUI("def_row", 3),
            statRowUI("spatk_row", 4),
            statRowUI("spdef_row", 5),
            statRowUI("speed_row", 6),
            
            fluidRow(
                column(2),
                column(1, textOutput("known_avs"), inline = FALSE)
            ),
            fluidRow(
                column(2),
                column(1, textOutput("unknown_avs"), inline = FALSE),
                column(2),
                bookmarkButton()
            )
            
        )
        
    }
    
    server <- function(input, output) {
        
        level.in  <- reactive(input[["level_in"]])
        nature.id <- reactive(match(input[["nature"]], nature.names))
        happiness <- reactive(input[["happiness"]])
        level.out <- reactive(input[["level_out"]])
        
        statRowServer("hp_row", 1, level.in, nature.id, happiness, level.out)
        statRowServer("atk_row", 2, level.in, nature.id, happiness, level.out)
        statRowServer("def_row", 3, level.in, nature.id, happiness, level.out)
        statRowServer("spatk_row", 4, level.in, nature.id, happiness, level.out)
        statRowServer("spdef_row", 5, level.in, nature.id, happiness, level.out)
        statRowServer("speed_row", 6, level.in, nature.id, happiness, level.out)
        
        known.avs <- reactive(sum(
            input[["hp_row-av"]], input[["atk_row-av"]], 
            input[["def_row-av"]], input[["spatk_row-av"]], 
            input[["spdef_row-av"]], input[["speed_row-av"]]
        ))
        unknown.avs <- reactive(
            input[["level_in"]] - initial.level - known.avs()
        )
        output[["known_avs"]] <- renderText(paste("Known AVs:", known.avs()))
        output[["unknown_avs"]] <- renderText(paste("Missing AVs:", unknown.avs()))
        
    }
    
    shinyApp(ui = ui, server = server, enableBookmarking = "url")
    
}