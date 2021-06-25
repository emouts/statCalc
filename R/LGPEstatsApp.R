#' @title Stat calculation app for LGPE
#' @description The purpose of the app is to help the runner plan ahead
#' using the information gained from the stat values at each level up.
#' Each stat is considered independent (the calculation does not take into 
#' account the total AVs gained across all stats). At each level when stats
#' were seen by the runner, they can be adjusted in the app to narrow down 
#' the IV-AV combination and stat range projection of each stat. Some
#' other useful information is shown, mainly to help the runner avoid 
#' mistakes with inputs. Note that the app does not prevent invalid states
#' (e.g. a stat value that is impossible), it is up to the user to avoid that.
#' @import shiny
#' @export

LGPEstatsApp <- function(...){
    
    ui <- function(request){
        
        fluidPage(
            
            titlePanel("LGPE Eevee stat calculator"),
            
            # set up the general input row
            fluidRow(
                column(width = 1,
                       numericInput("level_in", "Level", 
                                    value = initial.level, min = 1, max = 100)),
                column(width = 1, selectInput("nature", "Nature", nature.names)),
                column(width = 1),
                column(width = 1,
                       numericInput("happiness", "Happiness modifier", 
                                    value = 1, min = 1, max = 1.1, step = 0.01)),
                column(width = 2),
                column(width = 1,
                       numericInput("level_out", "Level", 
                                    value = initial.level + 5, min = 1, max = 100))
            ),
            
            # set up the 6 stat rows
            statRowUI("hp_row", 1),
            statRowUI("atk_row", 2),
            statRowUI("def_row", 3),
            statRowUI("spatk_row", 4),
            statRowUI("spdef_row", 5),
            statRowUI("speed_row", 6),
            
            # show some extra information on total AVs
            fluidRow(column(2), column(1, textOutput("known_avs"), inline = FALSE)),
            fluidRow(column(2),
                column(1, textOutput("unknown_avs"), inline = FALSE),
                column(2),
                bookmarkButton())
            
        )
        
    }
    
    server <- function(input, output) {
        
        # set up reactives for the main inputs
        level.in  <- reactive(input[["level_in"]])
        nature.id <- reactive(match(input[["nature"]], nature.names))
        happiness <- reactive(input[["happiness"]])
        level.out <- reactive(input[["level_out"]])
        
        # run calculations for each stat
        statRowServer("hp_row", 1, level.in, nature.id, happiness, level.out)
        statRowServer("atk_row", 2, level.in, nature.id, happiness, level.out)
        statRowServer("def_row", 3, level.in, nature.id, happiness, level.out)
        statRowServer("spatk_row", 4, level.in, nature.id, happiness, level.out)
        statRowServer("spdef_row", 5, level.in, nature.id, happiness, level.out)
        statRowServer("speed_row", 6, level.in, nature.id, happiness, level.out)
        
        # calculate total AVs gained and missing
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