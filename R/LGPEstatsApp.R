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
#' @param ... arguments to be passed to other methods
#' @import shiny
#' @export

LGPEstatsApp <- function(...){
    
    ui <- function(request){
        
        fluidPage(
            
            shinyWidgets::setBackgroundColor("ghostwhite"),
            
            titlePanel("LGPE stat calculator"),
            
            tabsetPanel(tabPanel(
                "Eevee", 
                
                # set up the general input row and names
                fluidRow(
                    column(width = 1, tags$strong("Initial level")),
                    column(width = 1, tags$strong("Level")),
                    column(width = 2, tags$strong("Nature")),
                    column(width = 1, tags$strong("Happiness modifier")),
                    column(width = 1, offset = 1, tags$strong("Target level"))
                ),
                
                fluidRow(
                    column(width = 1, customStaticButtonConstant(
                        label = params.list[["eevee"]]$initial.level)),
                    column(width = 1, inputUpdateButtonUI("level_in")),
                    column(width = 2, shinyWidgets::pickerInput(
                        "nature", NULL, nature.named.list, 
                        options = list(style = "bttn-material-flat bttn-primary")
                    )),
                    column(width = 1, inputUpdateButtonUI("happiness")),
                    column(width = 1, offset = 1, inputUpdateButtonUI("level_out"))
                ),
                
                fluidRow(
                    column(width = 1, offset = 1, tags$strong("Base")),
                    column(width = 1, tags$strong("IV")),
                    column(width = 1, tags$strong("AV")),
                    column(width = 1, tags$strong("Stat")),
                    column(width = 1, tags$strong("Last updated")),
                    column(width = 1, tags$strong("Range [expected]"))
                ),
                
                # set up the 6 stat rows
                statRowUI("hp_row", "eevee", 1),
                statRowUI("atk_row", "eevee", 2),
                statRowUI("def_row", "eevee", 3),
                statRowUI("spatk_row", "eevee", 4),
                statRowUI("spdef_row", "eevee", 5),
                statRowUI("speed_row", "eevee", 6),
                
                # show some extra information on total AVs
                fluidRow(column(1, align = "left", offset = 3, textOutput(
                    "known_avs", container = customStaticButtonOutputLight))),
                fluidRow(column(1, align = "left", offset = 3, textOutput(
                    "unknown_avs", container = customStaticButtonOutputLight)),
                         column(2),
                    column(1, bookmarkButton()))
                
            ))
        )
        
    }
    
    server <- function(input, output, session) {
        
        # set up reactives for the main inputs
        level.in  <- inputUpdateButtonServer("level_in", params.list[["eevee"]]$initial.level)
        nature.id <- reactive(match(input[["nature"]], nature.names))
        happiness <- inputUpdateButtonServer("happiness", 1, 0.01)
        level.out <- inputUpdateButtonServer("level_out", params.list[["eevee"]]$initial.level + 5)
        
        # run calculations for each stat
        hp.av <- statRowServer("hp_row", "eevee", 1, level.in, nature.id, happiness, level.out)
        atk.av <- statRowServer("atk_row", "eevee", 2, level.in, nature.id, happiness, level.out)
        def.av <- statRowServer("def_row", "eevee", 3, level.in, nature.id, happiness, level.out)
        spatk.av <- statRowServer("spatk_row", "eevee", 4, level.in, nature.id, happiness, level.out)
        spdef.av <- statRowServer("spdef_row", "eevee", 5, level.in, nature.id, happiness, level.out)
        speed.av <- statRowServer("speed_row", "eevee", 6, level.in, nature.id, happiness, level.out)
        
        # calculate total AVs gained and missing
        known.avs <- reactive(sum(hp.av(), atk.av(), def.av(), spatk.av(), spdef.av(), speed.av()))
        unknown.avs <- reactive(level.in() - params.list[["eevee"]]$initial.level - known.avs())
        output[["known_avs"]] <- renderText(paste("Known:", known.avs()))
        output[["unknown_avs"]] <- renderText(paste("Missing:", unknown.avs()))
        
    }
    
    shinyApp(ui = ui, server = server, enableBookmarking = "url")
    
}