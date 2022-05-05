#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse) # for now
library(shiny)
library(shinyjs)
library(shinyWidgets)

pointlist <- read_delim("pointlist.csv", delim = ";", 
                        escape_double = FALSE, trim_ws = TRUE)

unit <- NULL
i <- 0

# Define UI for application that draws a histogram
ui <- fluidPage(
    # well, use shinyjs
    useShinyjs(),
    
    # import custom css
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "weird.css")
    ),
    
    # Application title
    titlePanel("Weirdo Wrapper"),
    
    # state printer
    fluidRow(
        actionBttn("state_print", "Print State to console")
    ),
    
    # unit builder
    fluidRow(
        column(3,
               h3("Attributes"),
               
               radioGroupButtons(
                   "speed", label = "Speed", selected = character(0),
                   choices = pointlist$Description[pointlist$Area == "Speed"],
                   size = "sm", direction = "horizontal"
               ),
               radioGroupButtons(
                   "def", label = "Defense", selected = character(0),
                   choices = pointlist$Description[pointlist$Area == "Defense"],
                   size = "sm", direction = "horizontal"
               ),
               radioGroupButtons(
                   "fp", label = "Firepower", selected = character(0),
                   choices = pointlist$Description[pointlist$Area == "Firepower"],
                   size = "sm", direction = "horizontal"
               ),
               radioGroupButtons(
                   "pw", label = "Prowess", selected = character(0),
                   choices = pointlist$Description[pointlist$Area == "Prowess"],
                   size = "sm", direction = "horizontal"
               ),
               radioGroupButtons(
                   "wp", label = "Willpower", selected = character(0),
                   choices = pointlist$Description[pointlist$Area == "Willpower"],
                   size = "sm", direction = "horizontal"
               )
        ),
        
        div(id = "rangwep",
            column(3,
                   h3("Ranged Weapons"),
                   radioGroupButtons(
                       "rw", label = "", selected = character(0),
                       choices = pointlist$Name[pointlist$Area == "Ranged Weapon"],
                       size = "sm", direction = "vertical"
                   )
            )
        ),
        
        column(3,
               h3("Close Combat Weapons"),
               radioGroupButtons(
                   "cc", label = "", selected = character(0),
                   choices = pointlist$Name[pointlist$Area == "Close Combat"],
                   size = "sm", direction = "vertical"
               )
        ),
        
        column(3,
               h3("Equipment"),
               radioGroupButtons(
                   "eq", label = "", selected = character(0),
                   choices = pointlist$Name[pointlist$Area == "Equipment"],
                   size = "sm", direction = "vertical"
               ),
               # "Only Leaders or Cyborg-Warbands may choose more than 1 Equipment. Make model Leader or de-select length(x)-1 items!"
               # textOutput("pts")
               actionBttn("add_unit", "Add to Warband"),
               actionBttn("reset", "Reset Counter")
        )
    ),
    
    hr(),
    
    fluidRow(
        shinyjs::hidden(
            div(id = "unit1", 
                column(
                    4, 
                    withTags({
                        table(
                            tr(
                                th("Name:"), td(textOutput("name")),
                                th("Points:"), td(textOutput("pts"))
                            ),
                            tr(
                                th("Speed"), 
                                th("Def"), 
                                th("Fire"), 
                                th("Prw"), 
                                th("Wil")
                            ),
                            tr(
                                td(textOutput("spd")), 
                                td(textOutput("def")), 
                                td(textOutput("fpw")), 
                                td(textOutput("prw")), 
                                td(textOutput("wil"))
                            ),
                            tr(
                                th("Weapon"), 
                                th("Actions"), 
                                th("Description")
                            ),
                            tr(
                                td(textOutput("rwp")), 
                                td(), 
                                td(textOutput("desc_rwp"))
                            ),
                            tr(
                                td(textOutput("ccw")), 
                                td(), 
                                td(textOutput("desc_ccw"))
                            ),
                            tr(
                                th("Equipment"), th("Description")
                            ),
                            tr(
                                td(textOutput("eqp")), td(textOutput("desc_eqp"))
                            ),
                        )
                    })
                )
            )
        ),
        shinyjs::hidden(div(id = "B", column(4, p("no")))),
        shinyjs::hidden(div(id = "C", column(4, p("no")))),
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # observe({print(input$fp == "None")})
    
    # keep track of generated units
    unit_cards <- c()
    
    observe({
        if (input$fp == "None" || input$fp == "" || is.null(input$fp)) {
            # shinyjs::reset("rw")
            updateRadioGroupButtons(inputId = "rw", 
                                    selected = character(0),
                                    disabled = TRUE)
        } else {
            updateRadioGroupButtons(inputId = "rw", 
                                    disabled = FALSE)
        }
    })
    
    output$pts <- renderText({
        sum(c(
            pointlist$Points[pointlist$Name == input$speed & pointlist$Area == "Speed"],
            pointlist$Points[pointlist$Name == input$def & pointlist$Area == "Defense"],
            pointlist$Points[pointlist$Name == input$fp & pointlist$Area == "Firepower"],
            pointlist$Points[pointlist$Name == input$pw & pointlist$Area == "Prowess"],
            pointlist$Points[pointlist$Name == input$wp & pointlist$Area == "Willpower"],
            pointlist$Points[pointlist$Name == input$rw],
            pointlist$Points[pointlist$Name == input$cc],
            pointlist$Points[pointlist$Name == input$eq]
        ))
    })
    
    observeEvent(input$add_unit, {
        i <<- i+1
        if (i == 1) {
            shinyjs::toggle(id = "unit1", anim = TRUE)
        } else if (i == 2) {
            shinyjs::toggle(id = "B", anim = TRUE)
        } else if (i == 3) {
            shinyjs::toggle(id = "C", anim = TRUE)
        } 
    })
    
    ## prelim outputses // in == out, alright
    observe({
        unit <<- list(
            name  = "nono not yet",
            stats = c(input$speed, input$def, input$fp, input$pw, input$wp),
            gear  = c(input$rw, input$cc, input$eq),
            descriptions = c(pointlist$Description[pointlist$Name == input$rw],
                             pointlist$Description[pointlist$Name == input$cc],
                             pointlist$Description[pointlist$Name == input$eq])
        )
    })
    
    ## stats n stuff output
    output$name <- renderText(unit$name)
    
    output$spd <- renderText(unit$stats[1])
    output$def <- renderText(unit$stats[2])
    output$fpw <- renderText(unit$stats[3])
    output$prw <- renderText(unit$stats[4])
    output$wil <- renderText(unit$stats[5])
    
    output$rwp <- renderText(unit$gear[1])
    output$ccw <- renderText(unit$gear[2])
    output$eqp <- renderText(unit$gear[3])
    
    output$desc_rwp <- renderText(unit$descriptions[1])
    output$desc_ccw <- renderText(unit$descriptions[2])
    output$desc_eqp <- renderText(unit$descriptions[3])
    
    observeEvent(input$reset, {
        i <<- 0
    })
    
    # state printer // debug-ish
    observeEvent(input$state_print, {
        # print(paste("Name:", input$name))
        print(paste("Speed:", input$speed))
        print(paste("Def:", input$def))
        print(paste("Fire:", input$fp))
        print(paste("Prowess:", input$pw))
        print(paste("Will:", input$wp))
        print(paste("Ranged:", input$rw))
        print(paste("Melee:", input$cc))
        print(paste("Equip:", input$eq))
        print(paste("i:", i))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
