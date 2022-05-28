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

# unit <- c()
# mutants  <- c("Claws & Teeth", "Horrible Claws & Teeth", "Whip/Tail")
# soldiers <- c("Grenade*", "Heavy Armor", "Medkit*")

# Define UI for application that draws a histogram
ui <- fluidPage(
  # set theme
  theme = shinythemes::shinytheme("spacelab"),
  
  # well, use shinyjs
  useShinyjs(),
  
  # import custom css
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "weird.css"),
    tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jQuery.print/1.6.0/jQuery.print.min.js")
  ),
  
  # Application title
  titlePanel("Weirdo Wrapper"),
  
  ### state printer
  fluidRow(
    column(2,
           # actionBttn("state_print", "Print State to console")
           actionButton("print", "Print", onclick = "$('#print_space').print();")
    )
  ),
  
  fluidRow(
    column(6,
           textInput("warband_name",
                     "Name of the Warband",
                     "Weltraum Whackos")
    )
  ),
  
  fluidRow(
    column(4, h3("Choose A Warband Trait")),
    column(8, h3("Warband Strength: ", textOutput("team_points", inline = TRUE)))
  ),
  fluidRow(
    column(8,
           radioGroupButtons(
             "warband_trait", label = "", selected = character(0),
             choices = pointlist$Name[pointlist$Area == "Warband Traits"],
             size = "normal", direction = "horizontal"
           ),
           # tags$b(textOutput("warband_trait")), 
           textOutput("trait_description"))
  ),
  
  # unit builder
  fluidRow(
    column(4, h3(textOutput("current_points"))),
    # column(8, "Points:")
  ),
  fluidRow(
    # Attributes
    column(2,
           textInput("unit_name",
                     "Name of the unit",
                     "Fritz Gooney"),
           materialSwitch("is_leader", "Warband Leader"),
           radioGroupButtons(
             "speed", label = "Speed", selected = character(0),
             choices = pointlist$Description[pointlist$Area == "Speed"],
             size = "sm", direction = "horizontal", justified = TRUE
           ),
           radioGroupButtons(
             "def", label = "Defense", selected = character(0),
             choices = pointlist$Description[pointlist$Area == "Defense"],
             size = "sm", direction = "horizontal", justified = TRUE
           ),
           radioGroupButtons(
             "fp", label = "Firepower", selected = character(0),
             choices = pointlist$Description[pointlist$Area == "Firepower"],
             size = "sm", direction = "horizontal", justified = TRUE
           ),
           radioGroupButtons(
             "pw", label = "Prowess", selected = character(0),
             choices = pointlist$Description[pointlist$Area == "Prowess"],
             size = "sm", direction = "horizontal", justified = TRUE
           ),
           radioGroupButtons(
             "wp", label = "Willpower", selected = character(0),
             choices = pointlist$Description[pointlist$Area == "Willpower"],
             size = "sm", direction = "horizontal", justified = TRUE
           )
    ),
    
    # Gear & Powers
    column(10,
           fluidRow(
             column(8, div(id = "rangwep",
                           radioGroupButtons(
                             "rw", label = "Ranged Weapons", selected = character(0),
                             choices = pointlist$Name[pointlist$Area == "Ranged Weapon"],
                             size = "sm", direction = "horizontal"
                           ))
             ),
             column(2, textOutput("rw_text"))
           ),
           
           hr(),
           
           fluidRow(
             column(8, radioGroupButtons(
               "cc", label = "Close Combat Weapons", selected = character(0),
               choices = pointlist$Name[pointlist$Area == "Close Combat"],
               size = "sm", direction = "horizontal"
             )),
             column(2, textOutput("cc_text"))
           ),
           hr(),
           
           fluidRow(
             column(8, checkboxGroupButtons(
               "eq", label = "Equipment", selected = character(0),
               choices = pointlist$Name[pointlist$Area == "Equipment"],
               size = "sm", direction = "horizontal"#, justified = TRUE
             )),
             column(2, textOutput("eq_text", inline = FALSE))
           ),
           hr(),
           
           fluidRow(
             column(8, checkboxGroupButtons(
               "pp", label = "Psychic Powers", selected = character(0),
               choices = pointlist$Name[pointlist$Area == "Psychic Power"],
               size = "sm", direction = "horizontal", justified = TRUE
             )),
             column(2, htmlOutput("pp_text"))
           )
    )
  ),
  fluidRow(
    # column(1, p("Current Cost:", textOutput("current_points"))),
    column(2, actionButton("add_unit", "Add to Warband", class = "btn-primary")),
    # column(8, p("Remember: Regular units shouldn't cost more than 20 pts."))
  ),
  
  hr(),
  
  fluidRow(id = "print_space",
           column(4, id = "warband_tile", 
                  h3(textOutput("warband_name"), style = "font-family: 'Permanent Marker';")
           ),
           div(id = "unit_area")
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$warband_name      <- renderText({input$warband_name})
  output$warband_trait     <- renderText({input$warband_trait})
  output$trait_description <- renderText({pointlist$Description[input$warband_trait == pointlist$Name]})
  
  # observe({print(input$fp == "None")})
  
  observe({
    if (input$fp == "None" || input$fp == "" || is.null(input$fp)) {
      updateRadioGroupButtons(inputId = "rw", 
                              selected = character(0),
                              disabled = TRUE)
    } else {
      updateRadioGroupButtons(inputId = "rw", 
                              disabled = FALSE)
    }
  })
  
  output$rw_text <- renderText({pointlist$Description[input$rw == pointlist$Name]})
  output$cc_text <- renderText({pointlist$Description[input$cc == pointlist$Name]})
  output$eq_text <- renderText({
    # excessive bracketeering 
    if (length(input$eq) > 1 && (any(input$eq == "") || !is.null(input$eq))) {
      if (!input$is_leader && input$warband_trait != "Cyborgs") {
        "ONLY LEADERS AND CYBORGS MAY CARRY MORE THAN ONE PIECE OF EQUIPMENT"
      } else {
        pointlist$Description[pointlist$Name %in% input$eq]
      }
    } else {
      pointlist$Description[pointlist$Name %in% input$eq]
      
      # paste(name, "-", text)
    }
  })
  
  output$pp_text <- renderText({
    name <- input$pp
    text <- pointlist$Description[pointlist$Name %in% input$pp]
    
    paste(name, "-", text)
  })
  
  output$current_points <- renderText({
    wb_state <- input$warband_trait
    rw <- pointlist$Points[pointlist$Name == input$rw & pointlist$Area == "Ranged Weapon"]
    print(paste("rw:", wb_state != "Heavily Armed"))
    
    rw_cost <- ifelse(wb_state != "Heavily Armed" || is.null(wb_state), rw, rw - 1)
    print(paste("rw_cost:", is.null(rw_cost)))
    
    x <- sum(c(
      na.rm = TRUE,
      pointlist$Points[pointlist$Name == input$speed & pointlist$Area == "Speed"],
      pointlist$Points[pointlist$Name == input$def & pointlist$Area == "Defense"],
      pointlist$Points[pointlist$Name == input$fp & pointlist$Area == "Firepower"],
      pointlist$Points[pointlist$Name == input$pw & pointlist$Area == "Prowess"],
      pointlist$Points[pointlist$Name == input$wp & pointlist$Area == "Willpower"],
      rw_cost,
      pointlist$Points[pointlist$Name == input$cc],
      pointlist$Points[pointlist$Name %in% input$eq],
      pointlist$Points[pointlist$Name %in% input$pp]
    ))
    
    paste0("Create Unit (Cost: ", x, ")")
  })
  
  ## render unit table ----
  # keep track of generated units
  max_points  <- 0
  unit_cards  <- c()
  team_points <- c()
  
  # generate table/card
  observeEvent(input$add_unit, {
    unit <- list(
      name  = input$unit_name,
      speed = input$speed, 
      def = input$def, 
      fp = input$fp, 
      pw = input$pw, 
      wp = input$wp,
      range = input$rw, 
      close = input$cc, 
      equip = input$eq,
      psych = input$pp,
      act_range = pointlist$xx[pointlist$Name == input$rw],
      act_close = pointlist$xx[pointlist$Name == input$cc],
      desc_range = pointlist$Description[pointlist$Name == input$rw],
      desc_close = pointlist$Description[pointlist$Name == input$cc],
      desc_equip = pointlist$Description[pointlist$Name %in% input$eq],
      desc_psych = pointlist$Description[pointlist$Name %in% input$pp],
      points = sum(c(
        na.rm = TRUE,
        pointlist$Points[pointlist$Name == input$speed & pointlist$Area == "Speed"],
        pointlist$Points[pointlist$Name == input$def & pointlist$Area == "Defense"],
        pointlist$Points[pointlist$Name == input$fp & pointlist$Area == "Firepower"],
        pointlist$Points[pointlist$Name == input$pw & pointlist$Area == "Prowess"],
        pointlist$Points[pointlist$Name == input$wp & pointlist$Area == "Willpower"],
        pointlist$Points[pointlist$Name == input$rw],
        pointlist$Points[pointlist$Name == input$cc],
        pointlist$Points[pointlist$Name %in% input$eq],
        pointlist$Points[pointlist$Name %in% input$pp]
      ))
    )
    
    # debug much?
    # print(paste(unit$psych, "-", unit$desc_psych))
    print(unit)
    
    unit_counter <- input$add_unit
    unit_id <- paste0("unit_", unit_counter)
    
    insertUI(
      selector = "#unit_area",
      ui = column(4, tags$div(
        withTags({
          table(
            style = "width: 100%;",
            tr(
              th("Name:"), 
              td(style = "font-size: 16pt;",
                 unit$name, colspan = 2),
              th("Points:"), 
              td(style = "font-size: 16pt;",
                 unit$points)
            ),
            tr(
              th(width="20%", "Speed"), 
              th(width="20%", "Defense"), 
              th(width="20%", "Firepower"), 
              th(width="20%", "Prowess"), 
              th(width="20%", "Willpower")
            ),
            tr(style = "font-size: 24pt;",
               td(unit$speed), 
               td(unit$def), 
               td(unit$fp), 
               td(unit$pw), 
               td(unit$wp)
            ),
            tr(
              th("Weapon"), 
              th("Actions"), 
              th("Description", colspan = 3)
            ),
            if (unit$fp != "None") {
              tr(
                td(unit$range), 
                td(unit$act_range, style="font-size: 16pt;"), 
                td(unit$desc_range, colspan = 3)
              )},
            tr(
              td(unit$close), 
              td(unit$act_close, style="font-size: 16pt;"), 
              td(unit$desc_close, colspan = 3)
            ),
            tr(
              th("Equipment", colspan = 2), 
              th("Description", colspan = 3)
            ),
            tr(
              td(unit$equip[1], colspan = 2), 
              td(unit$desc_equip[1], colspan = 3)
            ),
            if (length(unit$equip) > 1) {
              tr(
                td(unit$equip[2], colspan = 2), 
                td(unit$desc_equip[2], colspan = 3)
              )
            },
            if (!is.null(unit$psych)) {
              tr(
                td(unit$psych[1], colspan = 2),
                td(unit$desc_psych[1], colspan = 3)
              )
            },
            if (!is.null(unit$psych) && !is.na(unit$psych[2])) {
              tr(
                td(unit$psych[2], colspan = 2),
                td(unit$desc_psych[2], colspan = 3)
              )
            },
            if (!is.null(unit$psych) && !is.na(unit$psych[3])) {
              tr(
                td(unit$psych[3], colspan = 2),
                td(unit$desc_psych[3], colspan = 3)
              )
            }
          )
        })
      ),
      id = unit_id
      )
    )
    max_points  <<- ifelse(unit$points > max_points, unit$points, max_points)
    unit_cards  <<- c(unit_id, unit_cards) 
    team_points <<- c(team_points, unit$points)
    
    output$team_points <- renderText({sum(team_points)})
  })
  
  # output$team_points <- renderText({
  #     
  #     unit <- sum(c(
  #         pointlist$Points[pointlist$Name == input$speed & pointlist$Area == "Speed"],
  #         pointlist$Points[pointlist$Name == input$def & pointlist$Area == "Defense"],
  #         pointlist$Points[pointlist$Name == input$fp & pointlist$Area == "Firepower"],
  #         pointlist$Points[pointlist$Name == input$pw & pointlist$Area == "Prowess"],
  #         pointlist$Points[pointlist$Name == input$wp & pointlist$Area == "Willpower"],
  #         pointlist$Points[pointlist$Name == input$rw],
  #         pointlist$Points[pointlist$Name == input$cc],
  #         pointlist$Points[pointlist$Name == input$eq]
  #     ))
  #     
  #     team <<- c(team, unit)
  #     print(unit)
  #     print(team)
  #     
  #     paste0("Warband Strength: ", sum(team), " pts")
  # })
  
  # state printer // debug-ish ----
  observeEvent(input$state_print, {
    print("--------- INPUTS ---------")
    # print(paste("Name:", input$unit_name))
    # print(paste("Speed:", input$speed))
    # print(paste("Def:", input$def))
    # print(paste("Fire:", input$fp))
    # print(paste("Prowess:", input$pw))
    # print(paste("Will:", input$wp))
    # print(paste("Ranged:", input$rw))
    # print(paste("Melee:", input$cc))
    # print(paste("Equip:", input$eq))
    print(paste("Psychic:", input$pp))
    # print("--------- OBJECT ---------")
    # print(unit)
  })
  
  observeEvent(input$warband_trait, {
    print(paste("speed:", pointlist$Points[pointlist$Name == input$speed & pointlist$Area == "Speed"]))
    print(paste("def:", pointlist$Points[pointlist$Name == input$def & pointlist$Area == "Defense"]))
    print(paste("fire:", pointlist$Points[pointlist$Name == input$fp & pointlist$Area == "Firepower"]))
    print(paste("close:", pointlist$Points[pointlist$Name == input$pw & pointlist$Area == "Prowess"]))
    print(paste("will:", pointlist$Points[pointlist$Name == input$wp & pointlist$Area == "Willpower"]))
    print(paste("rweap:", pointlist$Points[pointlist$Name == input$rw & pointlist$Area == "Ranged Weapon"]))
    print(paste("cweap:", pointlist$Points[pointlist$Name == input$cc]))
    print(paste("equip:", pointlist$Points[pointlist$Name %in% input$eq]))
    print(paste("psych:", pointlist$Points[pointlist$Name %in% input$pp]))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
