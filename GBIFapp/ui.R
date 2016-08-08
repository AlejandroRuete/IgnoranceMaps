
shinyUI(navbarPage("Ignorance Explorer", id="nav", theme = shinytheme("flatly"), windowTitle="Ignorance Explorer",
  ############# MAP #########
  tabPanel("Spatial Bias",
    div(class="outer",

      tags$head(
        # tags$link(rel="shortcut icon", href="/favicon.ico"),
        # Include our custom CSS
        includeCSS("styles.css"),
        includeScript("gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%"),

      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 70, left = 20, right = "auto", bottom = "auto",
        width = 330, height = "auto",

        h3("Assumptions and Options"),
    
        selectInput(inputId = "res",
                    label = h4("Grid Resolution (in km)"),
                    choices = c("100"="100",
                                "50" = "50",
                                "25" = "25"),
                    width = 200
        ),
        
        checkboxInput(inputId="index",
                      label="Observation index (N/R)",
                      value=TRUE
        ),

        numericInput(inputId ="obs50",
                     label = h4("O",tags$sub("0.5"), "RTG = Class Amphibia", br(),
                                h5("Number of observations required per 25 km grid cell, that is: (res/25)",tags$sup("2"), "x O",tags$sub("0.5"))),
                     value = 1, #(res/(25))^2
                     min = 0.1,
                     max = 1000,
                     step = .1,
                     width = "100%"
        ),
        # Target Species Lists
        tags$hr(),
        numericInput(inputId ="obs50spp", # for each 25km 
                     label = h4("O",tags$sub("0.5"),  em("Rana temporaria"), br(), h5("Number of observations required per 25 km grid cell")),
                     value = 1,
                     min = 0.1,
                     max = 100,
                     step = .1,
                     width = "100%"
        ),
        sliderInput(inputId ="prestol",
                    label = "Max Ignorance for Precense",
                    value = 0.5,
                    min = 0.1,
                    max = 0.9,
                    step = 0.1
        ),
        # tags$hr(), 
        actionButton("goButton", "Apply Changes (and wait)"),
        tags$hr(),
        h3("Plotting options"),
        selectInput(inputId = "layer",
                    label = h5("Layer"),
                    choices = c("RTG Ignorance" = "RTGIgn",
                                "Species Pseudo Absence" = "SppIgn",
                                "Population Size Index (i.e. Observation Odds)" = "PSI", 
                                "Species Presence" = "SppPres"),
                    width = "100%"
        ),
        sliderInput(inputId ="alpha",
                    label = "Transparency",
                    value = 0.8,
                    min = 0.1,
                    max = 0.9,
                    step = 0.1
        )
      ),
      tags$div(id="cite", #class='simpleDiv',
         tags$em('Ignorance Maps'), ' by Alejandro Ruete (SLU, 2016)'
      )
    )
  ),

  tabPanel("Temporal and Grouped Bias", 
    fluidRow(
      column(4,
        selectInput("countries", h4("Countries"), c("Europe"="", structure(CountriesList, names=CountriesListAb)), selected=("Spain"), multiple=TRUE),
        p("Remember that small countries, such as Andorra, are smaller than the grid resolution, therefore no density can be calculated and and error will be displayed."),
        p("For agility reasons using only 100 km grids cells"),
        checkboxInput(inputId="indexD",
                      label="Observation index (N/R)",
                      value=TRUE
        ),
        numericInput(inputId ="obs50D",
                     label = h4("O",tags$sub("0.5"), "RTG = Class Amphibia",br(),h5("Number of observations required per 25 km grid cell")),
                     value = 1, #(res/(25))^2
                     min = 1,
                     max = 1000,
                     step = 1,
                     width = "100%"
        ),
        sliderInput(inputId ="time",
                    label = h4("Years"),
                    value = c(1900,Years[length(Years)]), #c(Years[2],Years[length(Years)]),
                    min = Years[2],
                    max = Years[length(Years)],
                    step = 1
        ),
        checkboxInput(inputId="NAplot",
                      label="Include observations with year = NA?",
                      value=TRUE
        )
        
      ),
      column(8, 
        plotOutput("TempIgn"),
        plotOutput("DensIgn")
      )
    )
  ),
  tabPanel("Read me", includeHTML("data/Description.htm")
           ),

  conditionalPanel("false", icon("crosshair"))
))
