## UI ArtData
library(shiny)

shinyUI(fluidPage(#theme = "bootstrap.css",

  titlePanel("Ignorance maps for observations accessed via the Swedish LifeWatch"),

  fluidRow(
    column(2,

  wellPanel(
      strong("Reference Taxonomic Group"),
      selectInput(inputId = "dataset",
                  label = "",
                  choices = c("Amphibians" = "Amphibia",
                              "Birds" = "Aves",
                              "Butterflies" = "Papilionoidea",
                              "Land Mammals"="Mammals",
                              "Harvestmen" = "Opilions",
                              "Dragonflies" = "Odonata",
                              "Vascular Plants" = "Tracheophyta"),
                  selected = "Amphibia"
                 ),

      checkboxInput(inputId="index",
                    label="Observation index (N/R)",
                    value=FALSE
      ),

      radioButtons(inputId = "trans",
                  label = "Algorithms",
                  choices = c("Normalized" = 1,
                              "Log-Normalized" = 2,
                              "Inversed" = 3),
                  selected = 1,
                  ),
      numericInput(inputId ="obs50",
                   label = "O[0.5]",
                   value = 1,
                   min = .1,
                   max = 100,
                   step = .1
                   )#,
  ),
  wellPanel(
      strong("Target Species"),
      selectInput(inputId = "target",
                  label = "",
                  choices = c("Common"="Common",
                              "Rare" = "Rare"),
                  selected = "Common"
                 ),
      radioButtons(inputId = "trans2",
                  label = "Algorithms",
                  choices = c("Normalized" = 1,
                              "Log-Normalized" = 2,
                              "Inversed" = 3,
                              "Step" = 4),
                  selected = 1,
                  ),
      numericInput(inputId ="obs502",
                   label = "O[0.5]",
                   value = 1,
                   min = .1,
                   max = 100,
                   step = .1
                   )#,
      ), #end wellPanel

  wellPanel(
      strong("Visualization options"),
      sliderInput(inputId ="minAbs",
                   label = "Minimum Ps.absence certainty",
                   value = 0.8,
                   min = 0,
                   max = 1,
                   step = 0.1
                   ),

      sliderInput(inputId ="minPres",
                   label = "Minimum Presence certainty",
                   value = 0.8,
                   min = 0,
                   max = 1,
                   step = 0.1
                   ),

      sliderInput(inputId ="alpha",
                   label = "Transparency",
                   value = 0.3,
                   min = 0.1,
                   max = 0.9,
                   step = 0.1
                   )
  ) #end wellpanel
  ), # end column 2

  column(10,
    tabsetPanel(
        tabPanel("Maps", plotOutput("ObsPlot")),
        tabPanel("Data plots", plotOutput("TransPlot")),
        tabPanel("Read me", includeHTML("data/Description.htm")
                            )# end tab Read me,
      )#end tabset
  )
) #end fluid row
))
