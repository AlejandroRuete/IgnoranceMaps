## UI ArtData
library(shiny)
library(shinythemes)

shinyUI(fluidPage(id="nav", theme = shinytheme("flatly"), windowTitle="Ignorance Explorer", #theme = "bootstrap.css",
#shinyUI(navbarPage("Ignorance Explorer", id="nav", theme = shinytheme("flatly"), windowTitle="Ignorance Explorer",
  tags$head(tags$link(rel="shortcut icon", href="/favicon.ico"),  includeCSS("styles.css")),
  titlePanel("Ignorance maps for observations accessed via the Swedish LifeWatch"),

  fluidRow(
    column(3,

  wellPanel(
      selectInput(inputId = "dataset",
                  label = h4("Reference Taxonomic Group"),
                  choices = c("Amphibians" = "Amphibia",
                              "Birds" = "Aves",
                              "Butterflies" = "Papilionoidea",
                              "Land Mammals"="Mammals",
                              "Harvestmen" = "Opilions",
                              "Dragonflies" = "Odonata",
                              "Vascular Plants" = "Tracheophyta"),
                  selected = "Amphibia",
                  width = 150
                 ),

      checkboxInput(inputId="index",
                    label="Observation index (N/R)",
                    value=TRUE
      ),

      radioButtons(inputId = "trans",
                  label = "Algorithms",
                  choices = c("Normalization" = 1,
                              "Log-Normalization" = 2,
                              "Half-ignorance" = 3),
                  selected = 3
                  ),
      numericInput(inputId ="obs50",
                   label =  h4("O",tags$sub("0.5")),
                   value = 1,
                   min = .1,
                   max = 100,
                   step = .1,
                   width = 150
                   ),

      selectInput(inputId = "target",
                  label = h4("Target Species"),
                  choices = c("Common"="Common",
                              "Rare" = "Rare"),
                  selected = "Common",
                  width = 150
                 ),
      radioButtons(inputId = "trans2",
                  label = "Algorithms",
                  choices = c("Normalization" = 1,
                              "Log-Normalization" = 2,
                              "Half-ignorance" = 3,
                              "Step" = 4),
                  selected = 3
                  ),
      numericInput(inputId ="obs502",
                   label =  h4("O",tags$sub("0.5")),
                   value = 1,
                   min = .1,
                   max = 100,
                   step = .1,
                   width = 150
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
                   value = 0.6,
                   min = 0.1,
                   max = 0.9,
                   step = 0.1
                   )
  ) #end wellpanel
  ), # end column 2

  column(9,
    tabsetPanel(
        tabPanel("Maps", plotOutput("ObsPlot")),
        tabPanel("Data plots", plotOutput("TransPlot")),
        tabPanel("Read me", includeHTML("data/Description.htm")
                            )# end tab Read me,
      )#end tabset
  )
) #end fluid row
))
