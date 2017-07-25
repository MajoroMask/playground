library(shiny)
library(ggplot2)
pageWithSidebar(
    headerPanel("Miles Per Gallon"),
    sidebarPanel(
        selectInput(
            "variable", "Variable:", 
            list(Cylinders = "cyl", 
                 Transmission = "am", 
                 Gears = "gear")
        ), 
        checkboxInput("outliers", "Show outliers", F)
    ), 
    mainPanel(
        textOutput("caption"), 
        plotOutput("mpgPlot")
    )
)
