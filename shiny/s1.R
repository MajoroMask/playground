library(shiny)
library(ggplot2)

shinyServer(fnction(input, output) {
    formulaText <- reactive(function() {
        paste("mpg ~", input$variable)
    })
    output$caption <- reactive(function() {
        formulaText()
    })
    output$mpgPlot <- reactivePlot(function() {
        if(input$variable == 'am') {
            mpgData <- data.frame(
                mpg = mtcars$mpg, 
                var = factor(mtcars[[input$variable]])
            )
        } else {
            mpgData <- data.frame(
                mpg = mtcars$mpg, 
                var = factor(mtcars[input$variable])
            )
        }
        p <- ggplot(mpgData, aes(var, mpg)) +
            geom_boxplot(outlier.size = ifelse(input$outliers, 2, NA)) +
            labs(x = input$variable)
        print(p)
    })
})