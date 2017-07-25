library(shiny)
library(ggplot2)
shinyServer(function(input, output) {
    
    output$caption <- renderText(
        temp <- paste("mpg ~", input$variable)
    )
    output$mpgPlot <- renderPlot({
        mpgData <- data.frame(
            mpg = mtcars$mpg, 
            var = factor(mtcars[[input$variable]])
        )
        ggplot(mpgData, aes(var, mpg)) +
            geom_boxplot(outlier.size = ifelse(input$outliers, 2, NA)) +
            xlab(input$variable)
    })
})