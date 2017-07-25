library(plotly)
plot_ly(mtcars, x = ~wt, y = ~mpg) %>%
    slice(which.max(mpg)) %>%
    add_annotations(text = "Good mileage")
