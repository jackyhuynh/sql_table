library(shiny)
library(tidyverse)
library(DT)


# Define UI for application that draws a histogram
ui <- fluidPage(
    # R Shiny UI, Add Button
    div(
        class = "container",
        div(
            style = "margin-top: 50px;",
            shiny::actionButton(
                inputId = "add_car",
                label = "Add Row",
                icon = shiny::icon("plus"),
                class = "btn-success"
            )
        )
    )

)

# Define server logic required to draw a histogram
server <- function(input, output) {

   
}

# Run the application 
shinyApp(ui = ui, server = server)
