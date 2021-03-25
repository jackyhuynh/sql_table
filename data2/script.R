library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)

pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

dbListTables(pool)

UserTransaction<- dbReadTable(pool,"transdata")

#Create sql lite df
transdata <- data.frame(
  description = character(),
  type = character(),
  amount = numeric(), 
  category = character(),
  accounts = character(),
  lat= numeric(),
  long=numeric(),
  date = as.Date(character()),
  stringsAsFactors = FALSE)

#Create responses table in sql database
dbWriteTable(pool, "transdata", transdata, overwrite = FALSE, append = TRUE)

#Label mandatory fields
labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <- ".mandatory_star { color: red; }"

ui <- fluidPage(
  shinyjs::useShinyjs(),
  shinyjs::inlineCSS(appCSS),
  
  fluidRow(
    actionButton("edit_button", "Edit", icon("edit")),
  ),
  br(),
  fluidRow(width="100%",
           dataTableOutput("responses_table", width = "100%")
  )
)

server <- function(input,output){
  output$responses_table <- DT::renderDataTable({
    
    
    
    
  })
  
  transdata <- reactive({
    input$submit_edit
  })
  
  # The mandatory field
  fieldsMandatory <- c("name", "sex")
  
  observe({
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
  })
  
  entry_form <- function(button_id){
    
    showModal(
      modalDialog(
        div(id=("entry_form"),
            tags$head(tags$style(".modal-dialog{ width:400px}")), #Modify the width of the dialog
            tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))), #Necessary to show the input options
            fluidPage(
              fluidRow(
                splitLayout(
                  cellWidths = c("250px", "100px"),
                  cellArgs = list(style = "vertical-align: top"),
                  textInput("name", labelMandatory("Name"), placeholder = ""),
                  selectInput("sex", labelMandatory("Sex"), multiple = FALSE, choices = c("", "M", "F"))
                ),
                sliderInput("age", "Age", 0, 100, 1, ticks = TRUE, width = "354px"),
                textAreaInput("comment", "Comment", placeholder = "", height = 100, width = "354px"),
                helpText(labelMandatory(""), paste("Mandatory field.")),
                actionButton(button_id, "Submit")
              ),
              easyClose = TRUE
            )
        )
      )
    )
  }
}
  
shinyApp(ui, server)