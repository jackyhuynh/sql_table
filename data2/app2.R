library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)

#Create sql lite database
pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

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

# ui
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


# Server
server <- function(input, output, session) {
    
    #load transdata and make reactive to inputs  
    transdata <- reactive({
        
        #make reactive to
        input$submit
        input$submit_edit
        
        dbReadTable(pool, "transdata")
        
    })  
    
    #List of mandatory fields for submission
    fieldsMandatory <- c("name", "sex")
    
    #define which input fields are mandatory 
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
    
    #Form for data entry
    entry_form <- function(button_id){
        
        showModal(
            modalDialog(
                div(id=("entry_form"),
                    tags$head(tags$style(".modal-dialog{ width:400px}")),
                    tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
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
    
    #
    fieldsAll <- c("name", "sex", "age", "comment")
    
    #save form data into data_frame format
    formData <- reactive({
        
        formData <- data.frame(row_id = UUIDgenerate(),
                               name = input$name,
                               sex = input$sex,
                               age = input$age, 
                               comment = input$comment,
                               date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
                               stringsAsFactors = FALSE)
        return(formData)
        
    })
    
    #edit data
    observeEvent(input$edit_button, priority = 20,{
        
        SQL_df <- dbReadTable(pool, "transdata")
        
        showModal(
            if(length(input$responses_table_rows_selected) > 1 ){
                modalDialog(
                    title = "Warning",
                    paste("Please select only one row." ),easyClose = TRUE)
            } else if(length(input$responses_table_rows_selected) < 1){
                modalDialog(
                    title = "Warning",
                    paste("Please select a row." ),easyClose = TRUE)
            })  
        
        if(length(input$responses_table_rows_selected) == 1 ){
            
            entry_form("submit_edit")
            
            updateTextInput(session, "name", value = SQL_df[input$responses_table_rows_selected, "name"])
            updateSelectInput(session, "sex", selected = SQL_df[input$responses_table_rows_selected, "sex"])
            updateSliderInput(session, "age", value = SQL_df[input$responses_table_rows_selected, "age"])
            updateTextAreaInput(session, "comment", value = SQL_df[input$responses_table_rows_selected, "comment"])
            
        }
        
    })
    
    observeEvent(input$submit_edit, priority = 20, {
        
        SQL_df <- dbReadTable(pool, "transdata")
        row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"] 
        dbExecute(pool, sprintf('UPDATE "transdata" SET "name" = ?, "sex" = ?, "age" = ?,
                          "comment" = ? WHERE "row_id" = ("%s")', row_selection), 
                  param = list(input$name,
                               input$sex,
                               input$age,
                               input$comment))
        removeModal()
        
    })
    
    
    output$responses_table <- DT::renderDataTable({
        
        table <- transdata() %>% select() 
        names(table) <- c("Date", "Name", "Sex", "Age", "Comment")
        table <- datatable(table, 
                           rownames = FALSE,
                           options = list(searching = FALSE, lengthChange = FALSE)
        )
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

