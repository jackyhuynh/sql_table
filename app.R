library(shiny)
library(DT)
library(RSQLite)
library(pool)
library(shinyjs)
library(uuid)
library(dplyr)

#-------------------------------------------------------------------------------
# MySQL Server preparation

drv <- dbDriver("MySQL")
mydb <-
  dbConnect(
    drv,
    user = 'root',
    password = 'Myskhongbiet88',
    dbname = 'credit_card_analysis',
    host = 'localhost'
  )

# Get the data from the database
rs <- dbSendQuery(mydb, "SELECT * FROM transdata")
# to return all the row, return n=-1
transdata <- dbFetch(rs, n=-1)
dbClearResult(rs)


#-------------------------------------------------------------------------------
# SQLITE PREPARATION

#Create sql lite database
pool <- dbPool(RSQLite::SQLite(), dbname = "db.sqlite")

#Create sql lite df
responses_df <- data.frame(row_id = character(),
                           date = as.Date(character()),
                           description = character(),
                           amount = numeric(),
                           type = character(),
                           category = character(),
                           subCategory = character(),
                           accounts = character(),
                           lat = numeric(),
                           long = numeric(),
                           stringsAsFactors = FALSE)

#Create responses table in sql database
dbWriteTable(pool, "responses_df", responses_df, overwrite = FALSE, append = TRUE)

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
  tabsetPanel(
    tabPanel("SQLite",
      br(),
      fluidRow(
        actionButton("edit_button", "Edit Category", icon("edit")),
      ),
      br(),
      fluidRow(width="100%",
               dataTableOutput("responses_table", width = "100%")
      )
    ),
    tabPanel("MySQL",br(),
      fluidRow(
        actionButton("edit_button2","Edit Category",icon("edit"))),br(),
      fluidRow(width="100%", 
               DT::dataTableOutput("response_table_MySQL", width = "100%"))
    )
  ) # End Outer Tab Set
)

# Server
server <- function(input, output, session) {

#load responses_df and make reactive to inputs  
responses_df <- reactive({
  input$submit_edit  #make reactive to
  dbReadTable(pool, "responses_df")
})  

sub_Choice <- c("", "Credit Card Payment","Credit Card Payment", "Sell", "Internet")

#Form for data entry
entry_form <- function(button_id){
  showModal(
    modalDialog(
      div(id=("entry_form"),
          tags$head(tags$style(".modal-dialog{ width:400px}")),
          tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible}"))),
          fluidPage(
            fluidRow(
              textInput("description", labelMandatory("description"), placeholder = ""),
              selectInput("subCategory", labelMandatory("subCategory"), multiple = FALSE, choices = sub_Choice),
              actionButton(button_id, "Submit")
            ),
            easyClose = TRUE
          ))))
}


#save form data into data_frame format
formData <- reactive({
  formData <- data.frame(row_id = UUIDgenerate(),
                         subCategory = input$subCategory,
                         date = as.character(format(Sys.Date(), format="%d-%m-%Y")),
                         stringsAsFactors = FALSE)
  return(formData)
})


observeEvent(input$submit, priority = 20,{
  appendData(formData())
  shinyjs::reset("entry_form")
  removeModal()
})


#edit data
observeEvent(input$edit_button, priority = 20,{
  
  SQL_df <- dbReadTable(pool, "responses_df")
  
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
    updateTextInput(session, "description", value = SQL_df[input$responses_table_rows_selected, "description"])
    updateTextAreaInput(session, "subCategory", value = SQL_df[input$responses_table_rows_selected, "subCategory"])
  }
  
})

observeEvent(input$submit_edit, priority = 20, {
  SQL_df <- dbReadTable(pool, "responses_df")
  row_selection <- SQL_df[input$responses_table_row_last_clicked, "row_id"] 
  dbExecute(pool, sprintf('UPDATE "responses_df" SET "subCategory" = ? WHERE "row_id" = ("%s")', row_selection), 
            param = list(input$subCategory))
  removeModal()
})

# SQLite database out put
output$responses_table <- DT::renderDataTable({
  table <- responses_df() %>% select(-row_id) 
  names(table) <- c("Date", "Description", 
                    "Amount", "Type","Category", "subCategory","Accounts", "Lat", "Long"  )
  table <- datatable(table, options = list(autoWidth = TRUE))
})

# MySQL server out put
output$response_table_MySQL <- DT::renderDataTable({
  datatable(transdata[, c("date", "description", "amount", "type", "category", "accounts")], 
            options = list(autoWidth = TRUE))
})
}

# Run the application 
shinyApp(ui = ui, server = server)

