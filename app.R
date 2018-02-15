#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

library(data.table)

ui <- fluidPage(

  titlePanel("Multiple file uploads"),

  sidebarLayout(

    sidebarPanel(

      fileInput("csvs",

                label="Upload CSVs here",

                multiple = TRUE)

    ),

    mainPanel(

      textOutput("count")

    )

  )

)



server <- function(input, output) {

  mycsvs<-reactive({

    rbindlist(lapply(input$csvs$datapath, fread),

              use.names = TRUE, fill = TRUE)

  })

  output$count <- renderText(nrow(mycsvs()))

}



shinyApp(ui = ui, server = server)

