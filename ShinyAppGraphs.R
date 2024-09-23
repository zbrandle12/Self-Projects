library(shiny)
library(ggplot2)
library(DT)


ui = fluidPage(
  titlePanel("Interactive Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Upload CSV", accept = ".csv"),
      checkboxGroupInput("columns", "Select Columns to Plot", choices = NULL),
      radioButtons("plotType", "Select Plot Type", choices = list("Scatter" = "scatter", "Line" = "line", "Bar" = "bar")),
      actionButton("refresh", "Refresh Data"),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotOutput("plot")),
        tabPanel("Summary", dataTableOutput("summaryTable"))
      )
    )
  )
)


server = function(input, output, session) {
  data = reactiveVal(NULL)
  
  
  observeEvent(input$file, {
    req(input$file)
    df = read.csv(input$file$datapath)
    data(df)
    
   
    updateCheckboxGroupInput(session, "columns", choices = names(df))
  })
  
 
  output$summaryTable = renderDataTable({
    req(data())
    summary(data())
  })
  
 
  output$plot = renderPlot({
    req(data(), input$columns)
    df = data()
    
    
    withProgress(message = "Generating plot", value = 0, {
      incProgress(0.5)  
      
     
      p = ggplot(df, aes_string(x = input$columns[1], y = input$columns[2]))
      
      if (input$plotType == "scatter") {
        p = p + geom_point(color = "blue") + theme_minimal() + labs(title = "Scatter Plot")
      } else if (input$plotType == "line") {
        p = p + geom_line(color = "red") + theme_light() + labs(title = "Line Plot")
      } else if (input$plotType == "bar") {
        p = p + geom_bar(stat = "identity", fill = "green") + theme_classic() + labs(title = "Bar Plot")
      }
      
      incProgress(1)  
      
      p  
    })
  })
  
 
  output$downloadPlot = downloadHandler(
    filename = function() {
      paste("plot", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      ggsave(file, plot = last_plot(), device = "png")
    }
  )
}


shinyApp(ui = ui, server = server)
