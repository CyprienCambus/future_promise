library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(promises)
library(future)
library(DT)

plan(multisession)

ui <- fluidPage(
  selectInput("id1", "Choose:", choices = unique(as.character(iris$Species))),
  dataTableOutput("my_data"),
  plotlyOutput("my_plot")
)

server <- function(input, output, session) {
  
  output$my_data <- renderDataTable(
    dat <- datatable(iris %>% filter(Species == input$id1),
                     options = list(
                       paging =TRUE,
                       pageLength =  input$rows 
                     )
    )
  )
  

  
  data_to_plot <- reactiveVal()
  
  observe({
    # see: https://cran.r-project.org/web/packages/promises/vignettes/shiny.html
    # Shiny-specific caveats and limitations
    idVar <- input$id1
    
    # see https://github.com/rstudio/promises/issues/23#issuecomment-386687705
    future_promise({
      Sys.sleep(4)
      ggplot(iris %>% filter(Species == idVar), aes(x=Sepal.Length)) + geom_histogram()
    }) %...>%
      data_to_plot() %...!%  # Assign to data
      (function(e) {
        data(NULL)
        warning(e)
        session$close()
      }) # error handling
    
    # Hide the async operation from Shiny by not having the promise be
    # the last expression.
    NULL
  })
  
  output$my_plot <- renderPlotly({
    req(data_to_plot())
    ggplotly(data_to_plot())
  })
  
}

shinyApp(ui, server)