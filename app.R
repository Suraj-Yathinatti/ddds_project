# UI and Server part
# load the packages
# load the dataset -helps both the sub group
# ENable total profit, sales and region dropdown
# TImes series and Pie chart

rm(list=ls()) # Clears memory
graphics.off() # Clears graphs
if (!require("pacman")) install.packages("pacman") #Installs package for package installation
pacman::p_load("shiny", "tidyverse","dygraphs","xts","tidyr","lubridate")

sales_data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", header= TRUE, sep = ",")
sales_data$regions<- as.factor(sales_data$Region)

sales_data$date<-as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))

ui <- fluidPage(
  titlePanel("Sales Data"),
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_region", label = h3("Select the region"), 
              choices = c("all",levels(sales_data$regions))),
      hr(),
      fluidRow(column(3, verbatimTextOutput("value"))),
      fluidRow(column(3, verbatimTextOutput("totalSales"))),
      fluidRow(column(3, verbatimTextOutput("totalProfit")))
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


server <- function(input, output) {
  output$value <- renderPrint({ input$selected_region })
  output$totalSales<-renderPrint({  if(input$selected_region=="all"){
    sum(sales_data$Sales)
  }else{
    sales_data %>% filter(Region==input$selected_region ) %>% select("Sales") %>%  sum()
  }
    })

  
  output$totalProfit<-renderPrint({ if(input$selected_region=="all"){
    sum(sales_data$Profit)
  }else{
    sales_data %>% filter(Region==input$selected_region ) %>% select("Profit") %>% sum()
  } })
  output$distPlot <- renderPlot({
  
    if (input$selected_region=="all"){
    sales_data %>%
      ggplot(aes(date,Sales)) +
      geom_line() 

    }
    else{
    sales_data %>%
      filter(Region==input$selected_region ) %>%
      ggplot(aes(date,Sales)) +
      geom_line() 
   
    }
    
  })
}

shinyApp(ui = ui, server = server)