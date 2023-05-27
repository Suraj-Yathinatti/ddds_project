# UI and Server part
# load the packages
# load the dataset -helps both the sub group
# ENable total profit, sales and region dropdown
# TImes series and Pie chart

rm(list=ls()) # Clears memory
graphics.off() # Clears graphs
if (!require("pacman")) install.packages("pacman") #Installs package for package installation
pacman::p_load("shiny", "tidyverse","dygraphs","xts","tidyr","lubridate", "leaflet", "viridis")

sales_data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", header= TRUE, sep = ",")
sales_data$regions<- as.factor(sales_data$Region)
sales_data$date<-as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))


ui <- fluidPage(
    titlePanel("Retail Analysis"),
    sidebarLayout(
        sidebarPanel(
            selectInput("selected_region", label = h3("Select the region"), 
                        choices = c("All",levels(sales_data$regions))),
            hr(),
            #fluidRow(column(3, verbatimTextOutput("value"))),
            #fluidRow(column(3, verbatimTextOutput("totalSales"))),
            #fluidRow(column(3, verbatimTextOutput("totalProfit")))
            fluidRow(
                column(12,
                       h3("Total Sales"),
                       textOutput("totalSales")
                )),hr(),
            fluidRow(column(12,
                            h3("Total Profit"),
                            textOutput("totalProfit"))
        )),
        mainPanel(
            plotOutput("distPlot"),
            leafletOutput("geomap")
        )
    )
)


server <- function(input, output) {
    output$value <- renderPrint({ input$selected_region })
    
    output$totalSales<-renderText({  
        if(input$selected_region=="All"){
            total_sales <- sum(sales_data$Sales)
            total_sales
        }else{
            total_sales <- sales_data %>% 
                filter(Region == input$selected_region) %>% 
                select("Sales") %>%  
                sum()
            total_sales
        }
    })
    
    
    output$totalProfit <- renderText({
        if(input$selected_region=="All"){
            total_profit <- sum(sales_data$Profit)
            total_profit
        }else{
            total_profit <- sales_data %>% 
                filter(Region == input$selected_region) %>% 
                select("Profit") %>% 
                sum()
            total_profit
        } 
    })
    
    output$distPlot <- renderPlot({
        
        if (input$selected_region=="All"){
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