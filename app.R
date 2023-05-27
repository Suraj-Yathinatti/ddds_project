# UI and Server part
# load the packages
# load the dataset -helps both the sub group
# ENable total profit, sales and region dropdown
# TImes series and Pie chart

rm(list=ls()) # Clears memory
graphics.off() # Clears graphs
if (!require("pacman")) install.packages("pacman") #Installs package for package installation
pacman::p_load("shiny", "tidyverse","dygraphs","xts","tidyr","lubridate", "leaflet", "viridis", "reshape2",  "ggplot2", "DT")

sales_data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", header= TRUE, sep = ",")
sales_data$regions<- as.factor(sales_data$Region)
sales_data$date<-as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))


ui <- fluidPage(
    tags$head(
        tags$style(
            HTML("
           .bigger-font {
             font-size: 20px;
           }
           ")
        )
    ),
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
                       textOutput("totalSales"),
                       class = "bigger-font",
                )),hr(),
            fluidRow(column(12,
                            h3("Total Profit"),
                            textOutput("totalProfit"),
                            class = "bigger-font")
            )),
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("scatterPlot"),
            plotOutput("stackedBarPlot"),
            plotOutput("boxPlot"),
            dataTableOutput("datatable")
        )
    )
)


server <- function(input, output) {
    output$value <- renderPrint({ input$selected_region })
    
    output$totalSales<-renderText({  
        if(input$selected_region=="All"){
            total_sales <- sum(sales_data$Sales)
            paste(total_sales, "/- INR")
        }else{
            total_sales <- sales_data %>% 
                filter(Region == input$selected_region) %>% 
                select("Sales") %>%   
                sum()
            paste(total_sales, "/- INR")
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
                geom_line(color = "#800080") 
            
        }
        else{
            sales_data %>%
                filter(Region==input$selected_region ) %>%
                ggplot(aes(date,Sales)) +
                geom_line(color = "#800080") 
        }
    })
    
    output$scatterPlot <- renderPlot({
        if (input$selected_region == "All") {
            sales_data %>%
                ggplot(aes(Sales, Profit)) +
                geom_point(color = "#800080") +
                labs(x = "Sales", y = "Profit")
        } else {
            sales_data %>%
                filter(Region == input$selected_region) %>%
                ggplot(aes(Sales, Profit)) +
                geom_point(color = "#800080") +
                labs(x = "Sales", y = "Profit")
        }
    })
    
    output$stackedBarPlot <- renderPlot({
        if (input$selected_region == "All") {
            sales_data %>%
                group_by(Region, Category) %>%
                summarise(total_sales = sum(Sales)) %>%
                ggplot(aes(x = Region, y = total_sales, fill = Category)) +
                geom_bar(stat = "identity") +
                labs(x = "Region", y = "Total Sales", fill = "Category") +
                scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
                theme(legend.position = "right")
        } else {
            sales_data %>%
                filter(Region == input$selected_region) %>%
                group_by(Region, Category) %>%
                summarise(total_sales = sum(Sales)) %>%
                ggplot(aes(x = Region, y = total_sales, fill = Category)) +
                geom_bar(stat = "identity") +
                labs(x = "Region", y = "Total Sales", fill = "Category") +
                scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
                theme(legend.position = "right")
        }
    })
    
    output$boxPlot <- renderPlot({
        if (input$selected_region == "All") {
            sales_data %>%
                ggplot(aes(Category, Sales)) +
                geom_boxplot() +
                labs(x = "Category", y = "Sales") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        } else {
            sales_data %>%
                filter(Region == input$selected_region) %>%
                ggplot(aes(Category, Sales)) +
                geom_boxplot() +
                labs(x = "Category", y = "Sales") +
                theme(axis.text.x = element_text(angle = 45, hjust = 1))
        }
    })
    
    output$datatable <- DT::renderDataTable({
        DT::datatable(sales_data, options = list(scrollY = "300px", paging = FALSE, dom = 't', autoWidth = TRUE))
    })
}

shinyApp(ui = ui, server = server)