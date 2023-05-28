rm(list=ls()) # Clears memory
graphics.off() # Clears graphs
if (!require("pacman")) install.packages("pacman") #Installs package for package installation
pacman::p_load("shiny", "tidyverse","dygraphs","xts","tidyr","lubridate", "leaflet", "viridis", "reshape2",  "ggplot2", "DT")

# Check if the package is already installed
if (!require("lubridate")) {
  # If not installed, install the package
  install.packages("lubridate")
}

# Load the package
library(lubridate)

sales_data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", header= TRUE, sep = ",")
sales_data$regions <- as.factor(sales_data$Region)
sales_data$date <- as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))

# Aggregate sales data by month
monthly_sales <- sales_data %>%
  mutate(month = format(date, "%Y-%m")) %>%
  group_by(month) %>%
  summarise(avg_sales = mean(Sales))

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
                  choices = c("All", levels(sales_data$regions))),
      hr(),
      fluidRow(
        column(12,
               h3("Total Sales"),
               textOutput("totalSales"),
               class = "bigger-font"
        )
      ),
      hr(),
      fluidRow(
        column(12,
               h3("Total Profit"),
               textOutput("totalProfit"),
               class = "bigger-font"
        )
      )
    ),
    mainPanel(
      plotOutput("distPlot"),
      plotOutput("pieChart"),
      plotOutput("stackedBarPlot"),
      plotOutput("scatterPlot"),
      dataTableOutput("datatable")
    )
  )
)

server <- function(input, output) {
  output$value <- renderPrint({ input$selected_region })
  
  output$totalSales <- renderText({
    if (input$selected_region == "All") {
      total_sales <- sum(sales_data$Sales) / 1000
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "thousand INR")
    } else {
      total_sales <- sales_data %>%
        filter(Region == input$selected_region) %>%
        summarize(total_sales = sum(Sales)) %>%
        pull(total_sales) / 1000
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "thousand INR")
    }
  })
  
  output$totalProfit <- renderText({
    if (input$selected_region == "All") {
      total_profit <- sum(sales_data$Profit) / 1000
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "thousand INR")
    } else {
      total_profit <- sales_data %>%
        filter(Region == input$selected_region) %>%
        summarize(total_profit = sum(Profit)) %>%
        pull(total_profit) / 1000
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "thousand INR")
    }
  })
  
  output$distPlot <- renderPlot({
    if (input$selected_region == "All") {
      monthly_sales %>%
        ggplot(aes(x = as.Date(paste0(month, "-01")), y = avg_sales/1000)) +
        geom_line(color = "#800080") +
        labs(x = "Year", y = "Average Sales (in thousand INR)") +
        scale_y_continuous(labels = function(x) paste0(x, "K"))
    } else {
      region_sales <- sales_data %>%
        filter(Region == input$selected_region) %>%
        mutate(month = format(date, "%Y-%m")) %>%
        group_by(month) %>%
        summarise(avg_sales = mean(Sales))
      
      region_sales %>%
        ggplot(aes(x = as.Date(paste0(month, "-01")), y = avg_sales/1000)) +
        geom_line(color = "#800080") +
        labs(x = "Year", y = "Average Sales (in thousand INR)") +
        scale_y_continuous(labels = function(x) paste0(x, "K"))
    }
  })
  
  output$pieChart <- renderPlot({
    if (input$selected_region == "All") {
      sales_data %>%
        group_by(Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        mutate(total_sales = total_sales / 1000,  # Convert to thousand INR
               percent_sales = total_sales / sum(total_sales) * 100) %>%
        ggplot(aes(x = "", y = total_sales, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(fill = "Category (Sales)") +
        theme_void() +
        theme(legend.position = "right") +
        geom_text(aes(label = paste0(round(percent_sales, 2), "%\n", round(total_sales, 2), "K")), 
                  position = position_stack(vjust = 0.5),
                  vjust = 0.5,
                  fontface = "bold",
                  size = 4) +
        theme(plot.title = element_text(face = "bold"))
    } else {
      sales_data %>%
        filter(Region == input$selected_region) %>%
        group_by(Region, Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        mutate(total_sales = total_sales / 1000,  # Convert to thousand INR
               percent_sales = total_sales / sum(total_sales) * 100) %>%
        ggplot(aes(x = "", y = total_sales, fill = Category)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        labs(fill = "Category (Sales)") +
        theme_void() +
        theme(legend.position = "right") +
        geom_text(aes(label = paste0(round(percent_sales, 2), "%\n", round(total_sales, 2), "K")), 
                  position = position_stack(vjust = 0.5),
                  vjust = 0.5,
                  fontface = "bold",
                  size = 4) +
        theme(plot.title = element_text(face = "bold"))
    }
  })
  
  output$stackedBarPlot <- renderPlot({
    if (input$selected_region == "All") {
      sales_data %>%
        group_by(Region, Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        ggplot(aes(x = Region, y = total_sales, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Region", y = "Total Sales (in thousand INR)", fill = "Category") +
        scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
        scale_y_continuous(labels = function(x) paste0(x / 1000, "K")) +
        theme(legend.position = "right")
    } else {
      sales_data %>%
        filter(Region == input$selected_region) %>%
        group_by(Region, Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        ggplot(aes(x = Region, y = total_sales, fill = Category)) +
        geom_bar(stat = "identity") +
        labs(x = "Region", y = "Total Sales (in thousand INR)", fill = "Category") +
        scale_fill_viridis_d(option = "plasma", begin = 0.1, end = 0.9) +
        scale_y_continuous(labels = function(x) paste0(x / 1000, "K")) +
        theme(legend.position = "right")
    }
  })
  
    output$scatterPlot1 <- renderPlot({
    if (input$selected_region == "All") {
      sales_data %>%
        ggplot(aes(x = Profit, y = City, color = Category)) +
        geom_point() +
        labs(x = "Profit", y = "City")
    } else {
      sales_data %>%
        filter(Region == input$selected_region) %>%
        ggplot(aes(x = Profit, y = City, color = Category)) +
        geom_point() +
        labs(x = "Profit", y = "City")
    }
  })
  
    output$scatterPlot <- renderPlot({
      if (input$selected_region == "All") {
        sales_data %>%
          mutate(Profit_thousand = Profit / 1000) %>%
          ggplot(aes(x = Profit_thousand, y = City, color = Category)) +
          geom_point() +
          labs(x = "Profit (in thousand INR)", y = "City")
      } else {
        sales_data %>%
          filter(Region == input$selected_region) %>%
          mutate(Profit_thousand = Profit / 1000) %>%
          ggplot(aes(x = Profit_thousand, y = City, color = Category)) +
          geom_point() +
          labs(x = "Profit (in thousand INR)", y = "City")
      }
    })
    
    output$datatable <- DT::renderDataTable({
      filtered_data <- if (input$selected_region == "All") {
        sales_data
      } else {
        sales_data %>%
          filter(Region == input$selected_region)
      }
      
      DT::datatable(filtered_data, options = list(scrollY = "300px", paging = FALSE, dom = 't', autoWidth = TRUE))
    })
    
}

shinyApp(ui = ui, server = server)