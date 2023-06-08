rm(list = ls()) # Clears memory
graphics.off() # Clears graphs

if (!require("pacman")) install.packages("pacman") # Installs package for package installation
pacman::p_load("shiny", "tidyverse", "dygraphs", "xts", "tidyr", "lubridate", "leaflet", "viridis", "reshape2", "ggplot2", "DT")

# Check if the package is already installed
if (!require("lubridate")) {
  # If not installed, install the package
  install.packages("lubridate")
}

sales_data <- read.csv("Supermart Grocery Sales - Retail Analytics Dataset.csv", header = TRUE, sep = ",")
sales_data$regions <- as.factor(sales_data$Region)
sales_data$date <- as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))


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
  fluidRow(
    column(4,
           selectInput("selected_region", label = h3("Select the region"),
                       choices = c("All", levels(sales_data$regions))),
           selectInput("selected_category", label = h3("Select the category"),
                       choices = c("All", unique(sales_data$Category))), # Use unique() to get unique category values
           dateRangeInput("date_range", label = h3("Select Date Range"), start = min(sales_data$date), end = max(sales_data$date))
    ),
    column(8,
           fluidRow(
             column(4,
                    h3("Total Sales"),
                    textOutput("totalSales"),
                    class = "bigger-font"
             ),
             column(4,
                    h3("Total Profit"),
                    textOutput("totalProfit"),
                    class = "bigger-font"
             ),
             column(4,
                    h3("Selected Date Range"),
                    textOutput("selectedDateRange"),
                    class = "bigger-font"
             )
           ),
           hr(),
           selectInput("selected_interval", label = h3("Select the interval"),
                       choices = c("Daily", "Weekly", "Monthly"))
    )
  ),
  fluidRow(
    column(12,
           dygraphOutput("distPlot2")
    )
  ),
  fluidRow(
    column(6,
           plotOutput("pieChart")
    ),
    column(6,
           plotOutput("barChart")
    )
  ),
  fluidRow(
    column(12,
           DT::dataTableOutput("dataTable")
    )
  )
)

server <- function(input, output) {
  output$value <- renderPrint({ input$selected_region })
  
  output$selectedDateRange <- renderText({
    paste(format(input$date_range[1], "%Y-%m-%d"), " to ", format(input$date_range[2], "%Y-%m-%d"))
  })
  
  output$totalSales <- renderText({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      total_sales <- sum(filtered_data$Sales)
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      total_sales <- filtered_data %>%
        filter(Region == input$selected_region) %>%
        summarize(total_sales = sum(Sales)) %>%
        pull(total_sales)
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      total_sales <- filtered_data %>%
        filter(Category == input$selected_category) %>%
        summarize(total_sales = sum(Sales)) %>%
        pull(total_sales)
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else {
      total_sales <- filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category) %>%
        summarize(total_sales = sum(Sales)) %>%
        pull(total_sales)
      paste(format(round(total_sales, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    }
  })
  
  output$totalProfit <- renderText({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      total_profit <- sum(filtered_data$Profit)
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      total_profit <- filtered_data %>%
        filter(Region == input$selected_region) %>%
        summarize(total_profit = sum(Profit)) %>%
        pull(total_profit)
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      total_profit <- filtered_data %>%
        filter(Category == input$selected_category) %>%
        summarize(total_profit = sum(Profit)) %>%
        pull(total_profit)
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    } else {
      total_profit <- filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category) %>%
        summarize(total_profit = sum(Profit)) %>%
        pull(total_profit)
      paste(format(round(total_profit, 2), big.mark = ",", decimal.mark = ".", nsmall = 2), "INR")
    }
  })
  
  output$distPlot2 <- renderDygraph({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      filtered_data$interval <- case_when(
        input$selected_interval == "Daily" ~ format(filtered_data$date, "%Y-%m-%d"),
        input$selected_interval == "Weekly" ~ format(floor_date(filtered_data$date, "week"), "%Y-%m-%d"),
        input$selected_interval == "Monthly" ~ format(filtered_data$date, "%Y-%m"),
        input$selected_interval == "Yearly" ~ format(filtered_data$date, "%Y")
      )
      
      dy_data <- filtered_data %>%
        group_by(interval, Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        ungroup()
      
      dy_data_wide <- reshape2::dcast(dy_data, interval ~ Category, value.var = "total_sales", fill = 0)
      
      xtsdata <- xts(dy_data_wide[, -1], order.by = as.Date(paste0(dy_data_wide$interval, "-01")))
      dygraph(xtsdata) %>%
        dyRangeSelector()
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      region_sales <- filtered_data %>%
        filter(Region == input$selected_region)
      
      region_sales$interval <- case_when(
        input$selected_interval == "Daily" ~ format(region_sales$date, "%Y-%m-%d"),
        input$selected_interval == "Weekly" ~ format(floor_date(region_sales$date, "week"), "%Y-%m-%d"),
        input$selected_interval == "Monthly" ~ format(region_sales$date, "%Y-%m"),
        input$selected_interval == "Yearly" ~ format(region_sales$date, "%Y")
      )
      
      dy_data <- region_sales %>%
        group_by(interval, Category) %>%
        summarise(total_sales = sum(Sales)) %>%
        ungroup()
      
      dy_data_wide <- reshape2::dcast(dy_data, interval ~ Category, value.var = "total_sales", fill = 0)
      
      xtsdata <- xts(dy_data_wide[, -1], order.by = as.Date(paste0(dy_data_wide$interval, "-01")))
      dygraph(xtsdata) %>%
        dyRangeSelector()
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      category_sales <- filtered_data %>%
        filter(Category == input$selected_category)
      
      category_sales$interval <- case_when(
        input$selected_interval == "Daily" ~ format(category_sales$date, "%Y-%m-%d"),
        input$selected_interval == "Weekly" ~ format(floor_date(category_sales$date, "week"), "%Y-%m-%d"),
        input$selected_interval == "Monthly" ~ format(category_sales$date, "%Y-%m"),
        input$selected_interval == "Yearly" ~ format(category_sales$date, "%Y")
      )
      
      dy_data <- category_sales %>%
        group_by(interval, Region) %>%
        summarise(total_sales = sum(Sales)) %>%
        ungroup()
      
      dy_data_wide <- reshape2::dcast(dy_data, interval ~ Region, value.var = "total_sales", fill = 0)
      
      xtsdata <- xts(dy_data_wide[, -1], order.by = as.Date(paste0(dy_data_wide$interval, "-01")))
      dygraph(xtsdata) %>%
        dyRangeSelector()
    } else {
      region_category_sales <- filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category)
      
      region_category_sales$interval <- case_when(
        input$selected_interval == "Daily" ~ format(region_category_sales$date, "%Y-%m-%d"),
        input$selected_interval == "Weekly" ~ format(floor_date(region_category_sales$date, "week"), "%Y-%m-%d"),
        input$selected_interval == "Monthly" ~ format(region_category_sales$date, "%Y-%m"),
        input$selected_interval == "Yearly" ~ format(region_category_sales$date, "%Y")
      )
      
      dy_data <- region_category_sales %>%
        group_by(interval) %>%
        summarise(total_sales = sum(Sales)) %>%
        ungroup()
      
      xtsdata <- xts(dy_data$total_sales, order.by = as.Date(paste0(dy_data$interval, "-01")))
      dygraph(xtsdata) %>%
        dyRangeSelector()
    }
  })
  
  output$barChart <- renderPlot({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      sales_by_city <- filtered_data %>%
        group_by(City) %>%
        summarise(total_sales = sum(Sales)) %>%
        arrange(desc(total_sales))
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      sales_by_city <- filtered_data %>%
        filter(Region == input$selected_region) %>%
        group_by(City) %>%
        summarise(total_sales = sum(Sales)) %>%
        arrange(desc(total_sales))
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      sales_by_city <- filtered_data %>%
        filter(Category == input$selected_category) %>%
        group_by(City) %>%
        summarise(total_sales = sum(Sales)) %>%
        arrange(desc(total_sales))
    } else {
      sales_by_city <- filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category) %>%
        group_by(City) %>%
        summarise(total_sales = sum(Sales)) %>%
        arrange(desc(total_sales))
    }
    
    ggplot(data = sales_by_city, aes(x = reorder(City, -total_sales), y = total_sales)) +
      geom_bar(stat = "identity", fill = "purple") +
      labs(title = "Total Sales by City", x = "City", y = "Total Sales (INR)") +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
  })
  
  output$pieChart <- renderPlot({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      sales_by_region <- filtered_data %>%
        group_by(Region) %>%
        summarise(total_sales = sum(Sales))
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      sales_by_region <- filtered_data %>%
        filter(Region == input$selected_region) %>%
        group_by(Region) %>%
        summarise(total_sales = sum(Sales))
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      sales_by_region <- filtered_data %>%
        filter(Category == input$selected_category) %>%
        group_by(Region) %>%
        summarise(total_sales = sum(Sales))
    } else {
      sales_by_region <- filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category) %>%
        group_by(Region) %>%
        summarise(total_sales = sum(Sales))
    }
    
    pie_labels <- paste(sales_by_region$Region, ": ", sales_by_region$total_sales, " INR")
    
    pie_data <- sales_by_region$total_sales
    pie_percentage <- paste0(round(pie_data / sum(pie_data) * 100, 1), "%")
    
    pie_labels_with_percentage <- paste(pie_labels, "\n", pie_percentage)
    
    pie(sales_by_region$total_sales, labels = pie_labels_with_percentage, col = rainbow(length(pie_labels)))
    
    # Add label to the pie chart
    title("Pie Chart: Regional Sales")
  })
  
  
  output$dataTable <- DT::renderDataTable({
    filtered_data <- sales_data %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    if (input$selected_region == "All" && input$selected_category == "All") {
      filtered_data
    } else if (input$selected_region != "All" && input$selected_category == "All") {
      filtered_data %>%
        filter(Region == input$selected_region)
    } else if (input$selected_region == "All" && input$selected_category != "All") {
      filtered_data %>%
        filter(Category == input$selected_category)
    } else {
      filtered_data %>%
        filter(Region == input$selected_region, Category == input$selected_category)
    }
  })
}


shinyApp(ui = ui, server = server)
