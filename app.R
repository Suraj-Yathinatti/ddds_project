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

sales_data$date<-as.Date(lubridate::parse_date_time(sales_data$Order.Date, orders = c('mdy')))

