#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(caret)
library(randomForest)
lca <- read.csv("data/LCA_data.csv")
load("data/ranForestModel.RData")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  yearly<-lca[lca["LCA_CASE_WAGE_RATE_UNIT"]=="Year",]
  hourly<-lca[lca["LCA_CASE_WAGE_RATE_UNIT"]=="Hour",]

  ################################## 
  ##### Salary range predictor #####
  ##################################
  output$predictedSalary <- renderText({
    if (input$pCompany == "Select One" 
        | input$pCity == "Select One" 
        | input$pTitle == "Select One") {
      paste("Please select company, city & job title above too see predicted range.")
    } else {
      LCA_CASE_EMPLOYER_NAME <- input$pCompany
      LCA_CASE_EMPLOYER_CITY <- input$pCity
      LCA_CASE_JOB_TITLE <- input$pTitle
      pred <- predict(ranForestModel, data.frame(
        LCA_CASE_EMPLOYER_NAME,
        LCA_CASE_EMPLOYER_CITY,
        LCA_CASE_JOB_TITLE))
      paste(pred)
    }
  })
  
  ############################### 
  ##### Company & Job title #####
  ###############################
  output$jobtitle <-  renderUI({
    # Filter data set by company
    if (input$company == "All") {
      # Compute job titles across all companies
      filtered_lca<-lca
    } else {
      # Filter job titles choice based on company choice.
      filtered_lca <- lca[lca["LCA_CASE_EMPLOYER_NAME"]==input$company,]
    }
    
    jobtitles<-filtered_lca[,"LCA_CASE_JOB_TITLE"]
    jobtitlenames<-unique(jobtitles)
    jobtitlenames<-as.character(jobtitlenames)
    jobtitlenames<-append(jobtitlenames, "All", after=0)
    selectInput("jobtitle", "Job Title:", choices=jobtitlenames)
  })
  
  output$distPlot <- renderPlot({
    # Filter by selected company
    if (input$company == "All") {
      filtered_yearly <- yearly
      filtered_hourly <- hourly
    } else {
      filtered_yearly <- yearly[yearly["LCA_CASE_EMPLOYER_NAME"]==input$company,]
      filtered_hourly <- hourly[hourly["LCA_CASE_EMPLOYER_NAME"]==input$company,]
    }
    # Filter by selected job title
    if (!is.null(input$jobtitle)) {
      if (input$jobtitle == "All") {
        # Do nothing
        filtered_yearly <- filtered_yearly
        filtered_hourly <- filtered_hourly
      } else {
        if (length(rownames(filtered_yearly["LCA_CASE_JOB_TITLE"])) > 0) {
          filtered_yearly <- 
            filtered_yearly[filtered_yearly["LCA_CASE_JOB_TITLE"]==input$jobtitle,]
        }
        if (length(rownames(filtered_hourly["LCA_CASE_JOB_TITLE"])) > 0) {
          filtered_hourly <- 
            filtered_hourly[filtered_hourly["LCA_CASE_JOB_TITLE"]==input$jobtitle,]
        }
      }
    }
    salaries <- data.matrix(filtered_yearly["LCA_CASE_WAGE_RATE_FROM"])
    salaries <- append(salaries, 
                       data.matrix(filtered_hourly["LCA_CASE_WAGE_RATE_FROM"])*2087)
    if(input$removeoutliers) {
      salaries <- subset(salaries,
                         !(salaries > quantile(salaries, probs=c(.005, .995))[2] 
                           | salaries < quantile(salaries, probs=c(.005, .995))[1]))
    }
    # Render the plot
    if (length(salaries) == 0) {
      output$plotText <- renderText({"No data to plot! Maybe try including outliers."})
    } else {
      output$plotText <- renderText({""})
      hist(salaries,
           breaks = 40,
           ylab="# of people hired",
           xlab="Annual salary")
    }
  })
  
  
  ############################### 
  ###### City & Job title #######
  ###############################
  output$jobtitle2 <-  renderUI({
    # Filter data set by city
    if (input$city == "All") {
      # Compute job titles across all cities
      filtered_lca<-lca
    } else {
      # Filter job titles choice based on city choice.
      filtered_lca <- lca[lca["LCA_CASE_WORKLOC1_CITY"]==input$city,]
    }
    
    jobtitles<-filtered_lca[,"LCA_CASE_JOB_TITLE"]
    jobtitlenames<-unique(jobtitles)
    jobtitlenames<-as.character(jobtitlenames)
    jobtitlenames<-append(jobtitlenames, "All", after=0)
    selectInput("jobtitle2", "Job Title:", choices=jobtitlenames)
  })
  
  output$distPlot2 <- renderPlot({
    # Filter by selected city
    if (input$city == "All") {
      filtered_yearly <- yearly
      filtered_hourly <- hourly
    } else {
      filtered_yearly <- yearly[yearly["LCA_CASE_WORKLOC1_CITY"]==input$city,]
      filtered_hourly <- hourly[hourly["LCA_CASE_WORKLOC1_CITY"]==input$city,]
    }
    # Filter by selected job title
    if (!is.null(input$jobtitle2)) {
      if (input$jobtitle2 == "All") {
        # Do nothing
        filtered_yearly <- filtered_yearly
        filtered_hourly <- filtered_hourly
      } else {
        if (length(rownames(filtered_yearly["LCA_CASE_JOB_TITLE"])) > 0) {
          filtered_yearly <- 
            filtered_yearly[filtered_yearly["LCA_CASE_JOB_TITLE"]==input$jobtitle2,]
        }
        if (length(rownames(filtered_hourly["LCA_CASE_JOB_TITLE"])) > 0) {
          filtered_hourly <- 
            filtered_hourly[filtered_hourly["LCA_CASE_JOB_TITLE"]==input$jobtitle2,]
        }
      }
    }
    salaries <- data.matrix(filtered_yearly["LCA_CASE_WAGE_RATE_FROM"])
    salaries <- append(salaries, 
                       data.matrix(filtered_hourly["LCA_CASE_WAGE_RATE_FROM"])*2087)
    if(input$removeoutliers2) {
      salaries <- subset(salaries,
                         !(salaries > quantile(salaries, probs=c(.005, .995))[2] 
                           | salaries < quantile(salaries, probs=c(.005, .995))[1]))
    }
    # Render the plot
    if (length(salaries) == 0) {
      output$plotText2 <- renderText({"No data to plot! Maybe try including outliers."})
    } else {
      output$plotText2 <- renderText({""})
      hist(salaries,
           breaks = 40,
           ylab="# of people hired",
           xlab="Annual salary")
    }
  })
})
