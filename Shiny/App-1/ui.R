#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(randomForest)
lca <- read.csv("data/LCA_data.csv")
prediction <- read.csv("data/Prediction_data.csv")

companies<-table(lca["LCA_CASE_EMPLOYER_NAME"])
companynames<-names(companies)
companynames<-append(companynames, "All", after=0)

pCompanies<-table(prediction["LCA_CASE_EMPLOYER_NAME"])
pCompanynames<-names(pCompanies)
pCompanynames<-append(pCompanynames, "Select One", after=0)

city<-table(lca["LCA_CASE_WORKLOC1_CITY"])
citynames<-names(city)
citynames<-append(citynames, "All", after=0)

pCity<-table(prediction["LCA_CASE_EMPLOYER_CITY"])
pCitynames<-names(pCity)
pCitynames<-append(pCitynames, "Select One", after=0)

pJobtitles<-table(prediction["LCA_CASE_JOB_TITLE"])
pJobtitlenames<-names(pJobtitles)
pJobtitlenames<-append(pJobtitlenames, "Select One", after=0)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  
  ################################## 
  ##### Salary range predictor #####
  ##################################
  titlePanel("Salary Trends"),
  fluidRow(
    column(4,
           selectInput("pCompany", "Company:", choices = pCompanynames)),
    column(4,
           selectInput("pCity", "City:", choices = pCitynames)),
    column(4,
           selectInput("pTitle", "Job title:", choices = pJobtitlenames))
  ),
  
  fluidRow(
    column(4, textOutput("predictedSalary"), offset = 4)
  ),
  
  titlePanel("Explore the raw data"),
  ############################### 
  ##### Company & Job title #####
  ###############################
  sidebarLayout(
    sidebarPanel(
      selectInput("company", "Filter by company:", 
                  choices=companynames),
      uiOutput("jobtitle"),
      checkboxInput("removeoutliers", "Remove outliers",
                    value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("plotText"),
      plotOutput("distPlot")
    )
  ),
  
  ############################### 
  ###### City & Job title #######
  ###############################
  sidebarLayout(
    sidebarPanel(
      selectInput("city", "Filter by city:", 
                  choices=citynames),
      uiOutput("jobtitle2"),
      checkboxInput("removeoutliers2", "Remove outliers",
                    value = TRUE)
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      textOutput("plotText2"),
      plotOutput("distPlot2")
    )
  )
))
