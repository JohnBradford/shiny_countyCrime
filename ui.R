library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

stateList <- unique(cn$State)
shinyUI(fluidPage(
  # Application title
  titlePanel("County Level Crime Data Regression"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      p("by John H. Bradford"),
      p("Data from countyhealthrankings.org"),
      selectInput("st", label="Filter by State", c("Show All", stateList), selected="Show All"),
      selectInput("dv", label="Dependent Variable", c("Homicides","Violent Crime"), selected="Homicides"),
      selectInput("iv", label="Independent Variable", 
                  c("High School Graduation","Unemployment", "Child Poverty",
                    "Income Inequality", "Median Income", "Single Parent Households", "Social Associations",
                    "Black Pop.", "Hispanic Pop.", "White Pop.", "Female Pop.", 
                    "Rural Pop."), selected="Unemployment"),
      selectInput("scale", label="Scale According to which variable?", 
                  c("Independent Variable","Dependent Variable", "Population"), selected="Dependent Variable"),
      sliderInput("nlabels", label="Show Labels of How Many Counties?",
                  min = 10,
                  max = 50,
                  value = 10, 
                  step = 10)
      ),
    # Show a plot of the generated distribution
    mainPanel(
      #verbatimTextOutput("John Bradford"),
      plotOutput("distPlot")
    )
  )
))