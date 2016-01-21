library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
# library(choroplethr)
# library(choroplethrMaps)
# library(RColorBrewer)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux
cn <- read.csv("countyCrimeData.csv", na.strings=c("", "NA"), stringsAsFactors = F )
cnState <- cn  %>% group_by(State) %>% 
  summarise(n=n(), HS_grad=weighted.mean(HS_grad, pop2011, na.rm=T),
            Unemployment=weighted.mean(Unemployment, pop2011, na.rm=T),
            childPoverty=weighted.mean(childPoverty, pop2011, na.rm=T),
            incomeInequalityRatio=weighted.mean(incomeInequalityRatio, pop2011, na.rm=T),
            childSingleParent=weighted.mean(childSingleParent, pop2011, na.rm=T),
            socialAssociation=weighted.mean(socialAssociation, pop2011, na.rm=T),
            violentCrime=weighted.mean(violentCrime, pop2011, na.rm=T),
            blackPrcnt_nonH=weighted.mean(blackPrcnt_nonH, pop2011, na.rm=T),
            hispanicPrcnt=weighted.mean(hispanicPrcnt, pop2011, na.rm=T),
            whitePrcnt_nonH=weighted.mean(whitePrcnt_nonH, pop2011, na.rm=T),
            femalePrcnt=weighted.mean(femalePrcnt, pop2011, na.rm=T),
            ruralPrcnt=weighted.mean(ruralPrcnt, pop2011, na.rm=T),
            medianIncome=weighted.mean(medianIncome, pop2011, na.rm=T),
            crimeHomicide=weighted.mean(crimeHomicide, pop2011, na.rm=T),
            pop2011 = sum(pop2011, na.rm=T))
stateList <- unique(cn$State)
shinyUI(fluidPage(

  # Application title
  titlePanel("County and State-Level Regression Data"),

  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      #p("by John H. Bradford"),
      #h6("Data from countyhealthrankings.org"),
      radioButtons("regL", label="Add Regression Line?", choices=c("Yes", "No"), selected="No"),
      radioButtons("unit", label="Organize Data by:", choices=c("County", "State"), selected="County"),
      selectInput("st", label="Filter by State", c("Show All", stateList), selected="Show All"),
      selectInput("dv", label="Y-axis - Dependent Variable", 
                  c("Homicides","Violent Crime","High School Graduation","Unemployment", "Child Poverty",
                    "Income Inequality", "Median Income", "Single Parent Households", "Social Associations",
                    "Black Pop.", "Hispanic Pop.", "White Pop.", "Female Pop.", 
                    "Rural Pop."), selected="Homicides"),
      
      selectInput("iv", label="X-axis - Independent Variable", 
                                    c("Homicides","Violent Crime","High School Graduation","Unemployment", "Child Poverty",
                                      "Income Inequality", "Median Income", "Single Parent Households", "Social Associations",
                                      "Black Pop.", "Hispanic Pop.", "White Pop.", "Female Pop.", 
                                      "Rural Pop."), selected="Unemployment"),
      
#       selectInput("dv", label="Y-axis - Dependent Variable", c("Homicides","Violent Crime"), selected="Homicides"),
#       selectInput("iv", label="X-axis - Independent Variable", 
#                   c("High School Graduation","Unemployment", "Child Poverty",
#                     "Income Inequality", "Median Income", "Single Parent Households", "Social Associations",
#                     "Black Pop.", "Hispanic Pop.", "White Pop.", "Female Pop.", 
#                     "Rural Pop."), selected="Unemployment"),
      selectInput("scale", label="Scale According to which variable?", 
                  c("Independent Variable","Dependent Variable", "Population"), selected="Dependent Variable"),
      sliderInput("nlabels", label="Show How Many Labels?",
                  min = 10,
                  max = 50,
                  value = 10, 
                  step = 10),
      h6("by John H. Bradford."),
      h6("Data from countyhealthrankings.org")
      ),
    # Show a plot of the generated distribution
    mainPanel(
      h6("Select area and double-click to zoom.  Double-click to zoom out.  Click on data point to see more info."),
      #h6("")
      plotOutput("plot1",
#                  hover = hoverOpts(
#                    id = "plot_hover"),
                 click = "plot1_click",
                 dblclick = "plot1_dblclick",
                 brush = brushOpts(
                   id = "plot1_brush",
                   resetOnNew = TRUE
                 )),
        verbatimTextOutput("click_info")
      #verbatimTextOutput("hover_info")
      
    )
  )
))