library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
#devtools::install_github("jrnold/ggthemes")
library(choroplethr)
library(choroplethrMaps)
library(RColorBrewer)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

data(state.map)
data(county.map)
cn <- read.csv("countyCrimeData.csv", na.strings=c("", "NA"), stringsAsFactors = F )


shinyServer(function(input, output) {


  datasetInput <-  reactive({
    DV <- switch(input$dv, 
                  "Homicides" = cn$crimeHomicide,
                  "Violent Crime" = cn$violentCrime)
    
    IV <- switch(input$iv,
            "High School Graduation" = cn$HS_grad,
            "Unemployment" = cn$Unemployment, 
            "Child Poverty" = cn$childPoverty,
            "Income Inequality" = cn$incomeInequalityRatio, 
            "Median Income" = cn$medianIncome, 
            "Single Parent Households" = cn$childSingleParent, 
            "Social Associations" = cn$socialAssociation,
            "Black Pop." = cn$blackPrcnt_nonH, 
            "Hispanic Pop." = cn$hispanicPrcnt, 
            "White Pop." = cn$whitePrcnt_nonH, 
            "Female Pop." = cn$femalePrcnt, 
            "Rural Pop." = cn$ruralPrcnt)
     
    SIZE <- switch(input$scale, 
           "Independent Variable" = IV,
           "Dependent Variable" = DV, 
           "Population" = cn$pop2011)
    
    POP <- cn$pop2011
    
    NAME <- cn$NAME
    
    STATE <- cn$State
    
    cbind.data.frame(DV, IV, SIZE, POP, NAME, STATE, stringsAsFactors = F)
    })
    

#   output$summary <- renderPrint({
# 
#     str(datasetInput())
# 
#   })
#   
  
  output$distPlot <- renderPlot({

    df <- arrange(datasetInput(), desc(SIZE))
    n <- input$nlabels
    colorVar <- df$STATE
    if (input$st != "Show All") 
      {df <- df %>% 
      filter(STATE == input$st)
      colorVar <- df$SIZE
      }
    topList <- df[1:n,]

    
  thePlot <- ggplot(data=df, aes(y=DV, x=IV))

    Model <- lm(df$DV ~ df$IV, weights = df$POP)
    S <- summary(Model)
    
    plotOpts <- theme(axis.title.y = element_text(angle=90)) 

      
    colorLegend <- if (input$st != "Show All") {scale_colour_continuous(guide = FALSE) }
    else{scale_colour_discrete(guide = FALSE)}
    
    

    thePlot + geom_point(aes(size=SIZE, color=colorVar)) +
   geom_abline(intercept=Model$coefficients[1], slope= Model$coefficients[2], color="red") +
    geom_text(data=topList, aes(x=IV, y=DV, label=NAME, size=SIZE), vjust=1) +
      ylab(label=input$dv) + xlab(label=input$iv) + 
      ggtitle(paste("Linear Regression of ", input$dv, " on ", input$iv, " for US Counties", 
                    "\nWeighted by Population.  ", "R-squared: ", round(S$r.squared, 2), "  Coeff.:  ", 
                    round(S$coefficients[2], 2), sep="")) + scale_size_continuous(guide = FALSE) +
      colorLegend +
      plotOpts +
      theme_gdocs() 

    
  
    
  })
  
})