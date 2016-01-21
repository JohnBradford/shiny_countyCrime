library(shiny)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
#devtools::install_github("jrnold/ggthemes")
# library(choroplethr)
# library(choroplethrMaps)
# library(RColorBrewer)
library(Cairo)   # For nicer ggplot2 output when deployed on Linux

# data(state.map)
# data(county.map)
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

shinyServer(function(input, output) {

  
  ranges <- reactiveValues(x = NULL, y = NULL)
  



  datasetInput <-  reactive({
    
    
    if (input$unit == "County") {
      cn <- cn
    } else {
      cn <- cnState
    }
    
#     if (input$unit == "State") 
#     {df <- df %>% 
#       filter(STATE == input$st)
#     }
#     
    DV <- switch(input$dv, 
                 "Homicides" = cn$crimeHomicide,
                 "Violent Crime" = cn$violentCrime,
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
    
    IV <- switch(input$iv,
                 "Homicides" = cn$crimeHomicide,
                 "Violent Crime" = cn$violentCrime,
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
    STATE <- cn$State
    
    if(input$unit == "County") {
      NAME <- cn$NAME
      df <- cbind.data.frame(NAME, STATE, DV, IV, POP, SIZE, stringsAsFactors = F) %>% 
        arrange(desc(SIZE))
      if (input$st != "Show All") 
      {df <- df %>% 
        filter(STATE == input$st)
      }} else {
        NAME <- cn$State
        df <- cbind.data.frame(STATE, DV, IV, POP, SIZE, stringsAsFactors = F) %>% 
          arrange(desc(SIZE))
        #input$st <- "Show All"
        
      }
      
      
    #region <- cn$region
    
   df

    })
    

#   output$summary <- renderPrint({
# 
#     str(datasetInput())
# 
#   })
#   
  
#   vals <- reactiveValues(
#     showing = rep(TRUE, nrow(cn))
#   )
  
  output$plot1 <- renderPlot({
    df <- datasetInput()
   # df <- arrange(datasetInput(), desc(SIZE))
    n <- input$nlabels
   # if(input$unit=="County"){
      colorVar <- df$STATE
      if (input$st != "Show All" & input$unit == "County") {colorVar <- df$SIZE}
      if (input$unit == "State"){colorVar <- df$STATE}
    #}
    #if(input$unit=="State"){colorVar <- df$STATE}
    
    
    
    topList <- df[1:n,]
      #df[!vals$keeprows, , drop = FALSE]
     

    
    # Plot the kept and excluded points as two separate data sets
    #keep    <- df[ vals$keeprows, , drop = FALSE]
    #showName <- df[!vals$keeprows, , drop = FALSE]
    
    
    
    thePlot <- ggplot(data=df, aes(y=DV, x=IV)) +
      geom_point(aes(size=SIZE, color=colorVar, alpha=SIZE))


    
    if(input$regL == "Yes"){
      Model <- lm(df$DV ~ df$IV, weights = df$POP)
      S <- summary(Model)
      thePlot <- thePlot +    
        geom_abline(intercept=Model$coefficients[1], 
                    slope= Model$coefficients[2], color="red") +
        ggtitle(paste("Linear Regression of ", input$dv, " on ", input$iv, " Weighted by Population.", "\nR-squared: ", round(S$r.squared, 2), "  Coeff.:  ", 
                      round(S$coefficients[2], 2), sep="")) 
      
      }
    
    plotOpts <- theme(axis.title.y = element_text(angle=90)) 

#     colorLegend <- if (input$st != "Show All") {scale_colour_continuous(guide = FALSE) }
#     else{scale_colour_discrete(guide = FALSE)}
    
    gText <- if(input$unit == "County") {geom_text(data=topList, aes(x=IV, y=DV, label=NAME), size=4, vjust=1)}
    else {geom_text(data=topList, aes(x=IV, y=DV, label=STATE), size=4, vjust=1)}
    

    thePlot  +
      gText +
      ylab(label=input$dv) + xlab(label=input$iv) + 
      guides(colour=FALSE, size=FALSE, alpha=FALSE) +
      #scale_size_continuous(guide = FALSE) +
      #scale_alpha_continuous(guide = FALSE) +
      #colorLegend +
      plotOpts +
      theme_gdocs() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y)

    
#     #MAPS
#     df$value <- df$SIZE
#     myPalette <- colorRampPalette(rev(brewer.pal(5, "Spectral")))
#     sc <- scale_fill_gradientn(colours = myPalette(100), limits=c(0,.65))
#     t <-  theme(text=element_text(size=14),
#                 plot.title = element_text(size=18), axis.title.x= element_text(size=12)) 
#     
#     county_choropleth(df, 
#                       num_colors=1) + theme_gdocs() + t + sc
#   
    
  })
  
  # Toggle points that are clicked
#   observeEvent(input$plot1_click, {
#     res <- nearPoints(df, input$plot1_click, allRows = TRUE)
#     
#     vals$keeprows <- xor(vals$keeprows, res$selected_)
#   })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observeEvent(input$plot1_dblclick, {
    brush <- input$plot1_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  output$click_info <- renderPrint({
    df <- datasetInput()
    
    if(input$unit == "County") {
      df <- df[,1:5]
    names(df) <- c("County", "State", input$dv, input$iv, "Pop") # input$scale, input$scale )
    } else {
      df <- df[,1:4]
      names(df) <- c("State", input$dv, input$iv, "Pop")
    }
    #df <- df[,-6]
#     new_df <- df
#     new_df[,1] <- df$County
#     new_df[,2] <- df$State
#     new_df[,3] <- df[,1]
#     new_df[,4] <- df[,2]
    varNames <- names(df)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(df=df, xvar=input$iv, yvar=input$dv, input$plot1_click, addDist = F)
  })
  
#   output$hover_info <- renderPrint({
#     cat("input$plot_hover:\n")
#     str(input$plot_hover)
#   })
  
})