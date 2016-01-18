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
# cn$showName <- FALSE

shinyServer(function(input, output) {

  ranges <- reactiveValues(x = NULL, y = NULL)
  
  # For storing which rows have been excluded


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
    
    region <- cn$region
    
    df <- cbind.data.frame(NAME, STATE, DV, IV, POP, SIZE, region, stringsAsFactors = F) %>% 
   #df <- cbind.data.frame(DV, IV, SIZE, POP, NAME, STATE, stringsAsFactors = F) %>% 
     arrange(desc(SIZE))
   if (input$st != "Show All") 
   {df <- df %>% 
     filter(STATE == input$st)
   }
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
    colorVar <- df$STATE
    if (input$st != "Show All") {colorVar <- df$SIZE}
    
    
    
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
        ggtitle(paste("Linear Regression of ", input$dv, " on ", input$iv, " for US Counties", 
                      "\nWeighted by Population.  ", "R-squared: ", round(S$r.squared, 2), "  Coeff.:  ", 
                      round(S$coefficients[2], 2), sep="")) 
      
      }
    
    plotOpts <- theme(axis.title.y = element_text(angle=90)) 

    colorLegend <- if (input$st != "Show All") {scale_colour_continuous(guide = FALSE) }
    else{scale_colour_discrete(guide = FALSE)}
    

    thePlot  +
    geom_text(data=topList, aes(x=IV, y=DV, label=NAME), size=4, vjust=1) +
      ylab(label=input$dv) + xlab(label=input$iv) + 
      scale_size_continuous(guide = FALSE) +
      scale_alpha_continuous(guide = FALSE) +
      colorLegend +
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
    df <- df[,1:5]
    names(df) <- c("County", "State", input$dv, input$iv, "Pop") # input$scale, input$scale )
    #df <- df[,-6]
#     new_df <- df
#     new_df[,1] <- df$County
#     new_df[,2] <- df$State
#     new_df[,3] <- df[,1]
#     new_df[,4] <- df[,2]
    varNames <- names(df)
    # Because it's a ggplot2, we don't need to supply xvar or yvar; if this
    # were a base graphics plot, we'd need those.
    nearPoints(df=df, xvar=varNames[4], yvar=varNames[3], input$plot1_click, addDist = TRUE)
  })
  
#   output$hover_info <- renderPrint({
#     cat("input$plot_hover:\n")
#     str(input$plot_hover)
#   })
  
})