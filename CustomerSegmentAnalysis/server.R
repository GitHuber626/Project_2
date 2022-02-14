# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    
    
    plottedgraph<-reactive({
      if (('Education'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) return (barplot1)
      if (('Marital_Status'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) return (barplot2)
      if (('Dt_Customer_Year'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) return (barplot3)
      if (('Education'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (barplot4)
      if (('Marital_Status'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (barplot5)
      if (('Dt_Customer_Year'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (barplot6)
      if (('Dt_Customer_Year'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (lineplot1)
      if (('Year_Birth'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) return (scatterplot1)
      if (('Income'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) return (scatterplot2)
      if (('Year_Birth'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (lineplot2)
      if (('Income'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) return (densityplot1)
      
      
    })
    
    output$graphplot<-renderPlotly({
      dataplots = plottedgraph()
      print(dataplots)
      })
    
    
    
    
    
    })



# generate bins based on input$bins from ui.R
# x    <- faithful[, 2]
# bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
# # draw the histogram with the specified number of bins
# hist(x, breaks = bins, col = 'darkgray', border = 'white')