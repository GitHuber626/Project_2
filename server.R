# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
    choose_graph <- reactive({
      get_graph(input$independent_variable, input$dependent_variable)
  })
    
    plottedgraph<-reactive({
      if (('Education'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) {return (output$barplot1)}
      else if (('Marital_Status'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) {return (output$barplot2)}
      else if (('Dt_Customer_Year'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) {return (output$barplot3)}
      else if (('Education'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$barplot4)}
      else if (('Marital_Status'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$barplot5)}
      else if (('Dt_Customer_Year'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$barplot6)}
      else if (('Dt_Customer_Year'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$lineplot1)}
      else if (('Year_Birth'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) {return (output$scatterplot1)}
      else if (('Income'%in%input$independent_variable) & ('MntProducts'%in%input$dependent_variable)) {return (output$scatterplot2)}
      else if (('Year_Birth'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$lineplot2)}
      else if (('Income'%in%input$independent_variable) & ('NumPhurchases'%in%input$dependent_variable)) {return (output$densityplot1)}
      
      
    })
    
    output$graphplot<-renderPlotly({
      dataplots = plottedgraph()
      dataplots=ggplotly(dataplots)
      print(dataplots)
      })
    
    output$barplot1 <- renderPlotly({
      choose_graph()%>%group_by(Education,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%ggplot(aes(x=MntProducts,Education))+geom_bar(aes(y=mean_prod,fill=Education),stat='identity',position='dodge')+labs(x='Product',y='Dollar',title='Education Impact on Purchase')
    })
    
    output$barplot2 <- renderPlotly({
      choose_graph()%>%group_by(Marital_Status,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%
      ggplot(aes(x=MntProducts,Marital_Status))+geom_bar(aes(y=mean_prod,fill=Marital_Status),stat='identity',position
                                                         ='dodge')+labs(x='Product',y='Dollar',title='Marital Status Impact on Purchase')
    })
      
    
    output$barplot3 <- renderPlotly({
      choose_graph()%>%group_by(Dt_Customer_Year,MntProducts)%>%summarise(mean_prod=mean(Dollars))%>%
        ggplot(aes(x=MntProducts,Dt_Customer_Year))+geom_bar(aes(y=mean_prod,fill=as.factor(Dt_Customer_Year)),stat='identity',position
                                                             ='dodge')+labs(x='Product',y='Dollar',title='Customer Enrollment Date Impact on Purchase')
    })
    
    output$barplot4 <- renderPlotly({
      choose_graph()%>%group_by(Education,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
        ggplot(aes(x=NumPhurchases,Education))+geom_bar(aes(y=mean_freq,fill=Education),stat='identity',position
                                                        ='dodge')+labs(
                                                          x='Purchase Means',y=
                                                            'Avg Frequency',title=
                                                            'Education Impact on Purchase Frequency'
                                                        )
    })
    
    
    output$barplot5 <- renderPlotly({
      choose_graph()%>%group_by(Marital_Status,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
        ggplot(aes(x=NumPhurchases,Marital_Status))+geom_bar(aes(y=mean_freq,fill=Marital_Status),stat='identity',position
                                                             ='dodge')+labs(x='Purchase Means',y='Avg Frequency',title='Marital Status Impact on Purchase Frequency')
    })
    
    
    output$barplot6 <- renderPlotly({
      choose_graph()%>%group_by(Dt_Customer_Year,NumPhurchases)%>%summarise(mean_freq=mean(Number))%>%
        ggplot(aes(x=NumPhurchases,Dt_Customer_Year))+geom_bar(aes(y=mean_freq,fill=as.factor(Dt_Customer_Year)),stat='identity',position
                                                               ='dodge')+labs(x='Purchases Means',y='Avg Frequency',title='Customer Enrollment Date Impact on Purchase Frequency')
    })
    
    output$lineplot1 <- renderPlotly({
      choose_graph()%>%group_by(Dt_Customer_Year,NumPhurchases)%>%ggplot(aes(x=Dt_Customer_Year))+geom_line(aes(y=CPI_Increase,colour='CPI%'),size=2)+geom_line(aes(y=AWI_Increase,colour='AWI%'),size=2)+scale_color_manual("",breaks=c('CPI%','AWI%'),values=c('red','blue'))+scale_x_continuous(breaks=c(2012,2013,2014))
    })

    output$scatterplot1 <- renderPlotly({
      choose_graph()%>%group_by(MntProducts,Year_Birth)%>%select(c(2,5,6,7,23,24))%>%ggplot()+geom_point(
        aes(x=Year_Birth,y=Dollars,colour=MntProducts))+ggtitle('Amount Spent on Different Products')+scale_x_continuous(limits=c(1945,1996))
    })
    
    output$scatterplot2 <- renderPlotly({
      choose_graph()%>%group_by(MntProducts,Income)%>%select(c(2,5,6,7,23,24))%>%ggplot()+geom_point(
        aes(x=Income,y=Dollars,colour=MntProducts))+ggtitle('Amount Spent on Different Products')+scale_x_continuous(limits=c(2000,120000)) 
    })
    
    output$lineplot2 <- renderPlotly({
      choose_graph()%>%group_by(NumPhurchases,Year_Birth)%>%select(c(2,5,6,7,24,25))%>%summarise(meanfreq=mean(Number))%>%ggplot()+geom_line(
        aes(x=Year_Birth,y=meanfreq,colour=NumPhurchases))+ggtitle('Purchase Frequency on Different Products')+scale_x_continuous(limits=c(1945,1996)) 
    })
    
    output$densityplot1 <- renderPlotly({
      choose_graph()%>%group_by(NumPhurchases,Income)%>%select(c(2,5,6,7,24,25))%>%summarise(meanfreq=mean(Number))%>%ggplot(aes(x=Income))+geom_density(aes(y=meanfreq,colour=NumPhurchases),stat='identity')+ggtitle('Purchase Frequency on Different Products')+scale_x_continuous(limits=c(2000,120000))
    })
    
    
    
    })



# generate bins based on input$bins from ui.R
# x    <- faithful[, 2]
# bins <- seq(min(x), max(x), length.out = input$bins + 1)
# 
# # draw the histogram with the specified number of bins
# hist(x, breaks = bins, col = 'darkgray', border = 'white')