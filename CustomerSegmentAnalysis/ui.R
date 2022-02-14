
# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Supermarket Customer Segment Analysis"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
          selectizeInput(inputId = "independent_variable",
                         label = "independent_variable",
                         choices = list('Select','Education','Marital_Status','Dt_Customer_Year','Year_Birth','Income'),selected="Select"),
          selectizeInput(inputId = "dependent_variable",
                         label = "dependent_variable",
                         choices = list('Select','MntProducts','NumPhurchases'),selected="Select")
        ),
          
        # Show a plot of the generated distribution
        mainPanel(
          fluidRow(
            plotlyOutput("graphplot"))
        )
    )
))
