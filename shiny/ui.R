require(ggExtra)
require(ggplot2)


#load data and reference length of each chromosome
load('dat.Rdata')

Y = subset(dat.diff, select = c(n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
Y.name <- names(Y)

class_select <- list('Primary Baseline' = 'PT', 'Single Cell' = 'SC', 
  'Recurrent Primary' = 'RT' , 'Recurrent Single Cell' = 'RSC'
)

ui <- fluidPage(
    titlePanel('Lasso analysis'),
    sidebarPanel(
        selectInput('Y.choice', 'Choose the response variable Y', Y.name),
        sliderInput(inputId = "lambda.choice",
              label = "Penalisation parameter",
              min = 1,
              max = 100,
              value = NULL),
        textOutput("text1")
  ),
    # Main panel for displaying outputs ----
    mainPanel(
        plotOutput('distPlot'),
        plotOutput('diagPlot'),        
        # Output: HTML table with requested number of observations ----
        tableOutput("view"),
        tableOutput("view2"),        
#        verbatimTextOutput("summary")
  )
)