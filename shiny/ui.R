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
    selectInput('sample1.df', 'Select data source 1', class_select, selected=class_select[[1]]),
    pickerInput('sample1.id', 'Select sample 1', choices = class_select, selected=NULL),
    selectInput('sample2.df', 'Select data source 2', class_select, selected=class_select[[2]]),
    pickerInput('sample2.id', 'Select sample 2', choices = class_select, selected=NULL),
    numericInput('sample.chr', 'Chromosome', 1, min = 1, max = 22),
    numericInput('sample.res', 'Resolution /Mbp', 3, min = 0.1, max = 10),          
    selectInput('sample.arm', 'Chromosome arm', c('p','q')),
  ),

  mainPanel(
    plotOutput('plot1')
  )
)

var.name = 'n3index'
p <-
  ggplot(data = filter(melt.diff,variable==var.name), aes(x = value,color=trt,fill=trt)) +
  geom_point(aes(y = 0.1), alpha = 0) + # add an invisible scatterplot geom as the first layer
  geom_density(alpha = 0.5)+
    scale_x_continuous(var.name) + 
    theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

p %>% ggMarginal(type = "boxplot", 
                 margins = "x", 
                 size = 5,
                 groupColour = TRUE,
                 groupFill = TRUE) 