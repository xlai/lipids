require(glinternet)
require(glmnet)
require(tidyverse)
require(coefplot)
#require(cowplot)

#load data and reference length of each chromosome
load('dat.Rdata')

Y = subset(dat.diff, select = c(n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
X = subset(dat.diff, select = -c(id,visit,compliance,n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
## Contrust variables required for glithernet package
numLevels <- X %>% sapply(nlevels)
numLevels[numLevels==0] <- 1
i_num <- sapply(X, is.numeric)
idx_num <- (1:length(i_num))[i_num]
idx_cat <- (1:length(i_num))[!i_num]
X$trt <- as.integer(X$trt)-1
X$sex <- as.integer(X$sex)-1

get_lasso_interaction<- function(X, Y, numLevels, response_var='n3index',i_num = i_num){
    # locate index of the respon
    i = which(names(Y)== response_var)
    #Set aside training data for cv
    n = dim(Y)[1]
    set.seed(1234)
    cv_fit <- glinternet.cv(X, unlist(Y[i]), numLevels, 
                        interactionCandidates=c(1),numCores=2)
    return(cv_fit)
    }

get_lasso <- function(X, Y, response_var='n3index'){
    # locate index of the respon
    i = which(names(Y)== response_var)
    #Set aside training data for cv
    n = dim(Y)[1]
    set.seed(1234)
    lasso.cv = cv.glmnet(as.matrix(X), as.matrix(Y[i]),nfolds = 10)
return(lasso.cv)
}

function(input, output, session) {
    observeEvent(input$sample1.df, {
        choice1 <- switch(input$sample1.df,
        PT = PT.name,
        SC = SC.name,
        RT = RT.name,
        RSC = RSC.name
        )
        updatePickerInput(
            session,
            inputId = "sample1.id",
            choices = choice1
        )
    })
    observeEvent(input$sample2.df, {
        choice2 <- switch(input$sample2.df,
        PT = PT.name,
        SC = SC.name,
        RT = RT.name,
        RSC = RSC.name
        )
        updatePickerInput(
            session,
            inputId = "sample2.id",
            choices = choice2
        )
    })
    plt.x.axis <- reactive({
        auc_per_interval(df.PT,input$sample.res,input$sample1.id,
			input$sample.chr,input$sample.arm,ref.length )[[1]]
    })
    a1 <- reactive({
        auc_per_interval(df.list[[input$sample1.df]],input$sample.res,input$sample1.id,
			input$sample.chr,input$sample.arm,ref.length )[[2]]
    })

    a2 <- reactive({
	auc_per_interval(df.list[[input$sample2.df]],input$sample.res,input$sample2.id,
			input$sample.chr,input$sample.arm,ref.length )[[2]]
    })    
    output$plot1 <- renderPlot({
        r23 = cor(a1(),a2())
        par(mfrow=c(2,1))
        plot(plt.x.axis(), a1(),type='l', ylim=c(-2,2), lwd=1.5,
             ylab=paste('AUC/',input$sample.res,'Mb'),xlab=NA,main=paste('Overall Pearson r = ', r23)
            )
        lines(plt.x.axis(), a2(),col=2,lwd=1.5)
        legend('topright',legend=c(input$sample1.id, input$sample2.id),
        col=c("black", "red"), lty=1, cex=0.8)
        plot(plt.x.axis(),runCor(a1(),a2()),'l',main="Rolling Pearson correlation",
             ylab='r',xlab=paste('chr',input$sample.chr,'.',input$sample.arm),
             ylim=c(-1,1)
             )
        abline(h=0.0, col='blue',lty=2)

  })

}
