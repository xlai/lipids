require(tidyverse)
#require(coefplot)
#require(cowplot)
require(reshape2)
require(glinternet)
#load data 
load('dat.Rdata')

Y = subset(dat.diff, select = c(n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
X = subset(dat.diff, select = -c(id,visit,compliance,n3index,total_cholesterol,ldl_cholesterol,TG,EPA,DPA,DHA))
## Construct variables required for glithernet package
numLevels <- X %>% sapply(nlevels)
numLevels[numLevels==0] <- 1
i_num <- sapply(X, is.numeric)
idx_num <- (1:length(i_num))[i_num]
idx_cat <- (1:length(i_num))[!i_num]
X$trt <- as.integer(X$trt)-1
X$sex <- as.integer(X$sex)-1

Y.name <- names(Y)
melt.diff <- melt(Y)
melt.diff <- cbind(X$trt,melt.diff)
names(melt.diff)[1] <- 'Treatment'
melt.diff$Treatment <- as.factor(melt.diff$Treatment)

get_lasso_interaction<- function(X, Y, numLevels, response_var='n3index',i_num = i_num){
    set.seed(1234)
    cv_fit <- glinternet.cv(X, unlist(Y[response_var]), numLevels, 
                        interactionCandidates=c(1),numCores=4)
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

getTable <- function(lambda_cv, lambda_idx, i_num){
#    coef(lambda_cv$glinternetFit)[[lambda_idx]]
coef(cv_fit[['n3index.1']]$glinternetFit)[[24]]
}
getTablefromCV <- function(lambda_cv, lambda_idx, i_num){
#    lambda_idx <- which(lambda_cv$lambda == lambda_choice)
    coefs <- coef(lambda_cv$glinternetFit)[[lambda_idx]]
# Get indices for cat. and cont. variables in the list        
    idx_num <- (1:length(i_num))[i_num]
    idx_cat <- (1:length(i_num))[!i_num]
    # construct table for main effect
    # coef names
    t1 = names(numLevels)[idx_cat[coefs$mainEffects$cat]]
    t1 = c(t1, names(numLevels)[idx_num[coefs$mainEffects$cont]])
#    t1.value = unlist(coefs$mainEffectsCoef$cont)
    main_effect <- data.frame(t1)
    names(main_effect) <- 'Main.effect'
    t2 = names(numLevels)[idx_cat[coefs$interactions$catcat[,2]]]
    t2 = c(t2, names(numLevels)[idx_num[coefs$interactions$catcont[,2]]])
    interactions <- data.frame(t2)
    names(interactions) <- 'Interactions'   

return(list(main_effect, interactions))
}

server <- function(input, output, session) {

    datasetInput <- reactive({
        cv_fit[[paste(input$Y.choice,1,sep='.')]]
    })  
    output$distPlot <- renderPlot({
    p <- ggplot(filter(melt.diff,variable==input$Y.choice), aes(x = value,color=Treatment,fill=Treatment)) +
        geom_point(aes(y = 0.1), alpha = 0) + # add an invisible scatterplot geom as the first layer
        geom_density(alpha = 0.5)+
        scale_x_continuous(input$Y.choice) + 
        theme(axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank())

    p %>% ggMarginal(type = "boxplot", 
                        margins = "x", 
                        size = 5,
                        groupColour = TRUE,
                        groupFill = TRUE) 
    })
    output$diagPlot <- renderPlot({
        plot(datasetInput())
    })
    observeEvent(input$Y.choice, {
        dataset <- cv_fit[[paste(input$Y.choice,1,sep='.')]]
        updateSliderInput(
            session,
            inputId = "lambda.choice",
            max = length(cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambda
),
                value = which(cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambda ==
             cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambdaHat1Std))
    })
    output$lambdaSlider <- renderUI({
            sliderInput(inputId = "lambda.choice",
            label = "Penalisation parameter",
                min = 1,
                max = length(cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambda
),
                value = which(cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambda ==
             cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambdaHat1Std))
    })

    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })

    maintab <- reactive({
        temp = getTablefromCV(cv_fit[[paste(input$Y.choice,1,sep='.')]], 
                        input$lambda.choice, i_num)
        temp[[1]]
    })   
    interactiontab <- reactive({
        temp = getTablefromCV(cv_fit[[paste(input$Y.choice,1,sep='.')]], 
                        input$lambda.choice, i_num)
        temp[[2]]
    })       
    output$view <- renderTable({
        maintab()
    })   
    output$view2 <- renderTable({
        interactiontab()
    })
    output$text1 <- renderText({
        paste("N.B. Optimal value is ", 
    which(cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambda ==
             cv_fit[[paste(input$Y.choice,1,sep='.')]]$lambdaHat1Std))
             })
}
