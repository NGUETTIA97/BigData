#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(Amelia)
library(plotly)
library(ggplot2)
library(questionr)
library(RcmdrMisc)
library(ROCR)
library(Tplyr)
library(glmnet)
library(missMDA)
library(pROC)
library(tree)
library(MASS)
library(randomForest)
library(gbm)


#setwd("C:/Users/Elisa/Desktop/BigDataAnalytics/BigData")



source("test.R",encoding = "UTF-8",echo=TRUE) 
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$RawData <- DT::renderDataTable(
    DT::datatable({
      data=read_excel("kaggle.xlsx")
      data=as.data.frame(data)
      data
      
    },
    options = list(pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE
    # colnames = c("Retard","Solde_Rev", "Age", "N_dep30_50", "P_dette","Rev_mois", "N_pret_ouvert","N_dep90","N_hypo_immo", "N_dep60_89", "P_charge")
    ))
  output$missvalue <- renderPlot({
    data=read_excel("kaggle.xlsx", col_types = "numeric")
    data=as.data.frame(data)
    datab=data1(data)
    
    missmap(data, main = "Missing values vs observed")      
  })
  
  
  
  variables <- reactive({
    data=read_excel("kaggle.xlsx")
    datab=data1(data)
    
    switch(input$var,
           "Retard"=datab$Retard, "Solde_Rev"=datab$Solde_Rev, "Age"=datab$Age,"N_dep30_50"=datab$N_dep30_50 ,"P_dette"=datab$P_dette,
           "Rev_mois"=datab$Rev_mois,"N_pret_ouvert"=datab$N_pret_ouvert,"N_dep90"=datab$N_dep90,"N_hypo_immo"=datab$N_hypo_immo,"N_dep60_89"=datab$N_dep60_89,"P_charge"=datab$P_charge)
  }) 
  
  output$quanti <- renderPlotly({
    #Imputation des valeurs manquantes 
    
    plot_ly(x =variables(), type = input$graph)
    
  })
  
  var_quali <- reactive({
    data=read_excel("kaggle.xlsx")
    data=as.data.frame(data)
    datab=data1(data)
    switch(input$variable_quali,
           "Retard"=datab$Retard)
    
  })
  
  
  
  output$quali <- renderPlotly({
    
    piee= freq(var_quali())
    piee1=data.frame(piee, val=row.names(piee))
    plot_ly(piee1,y = piee1$val,x= piee1$n, type = "bar")
    
    #  g=as.vector(input$graphique_quali)
    
    # d=ifelse((g=="Circulaire"),plot_ly(piee1,labels = piee1$val, values = piee1$n, type = "pie"),)
    
  })
  
  variables_bi <- reactive({
    data=read_excel("kaggle.xlsx")
    data=as.data.frame(data)
    datab=data1(data)
    data1=datab[datab$Retard == 1,]
    
    switch(input$bivar,
           "Solde_Rev"=data1$Solde_Rev, "Age"=data1$Age,"N_dep30_50"=data1$N_dep30_50 ,"P_dette"=data1$P_dette,
           "Rev_mois"=data1$Rev_mois,"N_pret_ouvert"=data1$N_pret_ouvert,"N_dep90"=data1$N_dep90,"N_hypo_immo"=data1$N_hypo_immo,"N_dep60_89"=data1$N_dep60_89,"P_charge"=data1$P_charge)
  })
  variables_bi1 <- reactive({
    data=read_excel("kaggle.xlsx")
    data=as.data.frame(data)
    datab=data1(data)
    data0=datab[datab$Retard == 0,]   
    
    switch(input$bivar,
           "Solde_Rev"=data0$Solde_Rev, "Age"=data0$Age,"N_dep30_50"=data0$N_dep30_50 ,"P_dette"=data0$P_dette,
           "Rev_mois"=data0$Rev_mois,"N_pret_ouvert"=data0$N_pret_ouvert,"N_dep90"=data0$N_dep90,"N_hypo_immo"=data0$N_hypo_immo,"N_dep60_89"=data0$N_dep60_89,"P_charge"=data0$P_charge)
  }) 
  
  output$biv <- renderPlotly({
    data1=data[data$Retard== 1,]
    data0=data[data$Retard== 0,]
    p_box <-
      plot_ly(data = data1, x = ~ variables_bi(), type =input$graph_var, name = "Pas de crédit") %>%
      add_trace(data = data0,x = ~variables_bi1(), name = "Crédit")
    p_box
    
  })
  
  
  modele3 <- reactive({ 
    data=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=data1(data)
    datab=data.frame(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(datab),2000)
    Xtrain=as.matrix(datab[train,-1])
    Ytrain=as.matrix(datab[train,1])
    if(input$CV==1)
    {
      set.seed(2)
      cv.ridge2 <- cv.glmnet(Xtrain,Ytrain,family="binomial",type.measure="class" ,nfolds=5,alpha=0)
      ridge= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=0,lambda =cv.ridge2$lambda.min)
    }
    else
    {
      ridge= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=0,lambda =input$lamda)
    }
    ridge
  })
  
  output$resume_ridge <- renderPrint({
    coef(modele3())
  })
  
  output$courbe_ridge <- renderPlot({
    data=read_excel("kaggle.xlsx", col_types="numeric")
    datab=data1(data)
    datab=data.frame(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(datab),2000)
    x.test <- model.matrix(Retard ~., datab[-train,])[,-1]
    probabilities <- modele3() %>% predict(newx = x.test,type="response")
    roc_ridge=prediction( probabilities,datab[-train,]$Retard)
    perf_ridge=performance(roc_ridge,"tpr", "fpr")
    #plot(perf_ridge)
    #abline(a=0, b= 1)
    roc_ridge=roc(datab[-train,]$Retard,as.vector(probabilities),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    
  })
  
  auc_model3 <- reactive({
    data=read_excel("kaggle.xlsx", col_types="numeric")
    datab=data1(data)
    datab=data.frame(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(datab),2000)
    x.test <- model.matrix(Retard ~., datab[-train,])[,-1]
    probabilities <- modele3() %>% predict(newx = x.test,type="response")
    roc_ridge=prediction( probabilities,datab[-train,]$Retard)
    perf_ridge=performance(roc_ridge,"tpr", "fpr")
    p1= modele3() %>% predict(newx = x.test,type="class")
    tx_err=as.vector(mean(as.numeric(p1)!=datab[-train,]$Retard))
    roc3=  roc(datab[-train,]$Retard,as.vector(probabilities),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    rr=prediction(as.vector(probabilities),datab[-train,]$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    auc=as.vector(roc3$auc)
    gini=2*auc -1
    final_auc3=c(auc,tx_err,gini,ks)
    final_auc3
    
  })
  
  modele4 <- reactive({ 
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    data=data.frame(data)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    Xtrain=as.matrix(data[train,-1])
    Ytrain=as.matrix(data[train,1])
    if(input$CV_lasso==1)
    {
      set.seed(123)
      cv.lasso <- cv.glmnet(Xtrain,Ytrain,family="binomial",type.measure="class" ,nfolds=5,alpha=1)
      lasso= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=1,lambda =cv.lasso$lambda.min)
    }
    else
    {
      lasso= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=1,lambda =input$lamda_lasso)
    }
    lasso
  })
  output$resume_lasso <- renderPrint({
    coef(modele4())
  })
  
  output$courbe_lasso <-renderPlot({
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    x.test_lasso <- model.matrix(Retard ~., data[-train,])[,-1]
    probabilities_lasso <- modele4() %>% predict(newx = x.test_lasso,type="response")
    roc_lasso=prediction( probabilities_lasso,data[-train,]$Retard)
    perf_lasso=performance(roc_lasso,"tpr", "fpr")
    # plot(perf_lasso)
    #abline(a=0, b= 1)
    roc_lasso=roc(data[-train,]$Retard,as.vector(probabilities_lasso),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    
  })
  
  auc_model4 <- reactive({
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    x.test_lasso <- model.matrix(Retard ~., data[-train,])[,-1]
    probabilities_lasso <- modele4() %>% predict(newx = x.test_lasso,type="response")
    roc_lasso=prediction( probabilities_lasso,data[-train,]$Retard)
    p3= modele4() %>% predict(newx = x.test_lasso,type="class")
    rr=prediction(as.vector(probabilities_lasso),data[-train,]$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    tx_err=as.vector(mean(as.numeric(p3)!=data[-train,]$Retard))
    roc4=  roc(datab[-train,]$Retard,as.vector(probabilities_lasso),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    auc=as.vector(roc4$auc)
    
    gini=2*auc -1
    final_auc4=c(auc,tx_err,gini,ks)
    final_auc4
  })
  
  
  modele5 <- reactive({ 
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    data=data.frame(data)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    Xtrain=as.matrix(data[train,-1])
    Ytrain=as.matrix(data[train,1])
    if(input$CV_en==1)
    {
      set.seed(123)
      cv.en <- cv.glmnet(Xtrain,Ytrain,family="binomial",type.measure="class" ,nfolds=5,alpha=0.5)
      en= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=0.5,lambda =cv.en$lambda.min)
    }
    else
    {
      en= glmnet(Xtrain,Ytrain,family="binomial",standardize=FALSE,alpha=1,lambda =input$lamda_en)
    }
    en
  })
  output$resume_en <- renderPrint({
    coef(modele5())
  })
  
  output$courbe_en <- renderPlot({
    datab=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    x.test_en <- model.matrix(Retard ~., data[-train,])[,-1]
    probabilities_en <- modele5() %>% predict(newx = x.test_en)
    roc_en=prediction( probabilities_en,data[-train,]$Retard)
    perf_en=performance(roc_en,"tpr", "fpr")
    #plot(perf_en)
    #abline(a=0, b= 1)
    roc_en=roc(data[-train,]$Retard,as.vector(probabilities_en),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    
  })
  
  
  auc_model5 <- reactive({
    datab=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),105000)
    x.test_en <- model.matrix(Retard ~., data[-train,])[,-1]
    probabilities_en <- modele5() %>% predict(newx = x.test_en)
    p4= modele5() %>% predict(newx = x.test_en,type="class")
    tx_err=as.vector(mean(as.numeric(p4)!=data[-train,]$Retard))
    roc5=roc(datab[-train,]$Retard,as.vector( probabilities_en),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    rr=prediction(as.vector(probabilities_en),data[-train,]$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    auc=as.vector(roc5$auc)
    gini=2*auc -1
    final_auc5=c(auc,tx_err,gini,ks)
    final_auc5
  })
  
  
  modele7 <- reactive({ 
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    if(input$CV_rf==1)
    {
      mtr=1:11
      tx_mtry=vector()
      for(i in mtr){
        set.seed(123456)
        rf.fit=randomForest(factor(Retard)~.,data=data[train,],importance=TRUE, mtry=i)
        rf.pred=predict(rf.fit, newdata=data[-train,])
        tx_mtry[i]=mean(rf.pred!=data[-train,]$Retard)
      }
      rf.fit=randomForest(factor(Retard)~.,data=data[train,],importance=TRUE, mtry=mtr[which.min(tx_mtry)])
    }
    else
    {
      rf.fit=randomForest(factor(Retard)~.,data=data[train,],importance=TRUE, mtry=input$mt)
    }
    rf.fit
  })
  output$courbe_rf <- renderPlot({
    datab=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    rf.probs=predict(modele7(), newdata=data[-train, ], type="prob")
    roc(data[-train,]$Retard,as.vector(rf.probs[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
  })
  
  
  auc_model7 <- reactive({
    datab=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),2000)
    rf.probs=predict(modele7(), newdata=data[-train, ], type="prob")
    rf=predict(modele7(), newdata=data[-train, ],type="class")
    tx_err=as.vector(mean(rf!=data[-train,]$Retard))
    roc6=  roc(datab[-train,]$Retard,as.vector(rf.probs[,2]),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    rr=prediction(as.vector(rf.probs[,2]),data[-train,]$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    auc=as.vector(roc6$auc)
    gini=2*auc -1
    final_auc7=c(auc,tx_err,gini,ks)
    final_auc7
  })
  
  
  output$courbe_boost <- renderPlot({
    datab=read_excel("kaggle.xlsx", col_types = "numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),500)
    boost.fit=gbm(Retard~.,data=data[train,],distribution="bernoulli",n.trees=1000,interaction=2)
    
    boost.probs=predict(boost.fit,newdata=data[-train,],n.trees=1000,type="response")
    
    roc(data[-train,]$Retard,as.vector( boost.probs),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    
  })
  
  auc_model11 <- reactive({
    datab=read_excel("kaggle.xlsx", col_types="numeric")
    datab=as.data.frame(datab)
    data=data1(datab)
    datab$Retard=as.factor(datab$Retard)
    set.seed(2)
    train=sample(1:nrow(data),500)
    boost.fit=gbm(Retard~.,data=data[train,],distribution="bernoulli",n.trees=1000,interaction=2)
    
    boost.probs=predict(boost.fit,newdata=data[-train,],n.trees=1000,type="response")
    boost.pred=rep(0,nrow(data[-train,]))
    boost.pred[boost.probs>0.5]=1
    table(boost.pred,data[-train,]$Retard)
    tx_err=mean(boost.pred!=data[-train,]$Retard)
    
    roc11=roc(data[-train,]$Retard,as.vector( boost.probs),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    rr=prediction(as.vector(boost.probs),data[-train,]$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    auc=as.vector(roc11$auc)
    gini=2*auc -1
    final_auc11=c(auc,tx_err,gini,ks)
    final_auc11
  })
  
  
  
  
  modele11<-reactive({
    data2=modele10()
    trai=sample(1:nrow(data2),105000)
    train=data2[trai,]
    test=data2[-trai,]
    
    
    
    x=train[,-1]
    x=as.matrix(x)
    y=as.matrix(train[,1])
    
    lasso_model=cv.glmnet(x,y,family="binomial", alpha=1, type.measure = "class", nfolds=5, keep=TRUE)
    lasso_model
    
  })
  
  
  
  
  output$stat<-renderPrint({
    
    
    
    coef(modele11(), s="lambda.min")
    
  })
  
  output$taux<-renderPlot({
    
    
    plot( modele11())
    
  })
  
  output$roc<-renderPlot({
    data2=modele10()
    trai=sample(1:nrow(data2),105000)
    train=data2[trai,]
    test=data2[-trai,]
    
    
    
    x=train[,-1]
    x=as.matrix(x)
    y=as.matrix(train[,1])
    
    min=modele11()$lambda.min
    
    lasso_model=glmnet(x,y,family="binomial", alpha=1, type.measure = "class", keep=TRUE, lambda=min)
    xtest=test[,-1]
    xtest=as.matrix(xtest)
    lasso.probs=predict(lasso_model,xtest,type="response")
    roc(test$Retard,as.vector(lasso.probs),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    
  })
  
  output$conf<-renderPrint({
    data2=modele10()
    trai=sample(1:nrow(data2),105000)
    train=data2[trai,]
    test=data2[-trai,]
    
    
    
    x=train[,-1]
    x=as.matrix(x)
    y=as.matrix(train[,1])
    modele11()
    lasso_model=cv.glmnet(x,y,family="binomial", alpha=1, type.measure = "class", nfolds=5, keep=TRUE)
    min=modele11()$lambda.min
    
    lasso_model=glmnet(x,y,family="binomial", alpha=1, type.measure = "class", keep=TRUE, lambda=min)
    xtest=test[,-1]
    xtest=as.matrix(xtest)
    lasso.probs=predict(lasso_model,xtest,type="response")
    lasso.pred=rep(0,nrow(xtest))
    lasso.pred[lasso.probs>input$lambda]=1
    lasso.confusion=table(lasso.pred,test$Retard)
    lasso.confusion
    
  })
  
  auc_model8 <- reactive({
    data2=modele10()
    trai=sample(1:nrow(data2),105000)
    train=data2[trai,]
    test=data2[-trai,]
    
    
    
    x=train[,-1]
    x=as.matrix(x)
    y=as.matrix(train[,1])
    
    min=modele11()$lambda.min
    
    lasso_model=glmnet(x,y,family="binomial", alpha=1, type.measure = "class", keep=TRUE, lambda=min)
    xtest=test[,-1]
    xtest=as.matrix(xtest)
    lasso.probs=predict(lasso_model,xtest,type="response")
    roc(test$Retard,as.vector(lasso.probs),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    lasso.proba=predict(lasso_model,xtest,type="class")
    tx_err=as.vector(mean(as.numeric(lasso.proba)!=test$Retard))
    roc7=  roc(test$Retard,as.vector(lasso.probs),plot=TRUE,legacy.axes=TRUE, lwd=2, col="blue",print.auc=TRUE,grid=TRUE)
    rr=prediction(as.vector(lasso.probs),test$Retard)
    sensispe=performance(rr,"sens", "spec")
    ks=as.vector(ks.test(unlist(sensispe@x.values),unlist(sensispe@y.values))$statistic)
    auc=as.vector(roc7$auc)
    gini=2*auc -1
    final_auc8=c(auc,tx_err,gini,ks)
    final_auc8
  })
  output$Conclusion <-renderTable({
    
    auc_t=round( c(auc_model3()[1],auc_model4()[1],auc_model5()[1],auc_model7()[1],auc_model11()[1]),digits = 3)
    erreur=round(1 -  c(auc_model3()[2],auc_model4()[2],auc_model5()[2],auc_model7()[2],auc_model11()[2]),digits = 3)
    gini=round(c(auc_model3()[3],auc_model4()[3],auc_model5()[3],auc_model7()[3],auc_model11()[3]),digits = 3)
    ks=round(c(auc_model3()[4],auc_model4()[4],auc_model5()[4],auc_model7()[4],auc_model11()[4]),digits = 3)
    conclu=cbind(auc_t,erreur,gini,ks)
    colnames(conclu)=c("AUC","PCC",'PGI',"KS")
    Methodes=c("Regression Ridge", "Regression Lasso ","Elastic Net","RandomForest","Boosting")
    total=cbind(Methodes,conclu)
    print(total)
  })
  
  output$sortie<-renderText({
    auc_t=round( c(auc_model3()[1],auc_model4()[1],auc_model5()[1],auc_model7()[1]),digits = 3)
    erreur=round(1 -  c(auc_model3()[2],auc_model4()[2],auc_model5()[2],auc_model7()[2]),digits = 3)
    gini=round(c(auc_model3()[3],auc_model4()[3],auc_model5()[3],auc_model7()[3],auc_model11()[3]),digits = 3)
    conclu=cbind(auc_t,erreur,gini)
    colnames(conclu)=c("AUC","PCC",'PGI')
    Methodes=c("Regression Ridge", "Regression Lasso ","Elastic Net","RandomForest","Boosting")
    total=cbind(Methodes,conclu)
    s=total[which.max(total[,2]),1]
    uc=total[total[,1]==s,2]
    pc=total[total[,1]==s,3]
    pg=total[total[,1]==s,4]
    paste("Au regard des résultats du tableau on conclut que la méthode", s ,"est la meilleure car elle a une valeur plus élevé de l'AUC (",
          uc,") , PCC (",pc,") et de PGI (",pg ,") plus élevés que celle des autres methodes
        "
    )
  })
  
  
  
  
  
})
