
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(data.table)
library(readxl)
library(ggplot2)
library(agricolae)
library(car)
library(knitr)
library(ggpubr)
library(lattice)

shinyServer(function(input, output) {
  
  
  Rend<-reactive({inFile <- input$file1
  if(is.null(inFile))
    return(NULL)
  file.rename(inFile$datapath,
              paste(inFile$datapath, ".xlsx", sep=""))
  Rend<-read_excel(paste(inFile$datapath, ".xlsx", sep=""), 1)
  })
  
  output$contents <- renderDataTable({
   if (is.null(Rend())){
     return(NULL)}
    else{
    tab1<-Rend()
     tab1[,input$n0]<-lapply(tab1[,input$n0], factor)  
     tab1[,input$n1]<-lapply(tab1[,input$n1], factor)
     tab1[,input$n2]<-lapply(tab1[,input$n2], factor)
     datatable(tab1, rownames = FALSE,options = list(pageLength = 15)) %>% 
       formatStyle(names(tab1[input$n1]), backgroundColor = '#6393fd')%>%
       formatStyle(names(tab1[input$n0]), backgroundColor = '#6393fd')%>%
       formatStyle(names(tab1[input$n2]),backgroundColor = '#91b3fe')%>% 
       formatStyle(names(tab1[input$n3]),backgroundColor = "#ceddfe")%>% 
       formatStyle(names(tab1[input$n8]),backgroundColor = "#ccccce")%>% 
       formatStyle(names(tab1[input$n9]),backgroundColor = "#ceccce")%>% 
       formatString(input$n0)%>%
       formatString(input$n1)%>%
       formatString(input$n2)%>%
       formatRound(1:10,input$n4)%>%
       formatRound(input$n8,0)%>%
       formatRound(input$n9,0)
  }} )
  
  modelo<-reactive({
    dat<-Rend()
    names(dat[input$n1])<-"Tratamiento"
    setnames(dat, input$n0, "Parcela")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n0]<-lapply(dat[,input$n0], factor)
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo<-aov(VarDep ~ Parcela*Tratamiento+Error(Bloque/(Bloque*Parcela)),data=dat)
  } )
  
  modelo2<-reactive({
    dat<-Rend()
    names(dat[input$n1])<-"Tratamiento"
    setnames(dat, input$n0, "Parcela")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n0]<-lapply(dat[,input$n0], factor)
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    modelo2<-aov(VarDep ~ Parcela*Tratamiento+Bloque*Parcela,data=dat)
  })
  
  output$plano <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    if (input$n8==0||input$n9==0){
      return(NULL)}
    else{
      dat<-Rend()
      setnames(dat, input$n8, "row")
      setnames(dat, input$n9, "col")
      setnames(dat, input$n3, "VarDep")
      levelplot(VarDep ~ col*row, data=dat,xlab = "Columnas",ylab = "Filas", main="Heat Map de la Var. Dep en el plano")
      
    }})
  
  
  output$plots <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    tab<-Rend()
    tab[,input$n0]<-lapply(tab[,input$n0], factor)
    tab[,input$n1]<-lapply(tab[,input$n1], factor)
    tab[,input$n2]<-lapply(tab[,input$n2], factor)
    setnames(tab, input$n1, "Tratamiento")
    setnames(tab, input$n0, "Parcela")
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "RS")
    ggplot(tab, aes(x=Tratamiento ,y=RS, colour=Parcela))+geom_boxplot(aes(colour=Parcela))+xlab("Tratamiento") + ylab(input$n5)+theme_bw()+theme(axis.text.x = element_text(angle=90))
    }})
  
  output$plots2 <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      tab<-Rend()
      tab[,input$n0]<-lapply(tab[,input$n0], factor)
      tab[,input$n1]<-lapply(tab[,input$n1], factor)
      tab[,input$n2]<-lapply(tab[,input$n2], factor)
      setnames(tab, input$n1, "Tratamiento")
      setnames(tab, input$n0, "Parcela")
      setnames(tab, input$n2, "Bloque")
      setnames(tab, input$n3, "RS")
      #ggplot(tab, aes(x=Tratamiento ,y=RS, colour=Parcela))+geom_boxplot(aes(colour=Parcela))+xlab("Tratamiento") + ylab(input$n5)+theme_bw()+theme(axis.text.x = element_text(angle=90))
      bp <- ggplot(tab, aes(x=Tratamiento, y=RS, group=Tratamiento)) + 
        geom_boxplot(aes(fill=Tratamiento))+ ylab(input$n5)+ geom_jitter(width = 0, aes(colour=Bloque))+theme_bw()+theme(axis.text.x = element_text(angle=90))
      bp
      bp + facet_grid(Parcela ~ .)
    }})
  
  
  output$plotsblq <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    tab<-Rend()
    tab[,input$n0]<-lapply(tab[,input$n0], factor)
    tab[,input$n1]<-lapply(tab[,input$n1], factor)
    tab[,input$n2]<-lapply(tab[,input$n2], factor)
    names(tab[input$n1])<-"Tratamiento"
    setnames(tab, input$n2, "Parcela")
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "RS")
    labe<-input$n5
  #ggplot(tab, aes(x=Bloque , y=RS , group = Tratamiento, color = Parcela)) + geom_point(data = tab, aes(y = RS)) + geom_line(data = tab, aes(y = RS, group = Tratamiento, color=Parcela))+ xlab("Bloque") +ylab(input$n5)+  theme_bw()+ theme(panel.grid.minor = element_blank(),legend.position="bottom")
  interaction.plot(x.factor = tab$Tratamiento, trace.factor = tab$Parcela, 
                   response = tab$RS, fun = mean, 
                   type = "b", legend = TRUE, 
                   ylab=input$n5,
                   xlab="",
                   trace.label = "Parcela",
                   pch=c(1,19), col = c("#00AFBB", "#E7B800"), las=3, cex.axis=0.7)
  }})
  
  output$plotsblq2 <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      tab<-Rend()
      tab[,input$n0]<-lapply(tab[,input$n0], factor)
      tab[,input$n1]<-lapply(tab[,input$n1], factor)
      tab[,input$n2]<-lapply(tab[,input$n2], factor)
      names(tab[input$n1])<-"Tratamiento"
      setnames(tab, input$n2, "Parcela")
      setnames(tab, input$n2, "Bloque")
      setnames(tab, input$n3, "RS")
      labe<-input$n5
      ggline(tab, x = "Tratamiento", y = "RS", color = "Parcela",
             add = c("mean_se", "dotplot"),
             ylab=input$n5,
             xlab="",
             palette = c("#00AFBB", "#E7B800"))+theme(axis.text.x = element_text(angle=90),legend.position="bottom")
    }})
  
  
  
  output$table <- renderDataTable({
    if (is.null(Rend())){
      Datos<-c("No hay datos cargados!! Carga un archivo xlsx en la pestaña de Datos")
      
      data.table(Datos)
      
      }
    else{
    tab<-Rend()
    tab<-data.table(tab)
    
    setnames(tab, input$n0, "Parcela")
    setnames(tab, input$n1, "Tratamiento")
    setnames(tab, input$n2, "Bloque")
    setnames(tab, input$n3, "VarDep")
    
    tra=colnames(Rend()[,input$n1])
    tab2<-tab[ , lapply(.SD, mean), by=c("Tratamiento"), .SDcols =-c(input$n2, input$n0)]
    d1<-tab2[which(tab$Tratamiento==input$n7)]
    tab2$DifTest<-d1[1,c("VarDep")]
    tab2$DifTest<-tab2$VarDep-tab2$DifTest
    datatable(tab2,rownames = FALSE, options = list(
      dom = 't'))%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$anova <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
    summary(modelo())
  }})
  
  output$LSD <- renderPrint({
    if (is.null(Rend())){
      return( )}
    else{
      ANOVA.Rend<-modelo2()
    LSD.Trat.Rend<-LSD.test(ANOVA.Rend, c("Parcela","Tratamiento"),  alpha = input$n6, console = T, group=TRUE)
  }})
  
  output$plotlsd <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()  
    ANOVA.Rend<-modelo2()
    mx<-max(dat[ , input$n3])
    mx1<-mx/10
    mx2<-mx+mx1
    LSD.Trat.Rend<-LSD.test(ANOVA.Rend, c("Tratamiento","Parcela"), alpha = input$n6, console = F)
    bar.group(x = LSD.Trat.Rend$groups, 
            ylim=c(0,mx2),
            lwd=2,
            main="Prueba de comparación de medias - LSD",
            xlab="",
            ylab=input$n5,
            las=3,
            cex.axis=0.7,
            col="#6393fd")
  }})
  
  output$tabla <- renderDataTable({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    
    setnames(dat, input$n0, "Parcela")
    setnames(dat, input$n1, "Tratamiento")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n0]<-lapply(dat[,input$n0], factor)
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    
    ANOVA.Rend<-modelo2()
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    datatable(dat,rownames = FALSE,options = list(pageLength = 15))%>% formatString(
      input$n2)%>% formatString(
        input$n1)%>% formatRound(1:10,input$n4 ) 
  }})
  
  output$disres <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      ANOVA<-modelo2()
  qqPlot(rstandard(ANOVA), main="Normal Q-Q " )
  
 
  }})
  
  output$hista <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      
      ANOVA.Rend<-modelo2()
      Residuales<-ANOVA.Rend$residuals
      h<-hist(Residuales, 
              col="darkgray")
      xfit<-seq(min(Residuales),max(Residuales),length=40) 
      yfit<-dnorm(xfit,mean=mean(Residuales),sd=sd(Residuales)) 
      yfit <- yfit*diff(h$mids[1:2])*length(Residuales) 
      lines(xfit, yfit, col="blue", lwd=2) 
      
    }})
  
  output$shap <- renderPrint({
    if (is.null(Rend())){
      return(NULL)}
    else{
    
    ANOVA<-modelo2()
  shapiro.test(ANOVA$residuals)
  }})
  
  output$homo <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    ANOVA.Rend<-modelo2()
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    
    ggplot(dat,aes(x=Predichos, y=Residuos))+geom_point(main="Predichos vs Residuos ")+theme_bw()
   
    }})
  
  output$test <- renderPrint({
    if (is.null(Rend())){
      return(NULL)}
    else{
    dat<-Rend()
    names(dat[input$n1])<-"Tratamiento"
    setnames(dat, input$n0, "Parcela")
    setnames(dat, input$n2, "Bloque")
    setnames(dat, input$n3, "VarDep")
    dat[,input$n0]<-lapply(dat[,input$n0], factor)
    dat[,input$n1]<-lapply(dat[,input$n1], factor)
    dat[,input$n2]<-lapply(dat[,input$n2], factor)
    ANOVA.Rend<-modelo2()
    Predichos<-ANOVA.Rend$fitted.values #creamos los predichos
    Residuos<-ANOVA.Rend$residuals
    dat$Predichos<-Predichos
    dat$Residuos<-Residuos
    leveneTest(VarDep ~ Parcela*Tratamiento, data=dat)
     
  }})
  
  output$resi <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      
      ANOVA.Rend<-modelo2()
      Predichos<-fitted(ANOVA.Rend)#creamos los predichos
      Residuos<-residuals(ANOVA.Rend)
      dat<-Rend()
      dat$Predichos<-Predichos
      dat$Residuos<-Residuos
      boxplot(Residuos~ Tratamiento,data=dat, las=3, cex.axis=0.7 ) 
    }})
  
  output$inde <- renderPlot({
    if (is.null(Rend())){
      return(NULL)}
    else{
      
      ANOVA<-modelo2()
      plot(ANOVA$residuals)
    }})
  
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report.doc",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      # Set up parameters to pass to Rmd document
      params <- list(tab = Rend(), i1=input$n1, i2=input$n2, i3=input$n3, i4=input$n4, i5=input$n5, i7=input$n7, i6=input$n6 )
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
})

