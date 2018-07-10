library(lattice)
levelplot(z ~ Column*Row, data=ECR)
#------
dat <- data.frame("year"= c(2000:2005),"X1"=runif(6,-3,3),"X2"=runif(6,-3,3),"X3"=runif(6,-3,3),"X4"=runif(6,-3,3),"X5"=runif(6,-3,3),"X6"=runif(6,-3,3),"X7"=runif(6,-3,3),"X8"=runif(6,-3,3),"X9"=runif(6,-3,3),"X10"=runif(6,-3,3),"X11"=runif(6,-3,3),"X12"=runif(6,-3,3))
newd <- data.frame("year"=c(2000:2005), "val"=c(40,45,50,35,40,55), "sd"=c(5,6,8,4,5,9))
idx <- c(1:12)
dat2 <- expand.grid(y=dat[,1], x=idx)
dat2$z <- as.vector(as.matrix(dat[,-1]))
levelplot(z ~ y*x, data=dat2)

p1 = levelplot(z ~ y*x, data=dat2) 
p2 = lattice::xyplot(val ~ year, data=newd, type="l") 
latticeExtra::doubleYScale(p1, p2, add.axis=TRUE) 

p2 = lattice::xyplot(val ~ year, data=newd, type="l", lwd=3, ylim=c(30, 60)) + lattice::xyplot(I(val+sd) ~ year, data=newd, type="l") + lattice::xyplot(I(val-sd) ~ year, data=newd, type="l")
latticeExtra::doubleYScale(p1, p2, add.axis=TRUE) 
#-------

library(readxl)
ECR <- read_excel("C:/Users/pasqu/Dropbox/V. Amelia - Ensayos/2017-2018/ECR - Maiz Tardio/ECR-densidad.xlsx", 
                  col_types = c("blank", "blank", "text", 
                                "text", "text", "numeric", "numeric", 
                                "numeric"))

ECR[,"Rep."]<-lapply(ECR[,"Rep."], factor)
ECR[,"Densidad"]<-lapply(ECR[,"Densidad"], factor)
ECR[,"Hibrido"]<-lapply(ECR[,"Hibrido"], factor)

modelo.Rend<-aov(Rinde ~ Densidad*Hibrido+Error(Rep./(Rep.*Densidad)),data=ECR)
modelo.Rend<-aov(Rinde ~ Densidad*Hibrido,data=ECR)
modelo.Rend$Within$residuals
ANOVA.Rend<-modelo.Rend
summary(ANOVA.Rend)
LSD.Trat.Rend<-LSD.test(modelo.Rend, c("Parcela", "Hibrido"),  alpha = 0.01, console = T )
ANOVA.Rend
LSD.Trat.Rend<-LSD.test(ANOVA.Rend, c("Hibrido"), alpha = 0.01, console = F)
bar.group(x = LSD.Trat.Rend$groups, 
          lwd=2,
          main="Prueba de comparación de medias - LSD",
          xlab="",
          las=2,
          cex.axis=0.7,
          col="#6393fd")

attach(ECR)
interaction.plot(Hibrido,Densidad,Rinde)  #another way to graph the means 
detach(ECR)

interaction.plot(x.factor = ECR$Hibrido, trace.factor = ECR$Densidad, 
                 response = ECR$Rinde, fun = mean, 
                 type = "b", legend = TRUE,
                 xlab="",
                 ylab="",
                 trace.label = "tobacco",
                 pch=c(1,19), col = c("#00AFBB", "#E7B800"), las=3,cex.axis=0.8)+theme(axis.text.x = element_text(angle=90))

ggline(ECR, x = "Hibrido", y = "Rinde", color = "Densidad",
       add = c("mean_se", "dotplot"),
       palette = c("#00AFBB", "#E7B800"), ylab="Mmm", xlab="mmm",las=3)+theme(axis.text.x = element_text(angle=90),legend.position="bottom")

bp <- ggplot(ECR, aes(x=Hibrido, y=Rinde, group=Hibrido)) + 
  geom_boxplot(aes(fill=Hibrido))+ geom_jitter(width = 0, aes(colour=Rep.))
bp
bp + facet_grid(Densidad ~ .)
bp + facet_grid(Densidad ~ Hibrido, margins=TRUE)

