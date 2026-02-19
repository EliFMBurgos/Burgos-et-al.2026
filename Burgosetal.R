############ Trap  success ##################
ls()
rm(list=ls())
ls()
setwd()
library(readr)

data<- read_delim("datos.csv", 
                  ";",escape_double = FALSE, trim_ws = TRUE)

library(ggplot2)

ggplot(data, aes(fill=Species, y=TS, x=environm)) + 
  geom_bar(position="stack", stat="identity")+ 
  scale_y_continuous(breaks=c(0,1,2,3,4,5,6,7,8,9,10), limits = c(0,10))+
  ggtitle("Rodent trap success per trap type")+
  xlab("Levels of urbanization")+
  ylab("Rodent trap success (N° capture each 100 traps nights)")+
  theme_classic()+
  theme (legend.title = element_blank(),
         legend.text = element_text(size = 16, colour = "black", face = "italic"),
         legend.position="top")+
  theme(axis.title = element_text(size = 18,
                                  color = "black"),
        axis.text=element_text(size=16, face="bold"),
        plot.title = element_text(size=18, face="bold")) 




##### Statistical analysis ####
ls()
rm(list=ls())
ls()
setwd()
library(readr)
ratas<- read_delim("planilla.csv", 
                   ";",escape_double = FALSE, trim_ws = TRUE)

rata1 <- na.omit(ratas)

escalado<-function(data, a, b)
{
  data$id<-as.numeric(rownames(data)) # asigno un identificador a la base original
  hola<-scale(data[,a:b], center=T, scale=T) # centro las variables
  hola2<-data.frame(hola) # convierto las columnas con las variables centradas en un data frame
  colnames(hola2)<-paste(names(hola2),"c", sep="") # le cambio los nombres a las columnas
  id<-as.numeric(rownames(hola2)) # genero el mismo identificador que a la base original
  hola3<-cbind(hola2, id) # uno el identificador con las columnas centradas
  basenueva<-merge(data, hola3, by="id") # junto ambos data frames a través del id que generé
  return(basenueva)
}

colnames(rata1)
data1<-escalado(rata1, 30,72)
View(data1)
colnames(data1)

###MODELOS###

library(lme4)

library(MuMIn)
library(multcomp)
library(car)
library(AICcmodavg)
library(lsmeans) 
library(multcomp)
library(cowplot)
data1$Rr <- as.numeric (data1$Rr)
colnames(data1)

M0<-glm.nb(Rr~1,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M0)
AIC(M0)

M11<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos+pres_basura_chatarra+Altpastocmc+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
confint(M11, level=0.90)
summary(M11)

windows()
par(mfrow=c(1,2),cex.lab=1.5, cex.axis=1.3) 
resid=resid(M11,type="pearson")
plot(fitted(M11), resid, ylab="Residuales", xlab="Predichos",main=""); abline(h=0)
qqnorm(resid, ylab="Cuant. teoricos", xlab="Cuant Obs resid",main="")
qqline(resid,lwd=1.5, col="red")

((M11$null.deviance-M11$deviance)/M11$null.deviance)*100# 


int1<-glht(M11, linfct = mcp(season="Tukey"))
summary(int1)


int2<-glht(M11, linfct = mcp(pres_galpon="Tukey"))
summary(int2)

int3<-glht(M11, linfct = mcp(pres_cultivos="Tukey"))
summary(int3)


int4<-glht(M11, linfct = mcp(pres_basura_chatarra="Tukey"))
summary(int4)

library(variancePartition)

calcVarPart(M11)

 
library(ggplot2)
library(ggeffects)
M11SER<-glm.nb(Rr~1+season+PRECIPACUM_90+pres_galpon+pres_cultivos+pres_basura_chatarra+Altpastocm+tempMax_diaria,data=data1, na.action=na.omit,offset(log(tnoches05)))

pred.PA90<-ggpredict(M11SER, terms = "PRECIPACUM_90[all]")
pred.AltP<-ggpredict(M11SER, terms = "Altpastocm[all]")
pred.TMdiaria<-ggpredict(M11SER, terms = "tempMax_diaria[all]")

pred.S<-ggpredict(M11SE, terms = "season")
pred.PG<-ggpredict(M11SE, terms = "pres_galpon")
pred.C <- ggpredict(M11SE, terms = "pres_cultivos")
pred.bas <- ggpredict(M11SE, terms = "pres_basura_chatarra")

rr_PACUM <- plot(pred.PA90)+labs(x="90-days cumulative precipitacion (mm)", y="Rr Trap success")+ theme_classic()+
  theme(plot.title=element_blank(), legend.position = c(0.1,0.9),
        axis.title=element_text(size=rel(1)),axis.text= element_text(size = rel(1)))

rr_AltP <- plot(pred.AltP)+labs(x="Grass height (cm)", y="Rr trap success")+ theme_classic()+ylim(0,2.00)+
  theme(plot.title=element_blank(), legend.position = c(0.1,0.9),
        axis.title=element_text(size=rel(1)),axis.text= element_text(size = rel(1)))

rr_TMdiaria <- plot(pred.TMdiaria)+labs(x="Daily maximum temperature (°C)", y="Rr trap success")+ theme_classic()+ylim(0,2.00)+
  theme(plot.title=element_blank(), legend.position = c(0.1,0.9),
        axis.title=element_text(size=rel(1)),axis.text= element_text(size = rel(1)))



###############################################################################
### MUS MUSCULUS ####
data1$Mm <- as.integer(data1$Mm)
colnames(data1)


class(data1$VivTipo)
data1$VivTipo <- as.factor(data1$VivTipo)
M0<-glm.nb(Mm~1,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M0)
AIC(M0)


M11SE_M<-glm.nb(Mm~1+PRECIPACUM_60+Year+VivTipo+season+pres_cultivos+ dist_ANP-1,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M11SE_M)


library(variancePartition)
calcVarPart(M11SE_M)


summary(M11SE)
library(multcomp)
int5<-glht(M11, linfct = mcp(VivTipo="Tukey"))
summary(int5)
int6<-glht(M11, linfct = mcp(Year="Tukey"))
summary(int6)

int7<-glht(M11, linfct = mcp(season="Tukey"))
summary(int7)

int8<-glht(M11, linfct = mcp(pres_cultivos="Tukey"))
summary(int8)



AIC(M11)
((M11$null.deviance-M11$deviance)/M11$null.deviance)*100# 56.3466
confint.lm (M11, level=0.90)


windows()
par(mfrow=c(1,2),cex.lab=1.5, cex.axis=1.3) 
resid=resid(M11,type="pearson")
plot(fitted(M11), resid, ylab="Residuales", xlab="Predichos",main=""); abline(h=0)
qqnorm(resid, ylab="Cuant. teoricos", xlab="Cuant Obs resid",main="")
qqline(resid,lwd=1.5, col="red")



#variables

library(ggeffects)

pred.PA60<-ggpredict(M11SE_M, terms = "PRECIPACUM_60[all]")
pred.distANP<-ggpredict(M11SE_M, terms = "dist_ANP[all]")


library(ggplot2)
mm_PACUM <- plot(pred.PA60)+labs(x="60-days cumulative precipitation (mm)", y="Mm Trap success")+ 
  theme_classic()+ ylim(0,0.5)+
  theme(plot.title=element_blank(), legend.position = c(0.1,0.9),
        axis.title=element_text(size=rel(1)),axis.text= element_text(size = rel(1)))

mm_distANP <- plot(pred.distANP)+labs(x="Distance to natural protected areas (m)", y="")+ theme_classic()+ylim(0,0.5)+
  theme(plot.title=element_blank(), legend.position = c(0.1,0.9),
        axis.title=element_text(size=rel(1)),axis.text= element_text(size = rel(1)))



plot_grid(rr_PACUM, rr_TMdiaria, rr_AltP,mm_PACUM, ncol=2, labels="auto")



