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

add1.test<- add1(M0,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M1<-glm.nb(Rr~1+VivPiso,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M1)
AIC(M1)


add1.test<- add1(M1,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)
M2<-glm.nb(Rr~1+VivPiso+Altpastocmc,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M2)
AIC(M2)
add1.test<- add1(M2,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M3<-glm.nb(Rr~1+VivPiso+Altpastocmc+pres_galpon,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M3)
AIC(M3)

add1.test<- add1(M3,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M4<-glm.nb(Rr~1+VivPiso+Altpastocmc+pres_galpon+PRECIPACUM_90c,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M4)
AIC(M4)

add1.test<- add1(M4,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M5<-glm.nb(Rr~1+VivPiso+Altpastocmc+pres_galpon+PRECIPACUM_90c+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))

summary(M5)
AIC(M4)


M5<-glm.nb(Rr~1+season,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M5)#
AIC(M5)

add1.test<- add1(M5,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M6<-glm.nb(Rr~1+season+PRECIPACUM_90c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M6)
AIC(M6)

add1.test<- add1(M6,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M7<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M7)
AIC(M7)

add1.test<- add1(M7,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)
M8<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M8)
AIC(M8)

add1.test<- add1(M8,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M9<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos+pres_basura_chatarra,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M9)#
AIC(M9)

add1.test<- add1(M9,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M10<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos+pres_basura_chatarra+Altpastocmc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M10)#
AIC(M10)

add1.test<- add1(M10,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M11<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos+pres_basura_chatarra+Altpastocmc+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M11)#
AIC(M11)

add1.test<- add1(M11,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M12<-glm.nb(Rr~1+pres_galpon,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M12)#
AIC(M12)
add1.test<- add1(M12,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M13<-glm.nb(Rr~1+pres_galpon+PRECIPACUM_90c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M13)#
AIC(M13)
add1.test<- add1(M13,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M14<-glm.nb(Rr~1+pres_galpon+PRECIPACUM_90c+TempMedia_semanac,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M14)#
AIC(M14)

add1.test<- add1(M14,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M15<-glm.nb(Rr~1+pres_galpon+PRECIPACUM_90c+TempMedia_semanac+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M15)#
AIC(M15)

add1.test<- add1(M15,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M16<-glm.nb(Rr~1+pres_galpon+PRECIPACUM_90c+TempMedia_semanac+pres_cultivos+pres_basura_chatarra,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M16)#
AIC(M16)

add1.test<- add1(M16,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M17<-glm.nb(Rr~1+pres_galpon+PRECIPACUM_90c+TempMedia_semanac+pres_cultivos+pres_basura_chatarra+NDVI150c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M17)#
AIC(M17)


M18<-glm.nb(Rr~1+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M18)#
AIC(M18)

add1.test<- add1(M18,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M19<-glm.nb(Rr~1+tempMax_diariac+PRECIPACUM_90c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M19)#
AIC(M19)

add1.test<- add1(M19,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M20<-glm.nb(Rr~1+tempMax_diariac+PRECIPACUM_90c+season,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M20)#
AIC(M20)

add1.test<- add1(M20,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M21<-glm.nb(Rr~1+tempMax_diariac+PRECIPACUM_90c+season+NDVI100c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M21)#
AIC(M21)

add1.test<- add1(M21,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M22<-glm.nb(Rr~1+tempMax_diariac+PRECIPACUM_90c+season+NDVI100c+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M22)#
AIC(M22)

M23<-glm.nb(Rr~1+Altpastocmc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M23)#
AIC(M23)

add1.test<- add1(M23,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M24<-glm.nb(Rr~1+Altpastocmc+pres_galpon,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M24)#
AIC(M24)

add1.test<- add1(M24,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M25<-glm.nb(Rr~1+Altpastocmc+pres_galpon+ONI_PROM_TRIMc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M25)#
AIC(M25)

add1.test<- add1(M25,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M26<-glm.nb(Rr~1+Altpastocmc+pres_galpon+ONI_PROM_TRIMc+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M26)#
AIC(M26)

add1.test<- add1(M26,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)
M27<-glm.nb(Rr~1+Altpastocmc+pres_galpon+ONI_PROM_TRIMc+pres_cultivos+season,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M27)#
AIC(M27)

add1.test<- add1(M27,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M28<-glm.nb(Rr~1+Altpastocmc+pres_galpon+ONI_PROM_TRIMc+pres_cultivos+season+Year,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M28)#
AIC(M28)

add1.test<- add1(M28,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M29<-glm.nb(Rr~1+dist_basuralesc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M29)#
AIC(M29)

add1.test<- add1(M29,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M30<-glm.nb(Rr~1+dist_basuralesc+VivPiso,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M30)#
AIC(M30)

add1.test<- add1(M30,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M31<-glm.nb(Rr~1+dist_basuralesc+VivPiso+Altpastocmc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M31)#
AIC(M31)

add1.test<- add1(M31,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M32<-glm.nb(Rr~1+dist_basuralesc+VivPiso+Altpastocmc+PRECIPACUM_90c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M32)#
AIC(M32)

add1.test<- add1(M32,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M33<-glm.nb(Rr~1+dist_basuralesc+VivPiso+Altpastocmc+PRECIPACUM_90c+pres_galpon,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M33)

M33<-glm.nb(Rr~1+dist_basuralesc+VivPiso+Altpastocmc+PRECIPACUM_90c+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M33)

M33<-glm.nb(Rr~1+dist_basuralesc+VivPiso+Altpastocmc+PRECIPACUM_90c+season,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M33)
AIC(M33)# 


M34<-glm.nb(Rr~1+PRECIPACUM_30c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M34)#
AIC(M34)#

add1.test<- add1(M34,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M35<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M35)#
AIC(M35)#  

add1.test<- add1(M35,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M36<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso+Altpastocmc,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M36)

M36<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M36)

cor.test(data1$PRECIPACUM_30, data1$MINTEM90)#correlacionadas significativamente 
M36<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso+pres_cultivos,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M36)
AIC(M36)

add1.test<- add1(M36,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M37<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso+pres_cultivos+NDVI50c,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M37)
AIC(M37)


add1.test<- add1(M37,scope=Rr~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi


M38<-glm.nb(Rr~1+PRECIPACUM_30c+VivPiso+pres_cultivos+NDVI50c+Year,data=data1, na.action=na.omit,offset(log(tnoches05)))
summary(M38)
AIC(M38)

#Seleccion de modelos

mod_list1 <- list(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16,M17,M18,M19,M20,M21,M22,M23,M24,M25,M26,M27,M28,M29,M30,M31,M32,M33,M34,M35,M36,M37,M38)
mod_names1<- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23","M24","M25","M26","M27","M28","M29","M30","M31","M32","M33","M34","M35","M36","M37","M38")

aictab(mod_list1,modnames = mod_names1, second.ord = F) 
summary(M11)
windows()
par(mfrow=c(1,2),cex.lab=1.5, cex.axis=1.3) 
resid=resid(M11,type="pearson")
plot(fitted(M11), resid, ylab="Residuales", xlab="Predichos",main=""); abline(h=0)
qqnorm(resid, ylab="Cuant. teoricos", xlab="Cuant Obs resid",main="")
qqline(resid,lwd=1.5, col="red")

((M11$null.deviance-M11$deviance)/M11$null.deviance)*100# 

M11<-glm.nb(Rr~1+season+PRECIPACUM_90c+pres_galpon+pres_cultivos+pres_basura_chatarra+Altpastocmc+tempMax_diariac,data=data1, na.action=na.omit,offset(log(tnoches05)))
confint(M11, level=0.90)
summary(M11)


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

add1.test<- add1(M0,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M1<-glm.nb(Mm~1+MINTEM90c,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M1)
AIC(M1)

add1.test<- add1(M1,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M2<-glm.nb(Mm~1+MINTEM90c+pres_galpon ,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M2)
AIC(M2)

add1.test<- add1(M2,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M3<-glm.nb(Mm~1+MINTEM90c+pres_galpon +season ,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M3)
AIC(M3)


add1.test<- add1(M3,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M4<-glm.nb(Mm~1+MINTEM90c+pres_galpon +season +VivTipo,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M4)
AIC(M4)

add1.test<- add1(M4,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M5<-glm.nb(Mm~1+MINTEM90c+pres_galpon +season +VivTipo+ONI_PROM_TRIMc,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M5)
AIC(M5)

add1.test<- add1(M5,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M6<-glm.nb(Mm~1+PRECIPACUM_60c,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M6)
AIC(M6)

add1.test<- add1(M6,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M7<-glm.nb(Mm~1+PRECIPACUM_60c+Year,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M7)
AIC(M7)

add1.test<- add1(M7,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M8<-glm.nb(Mm~1+PRECIPACUM_60c+Year+VivTipo,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M8)
AIC(M7)

add1.test<- add1(M8,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M9<-glm.nb(Mm~1+PRECIPACUM_60c+Year+VivTipo+season,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M9)
AIC(M9)

add1.test<- add1(M9,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M10<-glm.nb(Mm~1+PRECIPACUM_60c+Year+VivTipo+season+pres_cultivos,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M10)
AIC(M10)

add1.test<- add1(M10,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)
M11<-glm.nb(Mm~1+PRECIPACUM_60c+Year+VivTipo+season+pres_cultivos+ dist_ANPc,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M11)
AIC(M11)


M12<-glm.nb(Mm~1+season,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M12)
AIC(M12)

add1.test<- add1(M12,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M13<-glm.nb(Mm~1+season+MEDTEM90c,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M13)
AIC(M13)

add1.test<- add1(M13,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M14<-glm.nb(Mm~1+season+MEDTEM90c+pres_cultivos,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M14)
AIC(M13)

add1.test<- add1(M14,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M15<-glm.nb(Mm~1+season+MEDTEM90c+pres_cultivos+Prec_semanac,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M15)
AIC(M15)

add1.test<- add1(M15,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M16<-glm.nb(Mm~1+season+MEDTEM90c+pres_cultivos+Prec_semanac+urbano100c,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M16)
AIC(M15)


add1.test<- add1(M16,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)


M17 <- glm.nb(Mm~1+urbano150c,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M17)
AIC(M16)
add1.test<- add1(M17,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M18 <- glm.nb(Mm~1+urbano150c+ONI_PROM_TRIMc,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M18)
AIC(M18)

add1.test<- add1(M18,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi)

M19 <- glm.nb(Mm~1+urbano150c+ONI_PROM_TRIMc+season,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M19)
AIC(M19)


M20 <- glm.nb(Mm~1+ambiente,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M20)
AIC(M20)

add1.test<- add1(M20,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M21 <- glm.nb(Mm~1+ambiente+MINTEM90c ,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M21)
AIC(M21)

add1.test<- add1(M21,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M22 <- glm.nb(Mm~1+ambiente+MINTEM90c+season,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M22)
AIC(M22)

add1.test<- add1(M22,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M23 <- glm.nb(Mm~1+ambiente+MINTEM90c+season+VivTipo,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M23)
AIC(M23)

add1.test<- add1(M23,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi

M24 <- glm.nb(Mm~1+ambiente+MINTEM90c+season+VivTipo+pres_cultivos,data=data1,na.action=na.omit,offset(log(tnoches05)))
summary(M24)
AIC(M23)

add1.test<- add1(M24,scope=Mm~Year+season+ambiente+pres_galpon+pres_basura_chatarra+pres_cultivos+Altpastocmc+Cobarboreac+brozac+MINTEM30c+MEDTEM30c+MAXTEM30c+MINTEM60c+MEDTEM60c+MAXTEM60c+MINTEM90c+MEDTEM90c+MAXTEM90c+PRECIPACUM_30c+PRECIPACUM_60c+PRECIPACUM_90c+dist_arroyo_mc+dist_basuralesc+dist_ANPc+Prec_semanac+prec_diariac+TempMedia_semanac+tempMin_diariac+tempMax_diariac+ONI_PROM_TRIMc+NDVIc+NDVI50c+NDVI100c+NDVI150c+NDVI200c+NDVI250c+urbanoc+urbano50c+urbano100c+urbano150c+urbano200c+urbano250c+VivTipo+VivPiso, test="Chisq", x=NULL, k=2, trace=T,na.action=na.omit)

add1.test[order(add1.test$"Pr(>Chi)"),] #AIC Pr(>Chi
#listo#
mod_list1 <- list(M1,M2,M3,M4,M5,M6,M7,M8,M9,M10,M11,M12,M13,M14,M15,M16,M17,M18,M19,M20,M21,M22,M23, M24)
mod_names1<- c("M1","M2","M3","M4","M5","M6","M7","M8","M9","M10","M11","M12","M13","M14","M15","M16","M17","M18","M19","M20","M21","M22","M23", "M24")

aictab(mod_list1,modnames = mod_names1, second.ord = F) 

#modelo M10yM5
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



# junto todos los graficos para que sean solo dos figuras por cada tipo de variables

plot_grid(rr_PACUM, rr_TMdiaria, rr_AltP,mm_PACUM, ncol=2, labels="auto")


