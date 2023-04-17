library(DataEditR)
library(foreign)
library(apaTables)
library(PerformanceAnalytics)
library(psych)
library(corrr)
library(readxl)
library(xlsx)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(rapportools)


file.choose()
#df_2 <- data_edit(df)


#espacio de trabajo
ruta <- "D:\\NIXON\\universidad\\programas universidad\\R\\practica\\correlacion\\06. comparacio estaciones.xlsx"
df <- read_excel(ruta)

#filtrado de año
df2011 = df %>% filter(año == 2011)
df2012 = df %>% filter(año == 2012)
df2013 = df %>% filter(año == 2013)
df2014 = df %>% filter(año == 2014)
df2015 = df %>% filter(año == 2015)
df2016 = df %>% filter(año == 2016)
df2017 = df %>% filter(año == 2017)
df2018 = df %>% filter(año == 2018)
df2019 = df %>% filter(año == 2019)
df2020 = df %>% filter(año == 2020)
df2021 = df %>% filter(año == 2021)



#promedio de concentraciones
intento2011 <- df2011 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2012 <- df2012 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2013 <- df2013 %>% group_by(mes) %>%summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2014 <- df2014 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2015 <- df2015 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2016 <- df2016 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2017 <- df2017 %>% group_by(mes) %>%summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
#intento2017 <- intento2017[intento2017$promedioBEME != "0",]
intento2018 <- df2018 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2019 <- df2019 %>% group_by(mes ) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2020 <- df2020 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )
intento2021 <- df2021 %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )



#df_2 <- data_edit(intento2019)


#area de graficos
g_2021 <- ggplot(intento2021, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2021")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2020 <- ggplot(intento2020, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2020")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2019 <- ggplot(intento2019, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2019")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2018 <- ggplot(intento2018, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2018")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2017 <- ggplot(intento2017, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2017")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2016 <- ggplot(intento2016, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2016")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

g_2015 <- ggplot(intento2015, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2015")+
  ylab("PM2.5")+
  theme(axis.text = element_text(angle = 90))


g_2014 <- ggplot(intento2014, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+ 
  labs(title = "2014")+
  ylab("PM2.5	(µ/m3)")

g_2013 <- ggplot(intento2013, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2013")+
  ylab("PM2.5	(µ/m3)")+theme(axis.text = element_text(angle = 90))

g_2012 <- ggplot(intento2012, aes(x = mes))+
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "2012")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))



ordenar1 <- grid.arrange(g_2021, g_2020 , g_2019, g_2018)
ordenar2 <- grid.arrange(g_2017,g_2016, g_2015, g_2014)
ordenar3 <- grid.arrange(g_2013, g_2012)



##################################################################################
##################################################################################
#   ANOVA
##############################################################################
#necesitamos organizarlo asi para poder graficar grafica de cajas 


AON = df %>% filter(año >= 2018 & año <= 2022 )

intentoAOV <- AON %>% group_by(mes) %>% summarise( EST_HOSP = mean(`EST-HOSP`), ITA_CONC = mean(`ITA-CONC`), CEN_TRAF = mean(`CEN-TRAF`), ITA_CJUS = mean(`ITA-CJUS`), MED_EXSA = mean(MED_EXSA) )

ggplot(intentoAOV, aes(x = mes))+
  geom_line(aes(y  = EST_HOSP, group = 1 , color = "EST_HOSP"  ))+
  geom_point(aes(y = EST_HOSP,group = 1, color = "EST_HOSP" ))+
  
  geom_line(aes(y  = ITA_CONC, group = 1 , color = "ITA_CONC"  ))+
  geom_point(aes(y = ITA_CONC,group = 1, color = "ITA_CONC" ))+
  
  geom_line(aes(y  = CEN_TRAF, group = 1 , color = "CEN_TRAF"  ))+
  geom_point(aes(y = CEN_TRAF,group = 1, color = "CEN_TRAF" ))+
  
  geom_line(aes(y  = ITA_CJUS, group = 1 , color = "ITA_CJUS"  ))+
  geom_point(aes(y = ITA_CJUS,group = 1, color = "ITA_CJUS" ))+
  
  geom_line(aes(y  = MED_EXSA, group = 1 , color = "MED_EXSA"  ))+
  geom_point(aes(y = MED_EXSA,group = 1, color = "MED_EXSA" ))+
  labs(title = "Concentraciones promedio de PM2.5 para las estaciones del Valle de Aburra")+
  ylab("PM2.5	(µ/m3)")+
  theme(axis.text = element_text(angle = 90))

print(intentoAOV)
ordenadoAOV <- intentoAOV %>% gather(key = 'estacion' , value = 'pm2.5', -mes) %>%
  arrange(mes)


cajas_2021 <- boxplot(pm2.5~estacion, ordenadoAOV)#con un diagrama de cajas podemos revisar que tan distintas son las medias entre si

anova <- aov(pm2.5~estacion, ordenadoAOV)
#este se hace para sacar el numero Pr donde si este es menor a 0.05 entonces nos dice que nuestras medias 
#si son distintas
summary(anova)
#la prueba de tukey se hace para saber cual de las medias es distinta
#donde si su P es mayor a 0.05 es significativo osea sus medias son
#iguales
TukeyHSD(anova)


ordenado2019 <- intento2019 %>% gather(key= 'estacion' , value = 'pm2.5', -mes) %>%
  arrange(mes)


cajas_2019 <- boxplot(pm2.5~estacion, ordenado2019)#con un diagrama de cajas podemos revisar que tan distintas son las medias entre si
anova <- aov(pm2.5~estacion, ordenado2019)
summary(anova)
TukeyHSD(anova)


################################################################################
################################################################################
#CORRELACION DE PEARSON
###############################################################################
###############################################################################



correlacioAOV <- select(intentoAOV, - mes)
cor(correlacioAOV)

pairs.panels(correlacioAOV, pch = 20 , stars = TRUE)
