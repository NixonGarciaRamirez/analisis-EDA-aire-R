#primero ubicamos el archivo que vamos a correlacionar

carpeta <- c("D:/NIXON/universidad/programas universidad/R/practica/correlacion")
setwd(carpeta)

#abriendo un archivo excel
file.choose()
ruta <- "D:\\NIXON\\universidad\\programas universidad\\R\\practica\\correlacion\\estaciones.xlsx"
df_excel <- read_excel(ruta)


#abriendo un archivo spss
df <- read.spss("base datos spss correlaciones.sav", to.data.frame = TRUE)
  
View(df)


#ahora vamos a limpiar el df eliminando todos los datos NA

df2 <- df_excel[complete.cases(df_excel),]


#ahora ejecutamos el copdigo de correlaciones 

#NOTA: tanto python como R tienen la correlacion de pearson como correlacion pr defecto
cor(df_excel)

#ahora vamos a crear un objeto con las correlaciones para poder exportarlo si es lo que queremos
correlaciones <- cor(df_excel)


#por ejemplo exportemos la correlacion a un word formato APA
apa.cor.table(df_excel, filename = "estraciones", table.number = 2 , show.conf.interval = FALSE , landscape = TRUE)


#crear una matriz de correlacion mucho mas vistosa, de hecho aqui se grafica
#un histograma y las correlaciones se analizan visualmente usando el metodo 
#quantile-quantile
pairs.panels(df_excel, pch = 20 , stars = TRUE)


#la idea ahora es aplicar un sistema de corelacio visual con R

h <- correlate(df_excel)
network_plot(h)
