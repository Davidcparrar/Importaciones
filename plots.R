#Funci�n principal que genera las gr�ficas solicitada
# - tweets = tweets: dataframe con la informaci�n sobre los tweets
main <- function(data = data, set = ""){
  
  #Gr�ficas de pie
  pie_SMA(topCant(data = data.frame(valor = data$rzn, cant = data$peso)),filename =  paste0("top_Importadores",set,".png"), main = "Importadores")
  pie_SMA(topCant(data = data.frame(valor = data$rzn, cant = data$costo)),filename =  paste0("top_Costo",set,".png"), main = "Importadores")
  #Gr�ficas series de tiempo
  data_ts <- data %>% group_by(tiempo) %>% summarise(cuenta = n(), peso = sum(peso/1000), costo = sum(costo)/1000000)
  plot_serie_de_t(df = data.frame(valor = data_ts$peso, tiempo = data_ts$tiempo), "Cantidad (Ton)", paste0("ts_cantidad",set,".png"))
  plot_serie_de_t(df = data.frame(valor = data_ts$costo, tiempo = data_ts$tiempo), "Costo FOB (Million USD)", paste0("ts_costo",set,".png"))
  
}

#Funci�n que genera una gr�fica tio pie
# - data = data: dataframe con los datos (2 columnas, valor y cuenta)
# - filename = filename: nombre del archivo .png a generar
# - main = "Hashtags m�s populares": T�tulo de la gr�fica
pie_SMA <- function(data = data, filename = filename, main = "Hashtags m�s populares"){
  
  #Se abre la conexi�n para crear el archivo .png
  png(filename)
  
  #Establecer labels y colores
  labels = paste(round(100*(data$cuenta)/sum(data$cuenta),1),"%")
  color = brewer.pal(6,"Dark2")#rainbow(length(data$valor)) 
  
  #Graficar el pie chart
  pie(data$cuenta, labels = labels, col = color, main = main)
  par(xpd=TRUE)
  legend("bottom",inset=c(0,-0.15), data$valor, fill = color, ncol = 3, cex = 0.8)
  
  #Cerrar conexi�n
  dev.off()
}

#Funci�n que extrae los items m�s comunes de los tweets (#,@,urls)
# - data = datos: lista con la informaci�n sobre Hashtags, Menciones, etc.
# - ndata = ndatos. lista con la cantidad de Hashtags, Menciones, etc. 
# - date = fecha: vector con las fechas de cada elemento
# - nplot = 5: n�mero de items relevantes (default 8)
top <- function(data = datos, nplot = 8){
  
  #Extraer vector de informaci�n (#,@,urls) y fecha, y ordenar
  vect <- tbl_df(data)
  vect$valor <- tolower(vect$valor)
  vect <- vect %>% group_by(valor) %>% summarise(cuenta = n()) %>% arrange(desc(cuenta))
  
  #Seleccionar los m�s relevantes
  vect_topn <- vect[1:nplot,]
  
  #Agrupar los restantes
  if(nrow(vect)>nplot){
    sum <- sum(vect$cuenta[nplot:nrow(vect)])
    vect_topn <- rbind(vect_topn,data.frame(valor = "otros", cuenta = sum))
  }
  #retornar dataframe ordenado
  vect_topn
}
topCant <- function(data = datos, nplot = 8){
  
  #Extraer vector de informaci�n (#,@,urls) y fecha, y ordenar
  vect <- tbl_df(data)
  vect$valor <- tolower(vect$valor)
  vect <- vect %>% group_by(valor) %>% summarise(cuenta = sum(cant)) %>% arrange(desc(cuenta))
  
  #Seleccionar los m�s relevantes
  vect_topn <- vect[1:nplot,]
  
  #Agrupar los restantes
  if(nrow(vect)>nplot){
    sum <- sum(vect$cuenta[nplot:nrow(vect)])
    vect_topn <- rbind(vect_topn,data.frame(valor = "otros", cuenta = sum))
  }
  
  #retornar dataframe ordenado
  vect_topn
}

#Funci�n que genera una gr�fica tio pie
# - df = dataframe: dataframe con los datos (2 columnas, valor y tiempo)
# - ylab = "Valor": Label del eje Y
# - filename = filename: nombre del archivo .png a generar
plot_serie_de_t <- function(df = dataframe, ylab = "Valor", filename = "plot.png"){
  
  #Se abre la conexi�n para crear el archivo .png
  png(filename)
  
  #Generar la serie de tiempo
  q <- ggplot(df, aes(x = tiempo, y = valor)) +geom_line(size=1.25)+xlab("Tiempo")+ylab(ylab)+geom_smooth(method = "lm")
  
  #Modificar theme y tama�o de los textos
  q <- q +theme(axis.text=element_text(size=12), axis.title=element_text(size=14,face="bold"))
  
  #Imprimir
  print(q)
  
  #Cerrar conexi�n
  dev.off()
  
}