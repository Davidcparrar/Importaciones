# Función encargada de procesar y limpiar la base de datos 
#   - data = dataframe: Base de datos a procesar
processData <- function(data = dataframe){
  
  id <- as.factor(data$NUMERO.DE.FORMULARIO_C4)
  id_prev <- as.factor(data$NUMERO.DE.FORMULARIO.ANTERIOR_C34)
  nit <- as.factor(data$NUMERO.DE.IDENTIFICACIÓN.TRIBUTARIA..NIT._C5)
  peso <- as.numeric(data$PESO.NETO.KGS_C72)
  rzn <- as.factor(data$RAZÓN.SOCIAL_C11)
  tipo <- as.factor(data$TIPO.DECLARACIÓN_C32)
  costo <- as.numeric(data$VALOR.FOB.USD_C78)
  subpartida <- as.factor(data$SUBPARTIDA.ARANCELARIA_C59)
  #Calcular tiempo de time series
  tiempo <- ymd(data$FECHA.EFECTIVA.DE.LA.TRANSACCION_C997)
  tiempo <- ymd(paste0(year(tiempo),month(tiempo),"-15"))
  año <- year(tiempo)
  datos <- tbl_df(data.frame(id = id,id_prev = id_prev, nit = nit, peso = peso, rzn = rzn, tipo = tipo, costo = costo, tiempo = tiempo, subpartida = subpartida, año = año))
  datos
}
