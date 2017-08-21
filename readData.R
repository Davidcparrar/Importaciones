# Funcion que se encarga de leer y modificar las variables de la base de datos 
# en el mejor formato para post-procesamiento
#   - filename = "500_PERIODO_01_2016.csv": archivo que contiene la base de datos
#   de extensión .csv. (Benchmark = "Python_Twitter.csv")
readData <- function(filename = "500_PERIODO_01_2016.csv"){

  columns <- c("NUMERO.DE.FORMULARIO_C4","NUMERO.DE.IDENTIFICACIÓN.TRIBUTARIA..NIT._C5","RAZÓN.SOCIAL_C11","CÓDIGO.ADMINISTRACIÓN_C12","CÓDIGO.DEPARTAMENTO_C16","CÓDIGO.MUNICIPIO_C17","RAZÓN.SOCIAL.DEL.DECLARANTE.AUTORIZADO_C26","TIPO.DECLARACIÓN_C32","NUMERO.DE.FORMULARIO.ANTERIOR_C34","NOMBRE.EXPORTADOR.O.PROVEEDOR.EN.EL.EXTERIOR_C46","CIUDAD_C47","PAIS.EXPORTADOR_48","CODIGO.DEPARTAMENTO.DESTINO_C56","TASA.DE.CAMBIO_C58","SUBPARTIDA.ARANCELARIA_C59","PAIS.DE.ORIGEN_C66","PAIS.COMPRA_C70","PESO.BRUTO.KGS_C71","PESO.NETO.KGS_C72","NUMERO.BULTOS_C74","SUBPARTIDAS_C75","VALOR.FOB.USD_C78","SUMATORIA.DE.FLETES..SEGUROS.Y.OTROS.GASTOS.USD__C82","VALOR.ADUANA.USD_C84","TOTAL.LIQUIDADO...._C125","DESCRIPCION.DE.LA.MERCANCIA_C91","FECHA.EFECTIVA.DE.LA.TRANSACCION_C997")  
  #Leer el archivo .csv. encoding UTF-8 para incluir caracteres especiales
  importaciones <- read.csv(filename, stringsAsFactors = FALSE, header = TRUE, encoding = "UTF-8", na.strings = "")
  #Tomar las columnas necesarias
  filtrado <- importaciones[,names(importaciones) %in% columns]
  #Partida arancelaria: 2204 --> Vino 2208 --> Bebidas Alcohólicas
  partida <- "^(2204|2208)"
  #partida <- "[Uu][Ll][Tt][Rr][Aa][Ff][Ii][Ll][Tt]"
  #Subset de acuerdo a la partida
  #vinos <- filtrado[grepl(partida,as.character(filtrado$DESCRIPCION.DE.LA.MERCANCIA_C91)),]
  vinos <- filtrado[grepl(partida,as.character(filtrado$SUBPARTIDA.ARANCELARIA_C59)),]
  #Retornar el data frame cargado
  print(paste("File: ",filename," L: ",nrow(vinos)))
  vinos
}