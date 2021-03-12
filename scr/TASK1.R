#Elaborado por: Joaquin Correa (201730895), Andres Molano (201814276), Ricardo Rodriguez(201822271)
#Fecha: 13 de marzo de 2021

#Eliminar objetos antiguos
rm(list=ls())

#Cargar y/o instalar paquetes
pacman::p_load(tidyverse,reshape2,readxl)

#Fijar carpeta de trabajo
setwd ("/Users/ANDRES MOLANO/OneDrive/Documentos/2021-1/Taller de R/task-1")

#1. Vectores
  #Se crea vector del 1 al 100
  v_1 = as.matrix(1:100)
  v_1

  #Se crea vector de numeros impares (1-99)
  v_2 = as.matrix(seq(1,99,2))
  v_2

  #Se utilizan los anteriores vectores para crear un vector de numeros pares (1-100)
  v_3 = as.matrix(v_1[-v_2])
  v_3


#2.Limpiar una base de datos
  
  #Se importa base de datos
  cultivos = read_excel("data/input/cultivos.xlsx")
  View(cultivos)
  summary(cultivos)

  #limpiar informacion no relevante
  
    #Se eliminan las primeras 3 filas porque solo tienen NA
    cultivos = cultivos[-(1:3),]

    #Se nombran las columnas con los nombres de la primera fila restante
    names(cultivos) <- cultivos[1,]

    #Se elimina la primera fila que contiene los nombres porque ya cumplio su utilidad
    cultivos = cultivos[-1,]

    #Se eliminan las filas que solo contienen NA en los nombres de los municipios (Especificaban el total de cada municipio pero solo contenian NA)
    cultivos = subset(cultivos, is.na(MUNICIPIO) == F)

    #Se elimina la ultima fila de referencias
    cultivos = cultivos[-329,]

    #Ver base de datos limpia
    View(cultivos)

    #Se crea otra base de datos que contiene las columnas que nos interesan para pasar de wide-long
    cultivos_2 = cultivos[,-(1:3)]
    
    #Se pivotea base (pasar a formato long)
    cultivos_long = melt(data = cultivos_2, id.vars = c("MUNICIPIO"), value.name = "cultivos_coca")
    
    #Reemplazar valores que contienen N.A con ceros
    cultivos_long2 = mutate(cultivos_long, cultivos_coca = ifelse(test = is.na(cultivos_coca) == T, yes = 0, no = cultivos_coca))
    View(cultivos_long2)

#3. GEIH
  #3.1 Se importan las nuevas bases de datos
  personas = readRDS(file ="data/input/2019/Cabecera - Caracteristicas generales (Personas).rds")
  ocupados = readRDS(file = "data/input/2019/Cabecera - Ocupados.rds")
    
  # Para verificar en "personas" que los individuos tengan identificadores unicos se hace lo siguiente: 
  
  duplicated(personas$directorio) %>% table()
  
  duplicated(paste0(personas$directorio,personas$secuencia_p)) %>% table()
  
  duplicated(paste0(personas$directorio,personas$secuencia_p,personas$orden)) %>% table() #No hay duplicados teniendo en cuenta el orden
  
  # Para verificar en "ocupados" que los individuos tengan identificadores unicos se hace lo siguiente:
  
  duplicated(ocupados$directorio) %>% table()
  
  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p)) %>% table()
  
  duplicated(paste0(ocupados$directorio,ocupados$secuencia_p,ocupados$orden)) %>% table() #No hay duplicados teniendo en cuenta el orden
  
  #Se unen las bases de datos
  cabecera = full_join(x = personas , y = ocupados , by = c('directorio','secuencia_p','orden'))
    
  #3.2 Descriptivas
  library(ggplot2)
  summary(cabecera)

  #Se reemplazan los NA en las variables Oci y P6500
  cabecera = mutate(cabecera, Oci = ifelse(test = is.na(Oci) == T, yes = 0, no = Oci))
  cabecera = mutate(cabecera, P6500 = ifelse(test = is.na(P6500) == T, yes = 0, no = P6500))

  #Tablas que se pueden apreciar en la consolar
  
  #Media, conteo y varianza de ocupados (categorizado por genero)
  cabecera %>% group_by(P6020) %>% summarise(promedio=mean(Oci))
  cabecera %>% group_by(P6020) %>% summarise(num=sum(Oci))
  cabecera %>% group_by(P6020) %>% summarise (var=var(Oci))
  
  #Media, conteo y varianza de ocupados (categorizado por edad)
  cabecera %>% group_by(P6040) %>% summarise(promedio=mean(Oci))
  cabecera %>% group_by(P6040) %>% summarise(num=sum(Oci))
  cabecera %>% group_by(P6040) %>% summarise (var=var(Oci))
  
  #Media, conteo y varianza de ocupados (categorizado por tipo de ocupacion)
  cabecera %>% group_by(P6430) %>% summarise(promedio=mean(Oci))
  cabecera %>% group_by(P6430) %>% summarise(num=sum(Oci))
  cabecera %>% group_by(P6430) %>% summarise (var=var(Oci))
  
  #Media, conteo y varianza de ocupados (categorizado por tipo de area)
  cabecera %>% group_by(area.x) %>% summarise(promedio=mean(Oci))
  cabecera %>% group_by(area.x) %>% summarise(num=sum(Oci))
  cabecera %>% group_by(area.x) %>% summarise (var=var(Oci))
  
  #Media de salario agrupado por edad y sexo
  cabecera %>% group_by(P6040, P6020) %>% summarize(salario = weighted.mean(x = P6500)) 
  
  #Media de salario agrupado por departamentos
  cabecera %>% group_by(dpto.x) %>% summarize(salario = weighted.mean(x = P6500))
  
  
  ###Ahora, para hacer los graficos podemos denotar las variables categoricas relevantes con el fin de identificar mejor los ejes
  cabecera = mutate(cabecera , Oci = ifelse(Oci==1,'Ocupado','Desocupado'))
  cabecera = mutate(cabecera , P6020 = ifelse(P6020==1,'Hombre','Mujer'))
  
  
  #Grafico de frecuencia entre ocupados vs desocupados 
  p = ggplot() + geom_bar(data = cabecera, aes(x = Oci , colour = Oci , fill = Oci))
  p = p + labs(title = "Ocupados vs Desocupados", subtitle = "(2019)", x = "Ocupados",y = "Frecuencia")
  p
  
  ggsave(plot= p , file = "views/Ocupados vs Desocupados.jpeg")
  
  #Grafico de frecuencia entre ocupados vs desocupados (segun sexo)
  s = ggplot() + geom_bar(data = cabecera, aes(x = P6020 , colour = Oci , fill = Oci))
  s = s + labs(title = "Ocupados vs Desocupados", subtitle = "Segun sexo", x = "Sexo",y = "Frecuencia")
  s  
  
  ggsave(plot= s , file = "views/Ocupados vs Desocupados, segun sexo.jpeg")
  
  #Grafico de frecuencia entre ocupados vs desocupados (segun edad)
  ed = ggplot() + geom_bar(data = cabecera, aes(x = P6040 , colour = Oci , fill = Oci))
  ed = ed + labs(title = "Ocupados vs Desocupados", subtitle = "Segun edad", x = "Edad",y = "Frecuencia")
  ed  
  
  ggsave(plot= ed , file = "views/Ocupados vs Desocupados, segun edad.jpeg")
  
  #Grafico de salario contra edad (segun sexo)
  q = cabecera %>% group_by(P6040, P6020) %>% summarize(salario = weighted.mean(x = P6500)) %>% ggplot() + geom_point(aes(x=P6040 ,y=salario,group=P6020,shape=P6020, colour = P6020), size=3) + theme_light()
  q = q + labs(title = "Salario por edad", subtitle = "Segun sexo", x = "Edad",y = "Salario")
  q
  
  ggsave(plot= q , file = "views/Salario contra edad, segun sexo.jpeg")
  