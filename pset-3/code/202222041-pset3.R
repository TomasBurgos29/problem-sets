##Taller 3
##Tomas Burgos Velasco
##202222041

##Version
R.version.string
##4.3.2

getwd()
rm(list = ls())



##Packages
require(pacman)
p_load(rio, data.table, tidyverse, janitor,
       ggplot2, stringr, readr,patchwork, hrbrthemes)

##____________ 1. Bucle ____________##

##1.1 Lista de archivos input

lista<-list.files("pset-3/input", 
                  recursive = T, 
                  full.names = T)
  ##Creo la lista con todas las rutas de todos los archivos
  ## de la carpeta input


##1.2 Importar archivos

imp_situ <- function(situacion) {
  situ <- import_list(str_subset(lista, situacion))
  
  return(situ)
}

##Creo una funcion con la que importo los archivos con
##las palabras que busco y las importa

ocupados<-imp_situ("Ocupados")
desocupados <- imp_situ("No ocupados")
fuerza_de_trabajo <- imp_situ("Fuerza de trabajo")

##Importo todos los archivos que tengan las palabras
## Ocupados,No ocupados y Fuerza de Trabajo gracias a 
## la funcion creada en el punto anterior


##1.3 Combinar conjunto de datos  

ocupados<-rbindlist(ocupados,fill=T)
desocupados<-rbindlist(desocupados,fill = T)
fuerza_de_trabajo<-rbindlist(fuerza_de_trabajo,fill = T)

##Con la funcion rbindlist combino todos los dataframe que 
##importe en el punto anterior


##____________ 2. Preparacion ____________##


##2.1 Creacion de base de datos

f_t<-fuerza_de_trabajo %>%
  group_by(MES)%>%
  summarise(total_ft=sum(FT==1, na.rm= TRUE), 
            total_pet=sum(PET==1, na.rm=TRUE))

ocup<-ocupados %>%
  group_by(MES)%>%
  summarise(total_ocu=sum(FT==1, na.rm= TRUE))

desocup<-desocupados %>%
  group_by(MES)%>%
  summarise(total_dsi=sum(DSI==1, na.rm= TRUE))

##Cuenta todos los 1 que hay en las variables FT, PET y DSI
##en el dataframe pedido y creo una nueva base de datos con
## los 1s que hay por cada mes

##2.2 Colapsar datos a nivel mensual

output<- left_join(f_t,ocup, by="MES") %>% 
  left_join(desocup, by="MES")

##En una nueva base de datos, junto las bases creadas en el
##punto anterior con la variable "MES" en comun

##2.3 Tasas de desempleo y ocupacion 

output<-mutate(output,TD=(total_dsi/total_ft)*100,
               TO=(total_ocu/total_pet)*100)

##Le añado dos variables a "ouput" con las tasa de
##ocupacion y de desempleo para cada mes con la formula dada


##____________ 3. GGplot2 ____________##

   
gra<-ggplot(output, aes(x = MES, y=TO)) +
  geom_line(aes(group=1),size = 2,) + 
  geom_point(shape=21, color="black", fill="black", size=3)+
  labs(title = "Tasa de Ocupación de Colombia en el 2023",
            y = "Porcentaje (%)",
            x = "Mes")+
  theme_ipsum_pub()
gra2<-ggplot(output, aes(x = MES, y=TD))+
  geom_line(aes(group=1), size = 2,)+ 
 labs(title = "Tasa de Desempleo de Colombia en el 2023",
           y = "Porcentaje (%)",
           x = "Mes")+
  geom_point(shape=21, color="black", fill="black", size=3)+
  theme_ipsum_pub()
TOTD<-gra+gra2
TOTD

##Creo un ggplot tal que el eje x sea la variable MES de
## output, luego le añado las lineas en y de las tasas pedidas
##en el ejercicio en dos diferentes graficas. Sumo los graficos con 
##el paquete patchwork. Le añado el titulo del grafico y los titulos
##de los ejes 


##___________No logre la escala en el 2ndo eje de Y___________##
grf<-ggplot(output, aes(x = MES)) +
  geom_line(aes(y = TO,
                color="Tasa de ocupación",
                group=1),size = 2) +
  geom_line(aes(y = TD, 
                color = "Tasa de desempleo", 
                group=1), size = 2,)+
  scale_y_continuous(
    name= "Tasa de Ocupación (%)",  
    sec.axis = sec_axis(~.,name= "Tasa de desempleo(%)"))+
  labs(title = "Tasas de Ocupación 
       y Desempleo de Colombia en el 2023",
       y = "Porcentaje (%)",
       x = "Mes",
       color = "Indicador")+
  theme_update()
grf  
