##Tomas Burgos Velasco
##202222041
##Taller 2

rm(list=ls())
getwd()

##Packages
require("pacman")
p_load(rio,skimr,janitor,tidyverse,data.table)

##1. Importar/exportar bases de datos
##1.1 Importar

location=import(file="pset-2/input/Módulo de sitio o ubicación.dta") ##Se importa de input
location<-location ##Guardo en un objeto location
identification=import(file="pset-2/input/Módulo de identificación.dta")  ##Se importa de input
identification<-identification ##Guardo en un objeto location

##1.2 Exportar

export(x=identification, file="pset-2/output/identification.rds") ##Exporto en output
export(x=location, file="pset-2/output/location.rds") ##Exporto en output


##2. Generar variables
##2.1 Bussiness_type
identification=mutate(identification, 
                      "bussiness_type"=case_when(GRUPOS4=="01"~"Agricultura",
                      identification$GRUPOS4=="02"~"Industria manufacturera",
                      identification$GRUPOS4=="03"~"Comercio",
                      identification$GRUPOS4=="04"~"Servicios")) ##Creo una variable segun el punto 2.1
##2.2 Grupo etario

##P241 Edad del propietario
max(identification$P241)##Encontramos el maximo en la variable P241
min(identification$P241)##Encontramos el minimo en la variable P241
##Jovenes=18-28 Es una etapa de la vida que muchos consideran alrededor de este
##rango, es interesante saber cuantos jovenes son propietarios de micronegocios
##Adultos=29-59 Etapa de la vida que muchos consideran alrededor de
##este rango. 
##Edad avanzada=60-74 Segun la OMS, las personas en esta rango de edad son
##personas de edad avanzada
##Ancianos= 75-99 Segun la OMS, las personas en esta rango de edad son
##personas de ancianos

identification=mutate(identification, 
                  "grupo_etario"= 
                  case_when
                  (identification$P241>=18 & identification$P241<=28~"Jovenes",
                  identification$P241>=29 & identification$P241<=59~"Adultos",
                  identification$P241>=60 & identification$P241<=74
                  ~"Edad avanzada",
                  identification$P241>=75 & identification$P241<=99~"Ancianos"))
#Creo una variable segun el punto 2.2
location=mutate(.data=location, ambulante=ifelse
                (test=location$P3053==3 | location$P3053==4|location$P3053==5,
                yes=1, no =0)) #Creo una variable segun el punto 2.3

##3.Eliminar filas/columnas de un conjunto de datos
##3.1 Almacenar en un objeto

ambulante<-select(.data=location,ambulante) ##Creo un objeto solo con las variables de ambulante

identification_1<-select(.data=identification, 
                      DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA, grupo_etario,
                      COD_DEPTO, F_EXP) ##Creo otro objeto con las otras variables

identification_sub<-bind_cols(identification_1,ambulante) ##Creo un objeto que tenga las variables de ambos objetos

##3.2 Seleccionar variables de un objeto y guardarlo en uno nuevo

location_sub<-select(.data=location,DIRECTORIO, SECUENCIA_P, SECUENCIA_ENCUESTA,
                     ambulante,P3054,P469,COD_DEPTO,F_EXP ) ##Creo un objeto seleccionando las variables especificas de otro objeto

##4.Combinar base de datos
##4.1 Unir en una base de datos 2 objetos

df<-full_join(x=identification_sub, y=location_sub, by =c("DIRECTORIO",
                                                          "SECUENCIA_P",
                                                          "SECUENCIA_ENCUESTA",
                                                          "ambulante",
                                                          "COD_DEPTO",
                                                          "F_EXP"))
##Junto los 2 objetos creados en el punto anterior y los uno a traves de sus variables compartidas


##5. Descriptivas
##5.1 Utlizar funciones descriptivas

skim(df)
summary(df)


##5.2 Extraer variables descriptivas

propietarios_edad <- df %>%
  group_by(COD_DEPTO, grupo_etario) %>%
  summarise(cantidad_propietarios = n())


##La cantidad de propietarios adultos es mayor en todos los departamentos 
##En cambio los ancianos tienen la menor cantidad de micronegocios siempre,
##los jovenes y edad avanzada tienen cifras similares


edad_bussiness <- identification %>%
  group_by(bussiness_type, grupo_etario) %>%
  summarise(cantidad_propietarios = n())

##Observamos que en todos los grupos etarios de los propietarios de micronegocios
##trabajan con servicios, exceptuando los ancianos. Mientras que donde menos
##trabajan es en la industria manufacturera


edad_negocio_ambulante <- df %>%
  group_by(grupo_etario, ambulante) %>%
  summarise(cantidad_propietarios = n())


##Vemos que hay más ambulantes jovenes comparados a los no ambulantes en el 
##mismo grupo etario. En el resto de grupos, siempre hay más no ambulantes.