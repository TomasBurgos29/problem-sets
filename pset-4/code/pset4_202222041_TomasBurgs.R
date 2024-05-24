##Tomas Burgos Velasco
##202222041
##PSET-4

##Limpiar
rm(list=ls())

##Paquetes
require(pacman)
p_load(tidyverse, 
       rio,
       data.table,
       skimr,
       sf,
       rvest,
       ggplot2,
       mapview,
       tmaptools,
       viridis,
       osmdata)

##1.Extraer la información de internet

##1.1 Obtener las url 

pset4<-"https://eduard-martinez.github.io/pset-4.html"
url_full<-read_html(pset4)%>%html_elements("a") %>% html_attr("href")

##1.2 Filtrar url

url_subset<- str_subset(url_full, "propiedad")

##1.3 Extraer las tablas de los HTML

lista_tablas<-list()

for (vivienda in 1:220){

html_i<-read_html(url_subset[vivienda])

  precio<-html_i%>%
    html_element(xpath=
      '//*[@id="caracteristicas-de-la-vivienda"]/table/tbody/tr/td[8]')%>%
    html_text2()

  coordenadas<- html_i%>%
    html_element(xpath=
     '//*[@id="caracteristicas-de-la-vivienda"]/table/tbody/tr/td[12]')%>%
    html_text2()

  latitud<- html_i%>%
    html_element(xpath=
      '//*[@id="caracteristicas-de-la-vivienda"]/table/tbody/tr/td[10]') %>%
    html_text2()

  
  longitud<-html_i%>%
    html_element(xpath=
      '//*[@id="caracteristicas-de-la-vivienda"]/table/tbody/tr/td[11]') %>%
    html_text2()  

casa_dt<-list(URL=url_subset[vivienda], price=precio,lat=latitud,
              lon=longitud, coords=coordenadas)
lista_tablas[[vivienda]]<-casa_dt 
}

##1.4 Preparar información

db_house<-rbindlist(lista_tablas, fill=T)
db_house$price<-as.numeric(db_house$price)

##2. Manipular la información GIS

##2.1 Cree un objeto sf

sf_house<- st_as_sf(db_house, coords=c("lon","lat"),crs=4326)

##2.2 Pintar mapa

bog <- opq(bbox = getbb("Bogota Colombia")) %>%
  add_osm_feature(key="boundary", value="administrative") %>% 
  osmdata_sf()
bog <- bog$osm_multipolygons %>% subset(admin_level==9)

mapa <- ggplot() +
  geom_sf(data = bog, color = "grey") +
  geom_sf(data = sf_house, aes(color = price), size = 1) +
  scale_color_viridis(option="inferno", trans="log10", direction=-1)+
  labs(title= "Precio de la vivienda en Bogotá", 
       color= "Precio (en escala log10)")+
  theme_void()

ggsave("pset-4/output/mapa_vivienda.pdf", plot=mapa)

##3. Bonos
  


##3.1 R-Markdown



##3.2 GitHub