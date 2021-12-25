####   SOFTONIC  #####

setwd("~/GitHub/Bigdata2021")

library("xml2")
library("rvest")
install.packages("dplyr")
library("dplyr")
install.packages("ggplot2")
library(ggplot2)
library(graphics)
install.packages("tidyverse")
library(tidyverse)
install.packages("datos")
library(datos)

  
  #leyendo pagina
  
  PaginaSoftonic <- read_html("https://en.softonic.com/windows")
  print(html_text(PaginaSoftonic))
  
  # Información body
  Categoria_productos <- html_nodes(PaginaSoftonic, css=".s-media__body")
  print(Categoria_productos)
  
  #Categorías
  Categoria_Apps <- html_nodes(PaginaSoftonic, css="h2")
  print(Categoria_Apps)
  Categoria_Apps <- html_text(Categoria_Apps)
  print(Categoria_Apps)
  
  # Aplicaciones
  Lista_Apps <- html_nodes(PaginaSoftonic, css="h3")
  print(Lista_Apps)
  Lista_Apps <- html_text(Lista_Apps)
  print(Lista_Apps )
  
  #Links categorias
  seccion_categoria <- html_nodes(PaginaSoftonic, css=".list-category-apps")
  Links_categoria <- html_nodes(seccion_categoria,css="a")
  print(Links_categoria)
  href_categoria <- html_attr(Links_categoria,"href")
  print(href_categoria)
  ##### quiero eliminar duplicados para que calce el length, pero no pude con distinct(href_categoria)

  #####SUBPAGINA 
  
  subpaginaSoftonic <- read_html("https://chrome.en.softonic.com/")
  print(subpaginaSoftonic)
  
  appSpecsLi <- html_nodes(subpaginaSoftonic, css=".app-specs__list > li")
  appSpecsLi <- html_text(appSpecsLi)
  print(paste(appSpecsLi))
  
  #extrayendo títulos
  appSpecsTitle <- html_nodes(subpaginaSoftonic, css=".app-specs__list > li")
  appSpecsTitle <- html_text(html_nodes(appSpecsTitle, css=".app-specs__title"))
  print(paste(appSpecsTitle))
  
  #extrayendo descripcion
  appSpecsDescription <- html_nodes(subpaginaSoftonic, css=".app-specs__list > li")
  appSpecsDescription <- html_text(html_nodes(appSpecsDescription, css=".app-specs__description"))
  print(paste(appSpecsDescription))
  #####la descripción es mas grande que el  titulo porque descarga varios idiomas como elementos
  
  
####Subpagina enfocada en los navegadores más populares
  
  pagina_navegadores <- read_html("https://www.softonic.com/windows/navegadores:tendencias")
  print(html_text(pagina_navegadores))
##Extrayendo nombre de navegadores más populares
  buscando_apps <- html_nodes(pagina_navegadores, css=".media__body")
  print(html_text(buscando_apps))
  nombre_apps <- html_nodes(buscando_apps, css="h2")
  print(html_text(nombre_apps))
    
##extrayendo links de navegadores más populares
  links_productos <- html_nodes(pagina_navegadores, css="ul > li")
  links_apps <- html_nodes(links_productos,css="a")
  print(links_apps)
  href_apps <- html_attr(links_apps,"href")
  links_finales <-(href_apps[11:30])
  print(links_finales)
  
  ##Extrayendo Idioma
  extrayendo_idioma <- html_nodes(pagina_navegadores, css=".media-app__language")
  print(html_text(extrayendo_idioma))

  
  ##Extrayendo rating
  buscando_rating <- html_nodes(pagina_navegadores, css=".app-list-item__rating")
  print(html_text(buscando_rating))
  rating_apps <- html_nodes(buscando_rating, css="div")
  print(html_text(rating_apps))
  score_numeros <- (rating_apps[c(1,4,7,10,13,16,19,22,25,28,31,34,37,40,43,46,49,52,55,58)])
  print(html_text(score_numeros))
  
  ##Extrayendo votos
  votos <- (rating_apps[c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44,47,50,53,56,59)])
  print(html_text(votos))
  
  ##Extrayendo número de descargas
  descargas <- (rating_apps[c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45,48,51,54,57,60)])
  print(html_text(descargas))
  
  ## Extrayendo resumen
  extrayendo_resumen <- html_nodes(pagina_navegadores, css=".media-app__summary")
  print(html_text(extrayendo_resumen))
  sumary <- html_nodes(extrayendo_resumen, css="strong")
  print(html_text(sumary))
  
  
  
##información detallada y ordenada de la aplicación
  
softonicProduct <- read_html("https://tineye-reverse-image-search.softonic.com/")

appSpectsLi <- html_nodes(softonicProduct, css=".app-specs__list > li")
print(appSpectsLi)

for (appSpectLi in appSpectsLi) {
  textTitle <- html_text(html_nodes(appSpectLi, css=".app-specs__title"))
  print(textTitle)
  if(textTitle[1] == "Licencia"){
    textDetail <- html_text(html_nodes(appSpectLi, css=".app-specs__description"))
    print(paste("====>",textDetail))
  }else if (textTitle[1] == "Versión"){
    textDetail <- html_text(html_nodes(appSpectLi, css=".app-specs__description"))
    print(paste("====>",textDetail))
  }else if (textTitle[1] == "Plataforma"){
    textDetail <- html_text(html_nodes(appSpectLi, css=".app-specs__description"))
    print(paste("====>",textDetail))
  }else if (textTitle[1] == "Sistema operativo"){
    textDetail <- html_text(html_nodes(appSpectLi, css=".app-specs__description"))
    print(paste("====>",textDetail))
  }else if (textTitle[1] == "Descargas"){
    textDetail <- html_text(html_nodes(appSpectLi, css=".app-specs__description"))
    print(paste("====>",textDetail))
  }
}


####Data Frame
todo_datos <- data.frame(nombre=html_text(nombre_apps),
                    links=links_finales,
                    idioma=(html_text(extrayendo_idioma)),
                    rating=html_text(score_numeros),
                    numeroVotos=html_text(votos),
                    numeroDescargas=html_text(descargas),
                    resumen=(html_text(sumary)))

#####Tabla links navegadores(20)
tablaLinks <- table(href_apps[11:30])
print(tablaLinks)
View(tablaLinks)

tabla_votos <- table(html_text(votos))
print(tabla_votos)
View(tabla_votos)

tabla_apps <- table(html_text(nombre_apps))
print(tabla_apps)
View(tabla_apps)

tabla_rating <-table(html_text(score_numeros))
print(tabla_rating)
view(tabla_rating)


