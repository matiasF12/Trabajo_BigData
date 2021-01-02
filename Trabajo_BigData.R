#################################################################################
############################# TRABAJO WEB SCRAPPING #############################

#Instalar paquetes
install.packages("Rvest")
install.packages("gdata")

#librerias

library("rvest")
library("gdata")

#-------------------------- BOOKDEPOSITORY ------------------------------------#

#[paso 1] data de los libros
Info_bookdepository <- data.frame()

for(nropag in 1:333){
  
  urlbookthepository <- paste("https://www.bookdepository.com/es/category/3391/Teen-Young-Adult?searchLang=404&page=",nropag, sep="")
  
  #Descarga de pagina bookthepository
  bookthepository <- read_html(urlbookthepository)
  
  #listado de libros
  listado_productos <- html_nodes(bookthepository, ".content-block")
  
  #listado productos individuales
  listado_individual <- html_nodes(listado_productos, ".book-item")
  
  for(producto in listado_individual){
    print("=========================== ITEMS =======================")
    #titulos libros
    libro <- html_nodes(producto, css = ".title")
    texto_libro <- html_text(libro)
    texto_libro <- gsub("\n","", texto_libro)
    texto_libro <- trim(texto_libro)
    print(texto_libro)
    
    #Autor libro
    autor_libro <- html_nodes(producto, css = ".author")
    texto_autor <- html_text(autor_libro)
    texto_autor <- gsub("\n", "", texto_autor)
    texto_autor <- trim(texto_autor)
    print(texto_autor)
    
    #formato libro
    formato_libro <- html_nodes(producto, css= ".format")
    texto_formato <- html_text(formato_libro)
    texto_formato <- gsub("\n", "",texto_formato)
    texto_formato <- trim(texto_formato)
    print(texto_formato)
    
    #precio libro
    precio_libro <- html_nodes(producto, css= ".price")
    texto_precio <- html_text(precio_libro)
    texto_precio <- gsub("\n", "",texto_precio)
    texto_precio <- gsub("[CLP]", "", texto_precio)
    texto_precio <- gsub("[$]","", texto_precio)
    texto_precio <- gsub("[.]", "", texto_precio)
    texto_precio <- trim(texto_precio)
    
    if(length(texto_precio)== 0){
      texto_precio <- NA
    }
    precio_antiguo <- html_nodes(producto, css = ".rrp")
    texto_precio_ant <- html_text(precio_antiguo)
    texto_precio_ant <- gsub("[CLP]", "", texto_precio_ant)
    texto_precio_ant <- gsub("[$]","", texto_precio_ant)
    texto_precio_ant <- gsub("[.]", "", texto_precio_ant)
    texto_precio_ant <- as.numeric(texto_precio_ant)
    if(length(texto_precio_ant) == 1){
      texto_precio <- gsub(texto_precio_ant, "", texto_precio)
      texto_precio <- trim(texto_precio)
    }
    texto_precio <- as.numeric(texto_precio)
    print(texto_precio)
    
    #link libros
    link_libro <- html_nodes(libro, css = "a")
    link_libro <- html_attr(link_libro,"href")
    texto_link <- paste("https://www.bookdepository.com",link_libro, sep = "")
    print(texto_link)
    
    #entrar a cada libro para obtener mayor informacion
    subpagina <- read_html(texto_link)
    
    #cantidad comentarios
    cant_comentarios <- html_nodes(subpagina, xpath = '//*[@id="rating-distribution"]/span')
    texto_cant_comentarios <- html_text(cant_comentarios)
    if(length(texto_cant_comentarios) == 0){
      texto_cant_comentarios <- 0
    }else{
      texto_cant_comentarios <- gsub("opiniones", "", texto_cant_comentarios)
      texto_cant_comentarios <- gsub("opinión","", texto_cant_comentarios)
      texto_cant_comentarios <- gsub("[.]", "", texto_cant_comentarios)
      texto_cant_comentarios <- gsub("[(]", "", texto_cant_comentarios)
      texto_cant_comentarios <- gsub("[)]", "", texto_cant_comentarios)
      texto_cant_comentarios <- gsub("\n", "", texto_cant_comentarios)
      texto_cant_comentarios <- trim(texto_cant_comentarios)
    }
    texto_cant_comentarios <- as.numeric(texto_cant_comentarios)
    print(texto_cant_comentarios)
    
    
    #valoración libro
    valoracion <- html_nodes(subpagina, xpath = '//*[@id="rating-distribution"]/div[2]')
    texto_valoracion <- html_text(valoracion)
    if(length(texto_valoracion)==0){
      texto_valoracion <- 0
    }else{
      texto_valoracion <- gsub("\n", "", texto_valoracion)
      texto_valoracion <- gsub("de 5 estrellas", "", texto_valoracion)
      texto_valoracion <- gsub("[,]", ".", texto_valoracion)
      texto_valoracion <- trim(texto_valoracion)
    }
    texto_valoracion <- as.numeric(texto_valoracion)
    print(texto_valoracion)
    
    #Editorial libro
    editorial <- subpagina %>% html_node("[itemprop='publisher']") %>% html_attr("itemscope")
    print(editorial)
    
    #Numero de paginas
    cant_pag <- subpagina%>% html_node("[itemprop='numberOfPages']")
    texto_cant_pag <- html_text(cant_pag)
    texto_cant_pag <- gsub(" páginas\n", "", texto_cant_pag)
    texto_cant_pag <- as.numeric(texto_cant_pag)
    print(texto_cant_pag)
    
    
    #[paso 2] data frame con informacion de cada item
    item <- data.frame(Titulo = texto_libro, Autor = texto_autor, Editorial = editorial, 
                       Precio = texto_precio, Valoracion =  texto_valoracion,
                       Cant_coment = texto_cant_comentarios, Cant_pag = texto_cant_pag,
                       Formato = texto_formato, Url = texto_link)
    
    # [paso 3 ]almacenar la info de los libros con los datos totales
    Info_bookdepository <- rbind(Info_bookdepository,item)
  }
}


write.csv(Info_bookdepository, "informacion_bookdepository.csv")




#----------------------------------- LINIO ------------------------------------#



# Creando un data frame
info_linio <- data.frame()

for(nro_pag in 1:17){
  # Descargando la pagina
  
  URL <-paste("https://www.linio.cl/c/literatura-y-novelas/literatura-juvenil?qid=f83321c78c0344926435195bd01d0acf&oid=RO169BK0FZI35LACL&position=34&sku=RO169BK0FZI35LACL&page=", nro_pag, sep = "")
  
  linio <- read_html(URL)
  
  
  # Listado de productos
  listado_productos <- html_nodes(linio, css = "#catalogue-product-container")
  
  
  #listado productos individuales
  listado_individual <- html_nodes(listado_productos, css =".catalogue-product")
  
  # Intentando sacar las cosas
  
  for (producto in listado_individual) {
    print("------------------ ITEMS ------------------")
    
    # Titulos
    titulo <- html_nodes(producto, css = ".title-section")
    texto_titulos <- html_text(titulo)
    print(texto_titulos)
    
    # Precio
    precio <- html_nodes(producto, css = ".price-main-md")
    texto_precios <- html_text(precio)
    texto_precios <- gsub("\n", "", texto_precios)
    texto_precios <- gsub("[$]", "", texto_precios)
    texto_precios <- gsub("[.]", "", texto_precios)
    texto_precios <- trim(texto_precios)
    texto_precios <- as.numeric(texto_precios)
    print(texto_precios)
    
    # Link
    link <- html_nodes(producto, css = ".rating-container")
    link_producto <- html_nodes(link, css = "a")
    link_producto <- html_attr(link, "href")
    link_producto <- paste("https://www.linio.cl", link_producto, sep = "")
    print(link_producto)
    
    #SUBPAGINA
    sub_pagina <- read_html(link_producto)
    
    #estrellas
    estrellas <- html_nodes(sub_pagina, xpath = '//*[@id="display-zoom"]/div[1]/div[1]/span[1]/span[1]')
    texto_estrellas <- html_text(estrellas)
    if(length(texto_estrellas)==0){
      texto_estrellas <- 0
    }else{
      texto_estrellas <- as.numeric(texto_estrellas)
    }
    print(texto_estrellas)
    
    #numero de reseñas
    comentarios <- html_nodes(sub_pagina, xpath = '//*[@id="display-zoom"]/div[1]/div[1]/span[2]')
    texto_cant_comentarios <- html_text(comentarios)
    if(length(texto_cant_comentarios)==0){
      texto_cant_comentarios <- 0
    }else{
      texto_cant_comentarios <- gsub(" reseñas", "", texto_cant_comentarios)
      texto_cant_comentarios <- gsub(" reseña", "", texto_cant_comentarios)
      texto_cant_comentarios <- as.numeric(texto_cant_comentarios)
    }
    print(texto_cant_comentarios)
    
    #Tabla
    Tabla <- html_nodes(sub_pagina, css = ".features-box-section")
    Tabla <- html_nodes(Tabla, css = "div")
    
    #Autor
    autor <- F
    for(variable in Tabla){
      texto_autor <- html_text(variable)
      if(autor){
        print(texto_autor)
        autor <- F
        texto_autor2 <- texto_autor
      }
      if(texto_autor=="Autor"){
        autor <- T
      }
    }
    
    
    #Editorial
    editorial <- F
    for(variable in Tabla){
      texto_editorial <- html_text(variable)
      if(editorial){
        print(texto_editorial)
        editorial <- F
        texto_editorial2 <- texto_editorial
      }
      if(texto_editorial=="Editorial"){
        editorial <- T
      }
    }
    
    
    #Cantidad de paginas
    cant_pag <- F
    texto_cant_pag2 <- NA
    for(variable in Tabla){
      texto_cant_pag <- html_text(variable)
      if(cant_pag){
        print(texto_cant_pag)
        cant_pag <- F
        texto_cant_pag2 <- texto_cant_pag
        texto_cant_pag2 <- as.numeric(texto_cant_pag2)
      }
      if(texto_cant_pag=="Número de paginas"){
        cant_pag <- T
      }
      
    }
    
    #Tipo de pasta (FORMATO)
    formato <- F
    texto_formato2 <- NA
    for(variable in Tabla){
      texto_formato <- html_text(variable)
      if(formato){
        print(texto_formato)
        formato <- F
        texto_formato2 <- texto_formato
      }
      if(texto_formato=="Tipo de pasta"){
        formato <- T
      }
      
    }
    
    producto <- data.frame(Titulo = texto_titulos, Autor = texto_autor2, Editorial = texto_editorial2,
                           Precio = texto_precios, Valoracion = texto_estrellas,
                           Cant_coment = texto_cant_comentarios, Cant_pag = texto_cant_pag2, 
                           Formato= texto_formato2, URL = link_producto)
    
    info_linio <- rbind(info_linio, producto) 
    
  }
  
}

write.csv(info_linio, "informacion_linio.csv")



##############################################################################################################
#----------------------------------- CREAR GRAFICOS PARA BOOKDEPOSITORY --------------------------------------

Info_bookdepository <- read.csv("informacion_bookdepository.csv")

##############################################################################################################
#--------------------------------------- CREAR GRAFICOS PARA LINIO -------------------------------------------


info_linio <- read.csv("informacion_linio.csv")
