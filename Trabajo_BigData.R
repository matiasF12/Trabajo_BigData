#################################################################################
############################# TRABAJO WEB SCRAPPING #############################

#Instalar paquetes
install.packages("Rvest")
install.packages("gdata")

#librerias
library("rvest")
library("gdata")

#-------------------------- BOOKDEPOSITORY ------------------------------------#

#[paso 1] generar dataframe que contendra datos extraidos (libros)
Info_bookdepository <- data.frame()

#Inicio de la extraccion de los datos, que va de la pag 1 a la 333
for(nropag in 1:333){
  #La url se mantiene, a excepcion del numero de pagina que va cambiando.
  urlbookthepository <- paste("https://www.bookdepository.com/es/category/3391/Teen-Young-Adult?searchLang=404&page=",nropag, sep="")
  
  #Descarga de pagina bookthepository
  bookthepository <- read_html(urlbookthepository)
  
  #listado de libros, ubicada en "content block"
  listado_productos <- html_nodes(bookthepository, ".content-block")
  
  #listado productos individuales ubicada en "book item"
  listado_individual <- html_nodes(listado_productos, ".book-item")
  
  #Se genera otro For para la extraccion de el titulo, autor, precio, etc de cada observacion.
  for(producto in listado_individual){
    print("=========================== ITEMS =======================")
    #titulo libro obtenido de clase ".title", se transforma a texto, se limpia y por ultimo, se imprime.
    libro <- html_nodes(producto, css = ".title")
    texto_libro <- html_text(libro)
    texto_libro <- gsub("\n","", texto_libro)
    texto_libro <- trim(texto_libro)
    print(texto_libro)
    
    #Autor libro obtenido de la clase ".author", se transforma a texto, se limpia y por ultimo, se imprime.
    autor_libro <- html_nodes(producto, css = ".author")
    texto_autor <- html_text(autor_libro)
    texto_autor <- gsub("\n", "", texto_autor)
    texto_autor <- trim(texto_autor)
    print(texto_autor)
    
    #formato libro obtenido de la clase ".format", se transforma a texto, se limpia y por ultimo, se imprime.
    formato_libro <- html_nodes(producto, css= ".format")
    texto_formato <- html_text(formato_libro)
    texto_formato <- gsub("\n", "",texto_formato)
    texto_formato <- trim(texto_formato)
    print(texto_formato)
    
    #precio libro obtendo de la clase ".price", se transforma a texto, se limpia y por ultimo, se imprime.
    precio_libro <- html_nodes(producto, css= ".price")
    texto_precio <- html_text(precio_libro)
    texto_precio <- gsub("\n", "",texto_precio)
    texto_precio <- gsub("[CLP]", "", texto_precio)
    texto_precio <- gsub("[$]","", texto_precio)
    texto_precio <- gsub("[.]", "", texto_precio)
    texto_precio <- trim(texto_precio)
    
    #El IF se aplica para aquellos libros en los que no este disponible el precio
    if(length(texto_precio)== 0){
      texto_precio <- NA
    }
    #Existe un precio antiguo dado que algunos libros contenian dos precios, por lo que en el dataframe aparecian dos valores 
    #en lugar de uno. Para solucionar este problema, se genera esta varible con el fin de eliminarla (obtenida de la clase
    # ".rrp"). Esta se transforma a texto, se limpia, y la siguiente condicionante indica que si es que el precio antiguo 
    #contiene alguna informacion esta sea cambiada por un valor vacio ("").
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
    
    #link libros se obtiene de "a" y "href", el cual esta dentro de el titulo del libro. Esta se limpia, y luego 
    #se copia el link de la pagina principal junto a la extension del libro en particular, para finalmente imprimirlo.
    link_libro <- html_nodes(libro, css = "a")
    link_libro <- html_attr(link_libro,"href")
    texto_link <- paste("https://www.bookdepository.com",link_libro, sep = "")
    print(texto_link)
    
    #Se genera subpagina para entrar a cada libro y obtener mayor informacion
    subpagina <- read_html(texto_link)
    
    #cantidad comentarios de cada libro obtenido de la subpagina, este se pasa a texto y luego se genera un IF para
    #saber si es que el existen comentarios. Si es que no existen comentarios se pondrá un valor 0, mientras que 
    #si es que existen comentarios se pondrá el valor correspondiente. Esto se limpia y luego se imprime.
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
    
    
    #valoración libro se obtiene de la subpagina, y se transforma a texto. El IF se utiliza para saber si es que 
    #existe alguna valoración de cada libro. En caso de que algun libro no tenga valoracion se pondra un 0, mientras que si
    #tiene alguna valoracion, se pondrá el valor correspondiente. A esto se le realiza limpieza, y luego se imprime.
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
    
    #Editorial libro se obtiene de la subpagina, a traves de "itemprop" llamada "publiser" y del "itemscope" debido a que
    #se encontraba dentro de una tabla y no tenia una clase o ID especifico.
    editorial <- subpagina %>% html_node("[itemprop='publisher']") %>% html_attr("itemscope")
    print(editorial)
    
    #Numero de paginas se obtiene de subpagina a traves del "itemprop" llamado "numberOfPages", luego se transforma a texto
    #se realiza la limpieza y se imprime. Al igual que editorial, esta variable estaba dentro de una tabla.
    cant_pag <- subpagina%>% html_node("[itemprop='numberOfPages']")
    texto_cant_pag <- html_text(cant_pag)
    texto_cant_pag <- gsub(" páginas\n", "", texto_cant_pag)
    texto_cant_pag <- as.numeric(texto_cant_pag)
    print(texto_cant_pag)
    
    
    #[paso 2] Se genera un data frame con informacion de cada item extraido.
    item <- data.frame(Titulo = texto_libro, Autor = texto_autor, Editorial = editorial, 
                       Precio = texto_precio, Valoracion =  texto_valoracion,
                       Cant_coment = texto_cant_comentarios, Cant_pag = texto_cant_pag,
                       Formato = texto_formato, Url = texto_link)
    
    #[paso 3] Se almacena la info de los libros con los datos totales.
    Info_bookdepository <- rbind(Info_bookdepository,item)
  }
}

#La informacion almacenada se guarda en un csv.
write.csv(Info_bookdepository, "informacion_bookdepository.csv")




#----------------------------------- LINIO ------------------------------------#



#Creando un data frame
info_linio <- data.frame()

#Se genera un for para extraer los datos de las diferentes paginas que contiene los libros
for(nro_pag in 1:17){
  
  #Se pega el link de la pagina al numero de esta, para que vaya cambiando.
  URL <-paste("https://www.linio.cl/c/literatura-y-novelas/literatura-juvenil?qid=f83321c78c0344926435195bd01d0acf&oid=RO169BK0FZI35LACL&position=34&sku=RO169BK0FZI35LACL&page=", nro_pag, sep = "")
  
  #Descarga de la pagina
  linio <- read_html(URL)
  
  #Listado de productos, ubicada en "catalogue product container" 
  listado_productos <- html_nodes(linio, css = "#catalogue-product-container")
  
  #listado productos individuales ubicado en "catalogue product"
  listado_individual <- html_nodes(listado_productos, css =".catalogue-product")
  
  #Extraccion de los datos del listado individual para cada observacion
  for(producto in listado_individual){
    print("------------------ ITEMS ------------------")
    
    #Titulos, obtenido de la clase "title section", se transforma a texto y si imprime
    titulo <- html_nodes(producto, css = ".title-section")
    texto_titulos <- html_text(titulo)
    print(texto_titulos)
    
    #Precio obtenido de la clase "price main md", se transforma a texto, se limpia y se imprime
    precio <- html_nodes(producto, css = ".price-main-md")
    texto_precios <- html_text(precio)
    texto_precios <- gsub("\n", "", texto_precios)
    texto_precios <- gsub("[$]", "", texto_precios)
    texto_precios <- gsub("[.]", "", texto_precios)
    texto_precios <- trim(texto_precios)
    texto_precios <- as.numeric(texto_precios)
    print(texto_precios)
    
    #Link se obtiene de la clase "rating container", y desde ahi se obtiene el "a" y "href" para extraer
    #la url a la cual se le debe agregar al link de la pagina principal y luego se imprime
    link <- html_nodes(producto, css = ".rating-container")
    link_producto <- html_nodes(link, css = "a")
    link_producto <- html_attr(link, "href")
    link_producto <- paste("https://www.linio.cl", link_producto, sep = "")
    print(link_producto)
    
    #SUBPAGINA ingresara a cada link de cada producto (libro)
    sub_pagina <- read_html(link_producto)
    
    #Valoracion se extrae de la sub pagina utilizando un Xpath. En caso de que no se tenga una valoracion
    #se pone un 0, en caso contrario se pone el valor correspondiente y se imprime
    estrellas <- html_nodes(sub_pagina, xpath = '//*[@id="display-zoom"]/div[1]/div[1]/span[1]/span[1]')
    texto_estrellas <- html_text(estrellas)
    if(length(texto_estrellas)==0){
      texto_estrellas <- 0
    }else{
      texto_estrellas <- as.numeric(texto_estrellas)
    }
    print(texto_estrellas)
    
    #numero de reseñas se obtiene de un Xpath, En caso de que no existan comentarios, se pone un 0, y si
    #si es que tiene comentarios, se pone el valor correspondiente, el cual debe ser limpiado y luego se
    #imprime
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
    
    #Tabla se saca de la clase "features box section" y se utiliza para extraer los datos de autor, editorial,
    #cantidad de paginas y formato. La tabla se separa  tantas veces como "div" aparezcan, con el fin de 
    #poder extraer los datos por separado.
    Tabla <- html_nodes(sub_pagina, css = ".features-box-section")
    Tabla <- html_nodes(Tabla, css = "div")
    
    #Autor se extrae de la tabla a traves de un IF que identifica si el dato es el autor, en caso de serlo
    #se tomara el valor siguiente ya que este contendra el nombre del autor.
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
    
    #Editorial se extrae de la tabla a traves de un IF que identifica si el dato es la editorial, en caso de
    #serlo se toma el valor siguiente que corresponde al nombre de la editorial
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
    
    #Cantidad de paginas se extrae de la tabla a traves de un IF que identifica si el dato es la cantidad de
    #paginas, en caso de serlo se toma el valor siguiente que corresponde al numero de paginas
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
    
    #Tipo de pasta (FORMATO) se extrae de la tabla a traves de un IF que identifica si el dato es el tipo de 
    #pasta, en caso de serlo se toma el valor siguiente que corresponde al tipo de pasta de cada libro
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
    
    #Se genera un dataframe con la informacion de cada producto extraido
    producto <- data.frame(Titulo = texto_titulos, Autor = texto_autor2, Editorial = texto_editorial2,
                           Precio = texto_precios, Valoracion = texto_estrellas,
                           Cant_coment = texto_cant_comentarios, Cant_pag = texto_cant_pag2, 
                           Formato= texto_formato2, URL = link_producto)
    
    #se agrega el producto extraido recientemente al dataframe
    info_linio <- rbind(info_linio, producto)
  }
  }

#Se genera un csv para guardar toda la informacion de linio
write.csv(info_linio, "informacion_linio.csv")



##############################################################################################################
#----------------------------------- CREAR GRAFICOS PARA BOOKDEPOSITORY --------------------------------------

Info_bookdepository <- read.csv("informacion_bookdepository.csv")



##############################################################################################################
#--------------------------------------- CREAR GRAFICOS PARA LINIO -------------------------------------------

info_linio <- read.csv("informacion_linio.csv")
