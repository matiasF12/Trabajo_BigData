#################################################################################
############################# TRABAJO WEB SCRAPPING #############################

#Instalar paquetes
install.packages("Rvest")
install.packages("gdata")

#librerias
library("rvest")
library("gdata")

setwd("~/Github/Trabajo_BigData")
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
  Url <-paste("https://www.linio.cl/c/literatura-y-novelas/literatura-juvenil?qid=f83321c78c0344926435195bd01d0acf&oid=RO169BK0FZI35LACL&position=34&sku=RO169BK0FZI35LACL&page=", nro_pag, sep = "")
  
  #Descarga de la pagina
  linio <- read_html(Url)
  
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
    texto_autor2 <- NA
    for(variable in Tabla){
      texto_autor <- html_text(variable)
      if(autor){
        print(texto_autor)
        autor <- F
        texto_autor2 <- texto_autor
        if(texto_autor2=="\n            ISBN\n            9789871208968\n          "){
          texto_autor2 <- NA
        }
      }
      if(texto_autor=="Autor"){
        autor <- T
      }
    }
    
    #Editorial se extrae de la tabla a traves de un IF que identifica si el dato es la editorial, en caso de
    #serlo se toma el valor siguiente que corresponde al nombre de la editorial
    editorial <- F
    texto_editorial2 <- NA
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
                           Formato= texto_formato2, Url = link_producto)
    
    #se agrega el producto extraido recientemente al dataframe
    info_linio <- rbind(info_linio, producto)
  }
  }

#Se genera un csv para guardar toda la informacion de linio
write.csv(info_linio, "informacion_linio.csv")



##############################################################################################################
#----------------------------------- CREAR GRAFICOS PARA BOOKDEPOSITORY --------------------------------------

library(tidyverse)

setwd("~/Github/Trabajo_BigData")

#Carga de base de datos
Info_bookdepository <- read.csv("informacion_bookdepository.csv")

#se agrega variable sitio web para poder utilizar una base combinada despues y variable mill 
#comentarios para que sea mas amigable el numero
Info_bookdepository <- mutate(Info_bookdepository, Sitio_web= "Bookdepository")
Info_bookdepository <- mutate(Info_bookdepository, mill_Comentarios=Cant_coment/1000000)
Info_bookdepository$Formato <- gsub("Paperback", "Tapa Blanda", Info_bookdepository$Formato)
Info_bookdepository$Formato <- gsub("Hardback", "Tapa Dura", Info_bookdepository$Formato)

#se eliminan aquellos libros extraidos los cuales no tienen precio (porque no estan disponibles)
#y la editorial se pone en mayuscula
Info_bookdepository2 <- Info_bookdepository%>%filter(Precio!='is.na')
Info_bookdepository2$Editorial <- str_to_upper(Info_bookdepository2$Editorial, locale = "en")



########## GRAFICO 1 ##########

#Para este grafico se quiere ver cuales son los libros mas populares, considerando
#la cantidad de comentarios que estos tengan, dado que existen libros que tienen valoración
#5 estrellas pero solo tienen un comentario.

#10 libros mas populares (valoracion)
top_bookde <- Info_bookdepository2%>% arrange(desc(Cant_coment,Valoracion))%>%
  filter(row_number()<=10)

#grafico valoracion de libros
ggplot(top_bookde, aes(x=Titulo, y=mill_Comentarios)) + 
  geom_point(aes(size=Valoracion, color=mill_Comentarios))+ 
  geom_segment(aes(x=Titulo, xend=Titulo, y=0, yend=mill_Comentarios), color="skyblue") + 
  coord_flip()+ ggtitle("Libros más valorados")+ theme_classic()

#En este grafico se puede ver que los libros mas y mejor valorados son de la saga de 
#"Harry Potter" alcanzando una valoracion de 4.5 puntos y superando los 7 millones de comentarios



########## GRAFICO 2 ##########

#Se ve la distribucion de los precios de bookdepository. En primer lugar están todos (contiene
#outliers) y el segundo tiene un acercamiento, por ende no se ven todos los outliers

#grafico con los precios (con outliers)
ggplot(Info_bookdepository2, aes(x=Sitio_web, y=Precio))+ 
  geom_boxplot(fill="skyblue", alpha=0.4) +
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="red", fill="red")+
  theme_classic() 

#grafico con acercamiento a boxplot
ggplot(Info_bookdepository2, aes(x=Sitio_web, y=Precio))+ 
  geom_boxplot(fill="skyblue", alpha=0.4) +
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="red", fill="red")+
  theme_classic() + scale_y_continuous(limits=c(5000, 35000))

summary(Info_bookdepository2$Precio)

#Para este caso, se puede notar que la mediana del precio de los libros en esta plataforma se
#encuentra en los 15.300 pesos aproximadamente. Tambien se ve una gran cantidad de outliers,
#que estan por sobre los 30000 pesos



########## GRAFICO 3 ##########

#Este grafico muestra los autores más valorados, obtenido mediante un promedio de las valoraciones
#de los libros disponibles en este sitio web y teniendo en consideracion la cantidad de libros
#disponibles, dado que como paso anteriormente, algunos autores tienen solo 1 libro que fue bien
#valorado, y esto genera problemas si se compara con otros autores que tienen mayor cantidad de 
#publicaciones.

#seleccion de variables, filtros y datos relevantes
Autores_bookde <-select(Info_bookdepository2,Autor,Valoracion, Cant_coment)%>%
  filter(Autor!="")%>%group_by(Autor)%>%
  summarise(promedio=mean(Valoracion), n=n(), sd(Valoracion))%>%  arrange(desc(promedio,n))%>%
  filter(promedio>=3.5)%>%filter(n>=10)
  
#grafico para autores
ggplot(Autores_bookde, aes(x=promedio, y=n))+ geom_point(size=3, color="#69b3a2")+
  geom_text(label=Autores_bookde$Autor, nudge_x = 0.07, nudge_y = 0.07, check_overlap = F)+
  xlab("Promedio valoración")+ ylab("Cantidad de libros del autor")+ 
  ggtitle("Autores más valorados y con mayor cantidad de libros")+ theme_classic()


#El autor con mayor cantidad de publicaciones es "Mary Pope Osborne", sin embargo, su 
#valoracion es inferior a 3.8. En el caso contrario autores con poca cantidad de libros
# como "Dav Pilkey" tiene una calificacion superior a 4.2. Dicho lo anterior, los que
#tienen unos 15 libros y nota superior a 4 son los mas y mejores valorados dado el 
#equilibrio que hay



########## GRAFICO 4 ##########
#Para este caso, se busca identificar las editoriales con mayor cantidad de publicaciones en 
#esta plataforma. 

#Editoriales con mayor cantidad de libros (para esto se cuentan los libros y luego se muestran
#solo las 15 editoriales con mayor cantidad de publicaciones)
editorial_bookde <- Info_bookdepository2%>%group_by(Editorial)%>%
  summarise(n=n())%>%arrange(desc(n))%>%filter(row_number()<=15)


ggplot(editorial_bookde,aes(x=Editorial, y=n)) + 
  geom_bar(stat = "identity", width = 0.7, color="blue", fill=rgb(0.1,0.4,0.5,0.7)) +
  coord_flip()+ theme_classic()+ ggtitle("Editoriales con más publicaciones")+ 
  ylab("Publicaciones")


#En este grafico se puede ver las editoriales o publicaciones "independientes" son las que
#tienen una mayor cantidad de libros en la plataforma.

##############################################################################################################
#--------------------------------------- CREAR GRAFICOS PARA LINIO -------------------------------------------

#Carga de base de datos 
linio <- read.csv("informacion_linio.csv")

#Se agrega la variable sitio web para despues poder combinar ambas bases de datos y poder
#obtener graficos que requieran identificar los libros de linio y bookdepository. Tambien la 
#variable mill_coment que es la cantidad de comentarios dividio en 1 millon con el fin de tener
#una mejor visualizacion en los graficos.
linio <- mutate(linio, Sitio_web = "Linio")
linio<- mutate(linio, mill_Comentarios=Cant_coment/1000000)

#Limpieza para variable autor
{linio$Autor <- str_trim(linio$Autor, side = "both")
linio$Autor <- str_to_upper(linio$Autor, locale = "es")
linio$Autor <- chartr("ÁÉÍÓÚ", "AEIOU", linio$Autor)
linio$Autor <- chartr("ÀÈÌÒÙ", "AEIOU", linio$Autor)
linio$Autor <- gsub("-", " ", linio$Autor)
linio$Autor <- gsub("[.]", "", linio$Autor)
linio$Autor <- gsub("[,]", "", linio$Autor)
linio$Autor <- gsub("AAVV", "VVAA", linio$Autor)
linio$Autor <- gsub("ALLAN POEEDGAR", "ALLAN POE EDGAR", linio$Autor)
linio$Autor <- gsub("JOANNE", "J", linio$Autor)
linio$Autor <- gsub("J K", "JK", linio$Autor)
linio$Autor <- gsub("JKROWLING", "ROWLING", linio$Autor)
linio$Autor <- gsub("JK", "", linio$Autor)
linio$Autor <- str_trim(linio$Autor, side = "both")
linio$Autor <- gsub("JOSEFA ARAOS JUNE GARCIA", "JOSEFA ARAOS Y JUNE GARCIA", linio$Autor)
linio$Autor <- gsub("JOSEFA ARAOS / JUNE GARCIA", "JOSEFA ARAOS Y JUNE GARCIA", linio$Autor)
linio$Autor <- gsub("KARINA & MARINA", "KARINA Y MARINA", linio$Autor)
linio$Autor <- gsub("GABRIEL GARCIAÊMARQUEZ", "GARCIA MARQUEZ GABRIEL", linio$Autor)
linio$Autor <- gsub("GREENWELL JESSICA", "GREENWELL JESSI", linio$Autor)
linio$Autor <- gsub("REVEIJO CARLOS", "REVIEJO CARLOS", linio$Autor)
linio$Autor <- gsub("MORENO / RODRIGUEZ", "MORENO RODRIGUEZ", linio$Autor)
select(linio, Autor)%>%count(Autor)%>%arrange(desc(n))
}

#Fue necesario limpiar la variable antes de realizar el grafico, debido a que la
#misma editorial estaba escrita de diferentes formas.

{linio$Editorial <- str_to_upper(linio$Editorial, locale = "es")
linio$Editorial <- chartr("ÁÉÍÓÚ", "AEIOU", linio$Editorial)
linio$Editorial <- gsub("EDITORIAL", "", linio$Editorial)
linio$Editorial <- gsub("EDITORAS", "", linio$Editorial)
linio$Editorial <- gsub("EDITORES", "", linio$Editorial)
linio$Editorial <- gsub("EDICIONES URANO", "URANO", linio$Editorial)
linio$Editorial <- gsub("EMPRESA EDITORA", "", linio$Editorial)
linio$Editorial <- gsub("-", " ", linio$Editorial)
linio$Editorial <- gsub("ZIGZAG", "ZIG ZAG", linio$Editorial)
linio$Editorial <- gsub("[.]", " ", linio$Editorial)
linio$Editorial <- gsub(" SA", "", linio$Editorial)
linio$Editorial <- gsub(" S A", "", linio$Editorial)
linio$Editorial <- gsub("PLANETALECTOR", "PLANETA LECTOR", linio$Editorial)
linio$Editorial <- gsub("[(]", "", linio$Editorial)
linio$Editorial <- gsub("[)]", "", linio$Editorial)
linio$Editorial <- gsub("CESMA", "", linio$Editorial)
linio$Editorial <- gsub("&", "Y", linio$Editorial)
linio$Editorial <- gsub("VYR", "V Y R", linio$Editorial)
linio$Editorial <- gsub(" AND ", " Y ", linio$Editorial)
linio$Editorial <- gsub("GRUPO", "", linio$Editorial)
linio$Editorial <- gsub("PENGUIN RANDOM HOUSE", "PENGUIN",linio$Editorial)
linio$Editorial <- gsub( "PENGUIN", "PENGUIN RANDOM HOUSE", linio$Editorial)
linio$Editorial <- gsub("CHILENA", "", linio$Editorial)
linio$Editorial <- gsub("CHILE", "", linio$Editorial)
linio$Editorial <- gsub("CHILENA", "", linio$Editorial)
linio$Editorial <- gsub("JUNIOR", "", linio$Editorial)
linio$Editorial <- gsub("LECTOR", "", linio$Editorial)
linio$Editorial <- gsub("COMIC", "", linio$Editorial)
linio$Editorial <- gsub("LOQUELEO", "", linio$Editorial)
linio$Editorial <- gsub("JUVENIL", "", linio$Editorial)
linio$Editorial <- gsub("ALFAGUARA I", "ALFAGUARA", linio$Editorial)
linio$Editorial <- gsub("ALFAGUARA INFANTIL JUVENIL", "ALFAGUARA", linio$Editorial)
linio$Editorial <- gsub("ALFAGUARA J", "ALFAGUARA", linio$Editorial)
linio$Editorial <- gsub("ALFAGUARANFANTIL", "ALFAGUARA", linio$Editorial)
linio$Editorial <- gsub("INFANTIL Y", "", linio$Editorial)
linio$Editorial <- gsub("BOLSILLO", "", linio$Editorial)
linio$Editorial <- gsub("INFANTIL", "", linio$Editorial)
linio$Editorial <- str_trim(linio$Editorial, side = "both")

select(linio, Editorial) %>% count(Editorial)%>%arrange(desc(n))
}



########## GRAFICO 1 ##########

#Para este grafico se quiere ver cuales son los libros más populares, considerando
#la cantidad de comentarios que estos tengan, dado que existen libros que tienen valoración
#5 estrellas pero solo tienen un comentario.

#10 libros mas populares (valoracion)
top_linio <- linio %>%select(Titulo, Valoracion, Cant_coment)%>%
  arrange(desc(Cant_coment, Valoracion))%>% filter(row_number()<= 10)

ggplot(top_linio, aes(x=Titulo, y=Cant_coment))+
  geom_point(aes(size=Valoracion), color="darkorange1")+
  geom_segment(aes(x=Titulo, xend=Titulo, y=0, yend=Cant_coment), color="orange1")+
  coord_flip()+ ggtitle("Libros más y mejor valorados")+ theme_classic()

#En este grafico se puede evidenciar que el libro con mas comentarios y con mejor 
#valoracion es el de "Sol de media noche", seguido por un libro de la saga de "Harry Potter" 
#y otro de "Juego de Tronos". Ademas, es posible notar que todos estos libros alcanzan la 
#valorizacion maxima.

########## GRAFICO 2 ##########

#Se ve la distribucion de los precios de linio. En primer lugar estan todos 
#(contiene outliers) y el segundo tiene un acercamiento, por ende no se ven todos los 
#outliers

#grafico con los precios (con outliers)
ggplot(linio, aes(x=Sitio_web, y=Precio))+
  geom_boxplot(fill="darkorange1", alpha=0.6) +
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="darkorange1", 
               fill="darkorange1")+  theme_classic()

#grafico con acercamiento a boxplot
ggplot(linio, aes(x=Sitio_web, y=Precio))+
  geom_boxplot(fill="darkorange1", alpha=0.6) +
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="darkorange1", 
               fill="darkorange1")+ theme_classic() + scale_y_continuous(limits=c(5000, 35000))

summary(linio$Precio)

#En este grafico se puede notar la distribucion de los precios de linio, el cual 
#tiene una media de $14.000 pesos. Tambien se puede ver una cantidad  importante de outliers.


########## GRAFICO 3 ##########

#Este grafico muestra los autores mas valorados, obtenido mediante un promedio de las valoraciones
#de los libros disponibles en este sitio web y teniendo en consideracion la cantidad de libros
#disponibles, dado que como paso anteriormente, algunos autores tienen solo 1 libro que fue bien
#valorado, y esto genera problemas si se compara con otros autores que tienen mayor cantidad de 
#publicaciones.


Autores_linio <-select(linio, Autor, Valoracion, Cant_coment) %>% filter(Autor!="is.na")%>%
  group_by(Autor) %>% summarise(promedio=mean(Valoracion), n=n(), sd(Valoracion)) %>%
  arrange(desc(n)) %>% filter(promedio >= 2) %>% filter(n > 3)


#grafico para autores mas valorados 
ggplot(Autores_linio, aes(x=promedio, y=n))+ geom_point(size=4,color="orange", alpha=0.3)+
  geom_text(label=Autores_linio$Autor, nudge_x = 0.1, nudge_y = -0.35, check_overlap = F)+
  xlab("Promedio valoración")+ ylab("Cantidad de libros del autor")+
  ggtitle("Autores más valorados y con mayor cantidad de libros")+ theme_classic()


#De la misma forma que en Bookdepository, es importante considerar tanto la valoracion 
#de cada autor como la cantidad de libros evaluados. En este aspecto J.K. Rowling tiene 
#una gran cantidad de libros sin embargo, tiene una puntuacion inferior a 2.5. Por otro 
#lado, autores como James Dashne tienen menos libros (5) pero una valoracion mejor, 
#superando los 3.5 puntos.

########## GRAFICO 4 ##########

#Para este caso, se busca identificar las editoriales con mayor cantidad de publicaciones en 
#esta plataforma. 

#Editoriales con mayor cantidad de libros (para esto se cuentan los libros y luego se muestran
#solo las 15 editoriales con mayor cantidad de publicaciones)
editorial_linio <- linio%>%filter(Editorial!="is.na")%>%group_by(Editorial)%>%
  summarise(n=n())%>%arrange(desc(n))%>%filter(row_number()<=15)

#grafico para editoriales con mayor cantidad de publicaciones
ggplot(editorial_linio,aes(x=Editorial, y=n)) + 
  geom_bar(stat = "identity", width = 0.7, color="darkorange1", fill=rgb(1,0.6,0,0.6)) +
  coord_flip()+ theme_classic()+ ggtitle("Editoriales con más publicaciones")+ 
  ylab("Publicaciones")


#Para el caso de linio, la editorial "Vicens Vives" es la que tiene una mayor cantidad de 
#libros, contrastando con bookdepository que la mayor cantidad era de editoriales 
#independientes. A esta editorial le sigue (aunque no de cerca) "Usborne" y "Penguin Random 
#House".





##############################################################################################################
#--------------------------------------- CREAR GRAFICOS COMBINADOS -------------------------------------------
#se combinan ambas bases de datos
libros <-  rbind(Info_bookdepository2, linio)


########## GRAFICO 1 ##########

#grafico que muestra la ditribucion de los precios para bookdepository y linio

#Precios para linio y bookde con outliers
ggplot(libros, aes(x=Sitio_web, y=Precio))+ 
  geom_boxplot(fill="slateblue", alpha=0.2) +
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="red", fill="red")+
  theme_classic()

#Precios para linio y bookde sin outliers
ggplot(libros, aes(x=Sitio_web, y=Precio))+ 
  geom_boxplot(fill="slateblue", alpha=0.2)+ 
  stat_summary(fun= mean, geom = "point", shape=20, size=4, color="red", fill="red")+
  theme_classic() + scale_y_continuous(limits=c(5000, 60000)) + 
  scale_fill_brewer(palette = "BuPu") + xlab("Página web")+ ylab("Precios")+
  ggtitle("Distribución de precios por página")+ theme_classic()

#En este grafico se puede observar que la distribucion de precios de linio es mayor que la de
#bookdepository, sin embargo, sus medias son similares. Por otro lado, bookdepository tiene 
#una gran cantidad de outliers  en comparacion a linio.


########## GRAFICO 2 ##########

#instalar librerias
install.packages("viridis")
install.packages("hrbrthemes")

#ejecutar librerias
library(viridis)
library(hrbrthemes)


#Se ve la cantidad de libros para el formato tapa dura y tapa blanda (bookdepository tiene mas 
#formatos pero estos son menores en comparacion a la cantidad que presenta el formato de tapa 
#dura y blanda)

libros_formato <- libros%>%filter((Formato=="Tapa Blanda" | Formato=="Tapa Dura"))%>%
  group_by(Sitio_web, Formato)%>% summarise(cant_formato=n())

#grafico
ggplot(libros_formato, aes(fill=Sitio_web, y=cant_formato, x=Formato))+
  geom_bar(position = "dodge", stat="identity")+
  scale_fill_viridis(discrete=T, option = "E")+
  ggtitle("Cantidad de libros por formato en cada página") + facet_wrap(~Sitio_web) +
  theme_ipsum() + theme(legend.position = "none")+ xlab("Sitio web") + ylab("Cantidad de libros")


#Es posible notar que ambas paginas tienen disponibles una gran cantidad de libros de tapa blanda,
#que supera con creces a los de tapa dura. Ambas paginas tienen un comportamiento similar.


######################## CARACTERISTICAS DE LOS LIBROS #####################

#Autores
select(linio, Autor) %>% filter(Autor != "is.na") %>% count(Autor) %>% arrange(desc(n)) %>% 
  filter(row_number()<=5)

select(Info_bookdepository2, Autor) %>% filter(Autor != "") %>% count(Autor) %>% 
  arrange(desc(n)) %>% filter(row_number()<=5)

#Precio
summary(libros$Precio)

#Cantidad de paginas
summary(libros$Cant_pag)

######################################################################################
#---------------------------- DESCRIPCION DE LAS VARIABLES --------------------------#

# -x: Numero de fila correspondiente a cada observacion
# -Titulo: Nombre del libro
# -Autor: Nombre de autor o autores de cada libro
# -Precio: Precios de los libros sin descuentos
# -Editorial: Nombre de la editorial de cada libro
# -Valoracion:Valoracion (estrellas) de cada libro, escala de 1 a 5
# -Cant_coment:Cantidad de comentarios que conforman la valoracion de un libro
# -Cant_pag: Cantidad de paginas de cada libro
# -Formato: Formato de cada libro (principalmente si es tapa dura o tapa blanca)






