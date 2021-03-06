############### An�lisis del contenido de art�culos cient�ficos en pdf#########
################ para seleccionar los articulos m�s pertinentes ###############

if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, pdftools, dplyr, NLP)

#Creaci�n de un tokenizador para poder hacer uso de palabras unitarias 
# o de uniones de dos palabras

UniBigramtokenizer <- function(x){
  unlist(lapply(ngrams(words(x), 1:2), paste, collapse = " "), use.names = FALSE)}

#Selecci�n del directorio de los pdf a analizar
setwd(choose.dir())
files <- list.files(pattern = "pdf$")

#Extracci�n del texto de los pdf en el listado de documentos del directorio
documentos_sciencedirect <- lapply(files, pdf_text)

#Creaci�n del objeto Vcorpus del paquete tm para el almacenamiento del contenido
#de todos los pdf
corp <- VCorpus(VectorSource(documentos_sciencedirect))

#Eliminaci�n de signos de puntuaci�n en el contenido de los pdf
corp <- tm_map(corp, removePunctuation, ucp = TRUE)

#Creaci�n del diccionario de unigramas y bigramas para encontrar sus frecuencias 
#en el pdf

mi_diccionario <- c("office","center","agency",
              "organizational structure",
              "spin-off","spinoff","technology transfer",
              "technology licensing","technological licensing",
              "licensing","technology commercialization",
              "technological commercialization","commercialization")

#Generaci�n de la matriz de t�rminos del documento (Term Document Matrix)
#Donde se especifican las frecuencias por palabras del documento

documentos_sciencedirect.tdm <- TermDocumentMatrix(corp, 
                                   control = 
                                     list(#stopwords =TRUE,
                                          removeNumbers = TRUE,
                                          tolower = TRUE,
                                          #stemming = TRUE,
                                          #removePunctuation = TRUE,
                                          dictionary = mi_diccionario,
                                          tokenize = UniBigramtokenizer))

#Organizaci�n de las frecuencias de aparici�n de las palabras del diccionario

frecuencia_diccionario<-as.data.frame(as.matrix(documentos_sciencedirect.tdm))
frecuencia_diccionario <- data.frame(t(frecuencia_diccionario))

#Filtro de documentos para aquellos que tengan m�s de 30 apariciones de
# alguno de los t�rminos del diccionario

minimo_resultados <- 30
frecuencias_finales <- subset(frecuencia_diccionario,
                              (agency >= minimo_resultados |
                                 center >= minimo_resultados |
                                 office >= minimo_resultados) |
                                
                              (organizational.structure >= minimo_resultados |
                                 spin.off >= minimo_resultados |
                                 spinoff >= minimo_resultados) |
                                
                              (technological.commercialization >= minimo_resultados |
                                 commercialization >= minimo_resultados | 
                                 technology.commercialization >= minimo_resultados) |
                                
                              (technological.licensing >= minimo_resultados | 
                                 technology.licensing >= minimo_resultados |
                                 licensing >= minimo_resultados) |
                                
                              technology.transfer >= minimo_resultados)

# Extracci�n de los nombres de los pdf depurados de acuerdo al filtro
pdfs_para_leer<-files[as.numeric(row.names(frecuencias_finales))]

new_dir <- "pdf para leer"

dir.create(new_dir, recursive = TRUE)

#Copia de los pdf depurados en una nueva carpeta para su lectura
for(file in pdfs_para_leer) {
  
  file.copy(file, new_dir)
}
