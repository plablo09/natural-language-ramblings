# Funciones varias

# Checa si está instalado un paquete
pkgTest <- function(x)
  {
    if (!require(x,character.only = TRUE))
    {
      install.packages(x,dep=TRUE)
        if(!require(x,character.only = TRUE)) stop("Package not found")
    }
  }



# Función para formatear el timestamp
ftime <- function(s){
  s = gsub(":", ".", s)
  s = gsub("T", " ", s)
}

# Leer el resultado de detectLanguage y regresar un string con el lenguaje o Nulo
# en caso de no haber match
parseDetectLanguage <- function(s) {
    df = detectLanguage(s)
    result = df["detectedLanguage"][[1]]
    return(result)
}


testDetectLanguage <- function(s){
    df = detectLanguage(s)
    print(df["detectedLanguage"][[1]])
    print(class(df["detectedLanguage"][[1]]))
}


# Regresar una lista de dataframes con los cortes de tiempo
sliceData <- function(data, fechas){
    cortes = list()
    cont = 1
    for(f in fechas){
        if(cont < length(fechas)){
            d = as.Date(f)
            corte = subset(data, fecha >= d & fecha <= as.Date(fechas[cont + 1]))
            cortes[[f]] = corte
        }
        cont = cont + 1
    }
    return(cortes)    
}


# Hace las nubes y las guarda en pngs. Recibe los datos (cortes temporales)
# y la paleta
buildCloud <- function(d,nombre){
    print(nombre)
    if(nrow(d) > 10){
        myStopWords <- c("trump", "donald", "realdonaldtrump",
                        "amp", "httptcolrziuoh6tk")
        d.corpus <- Corpus(VectorSource(d$Texto))
        d.corpus <- tm_map(d.corpus, content_transformer(tolower), mc.cores = 1)
        if(lang == "english"){
            d.corpus <- tm_map(d.corpus, removeWords,stopwords("english"),
                              mc.cores = 1)
        }else {
            d.corpus <- tm_map(d.corpus, removeWords,stopwords("spanish"),
                              mc.cores = 1)
        }
        d.corpus <- tm_map(d.corpus, removeWords, myStopWords, mc.cores = 1)
        d.corpus <- tm_map(d.corpus, removePunctuation, mc.cores = 1)
        d.corpus <- tm_map(d.corpus, PlainTextDocument, mc.cores = 1)
        f.name <- paste(lang, "wordcloud", nombre, sep = "_")
        f.name <- paste(f.name,"png", sep = ".")
        f.name <- paste("images", f.name, sep = "/")
        png(f.name, width=1280,height=800)
        wordcloud(d.corpus, max.words = 100, scale=c(12,1), rot.per = .15,
                  colors = paleta, random.order = FALSE)
        dev.off()
    }
    return(nrow(d))
}
