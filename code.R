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
    d.corpus = Corpus(VectorSource(d$Texto))
    d.corpus = tm_map(d.corpus, PlainTextDocument)
    d.corpus = tm_map(d.corpus, removePunctuation)
    d.corpus = tm_map(d.corpus, PlainTextDocument)
    d.corpus = tm_map(d.corpus, content_transformer(tolower))
    d.corpus = tm_map(d.corpus, PlainTextDocument)
    if(lang == "english"){
        d.corpus = tm_map(d.corpus, removeWords, stopwords("english"))
    }else {
        d.corpus = tm_map(d.corpus, removeWords, stopwords("spanish"))
    }
    d.corpus = tm_map(d.corpus, removeWords, c("trump","donald",
                                               "realdonaldtrump","amp",
                                               "httptcolrziuoh6tk"))
    f.name = paste(lang, "wordcloud", nombre, sep = "_")
    f.name = paste(f.name,"png", sep = ".")
    f.name = paste("images", f.name, sep = "/")
    png(f.name, width=1280,height=800)
    wordcloud(d.corpus, max.words = 100, scale=c(6,.3), rot.per = .15,
              colors = paleta, vfont=c("sans serif","plain"),
              random.order = FALSE)
    dev.off()
    }
    return(nrow(d))
}