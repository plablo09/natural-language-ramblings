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

# The following functions are designed to run with %dopar%

removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)

cleanCorpus <- function(d, nombre, language, myStopWords){
    if(!is.null(d)){
        d = tm_map(d, content_transformer(tolower), mc.cores = 1)
        if(language == "english"){
            d = tm_map(d, removeWords,stopwords("english"),
                              mc.cores = 1)
        }else {
            d = tm_map(d, removeWords,stopwords("spanish"),
                              mc.cores = 1)
        }
        d = tm_map(d, removeWords, myStopWords, mc.cores = 1)
        d = tm_map(d, removePunctuation, mc.cores = 1)
        d = tm_map(d, removeNumbers, mc.cores = 1)
        d = tm_map(d, content_transformer(removeURL), mc.cores = 1)
        d = tm_map(d, PlainTextDocument, mc.cores = 1)
        return(d)
    }else{
        return(NULL)
    }
}

# Hace las nubes y las guarda en pngs. Recibe los datos (corpus)
# y la paleta
buildCloud <- function(d, nombre, lang){
    print(nombre)
    if(is(d, "Corpus")){
        f.name <- paste(lang, "wordcloud", nombre, sep = "_")
        f.name <- paste(f.name,"png", sep = ".")
        f.name <- paste("images", f.name, sep = "/")
        png(f.name, width=1280,height=800)
        wordcloud(d, max.words = 100, scale=c(12,1), rot.per = .15,
                  colors = paleta, random.order = FALSE)
        dev.off()
        return(TRUE)
    }else{
        return(FALSE)
    }    
}

# Hace las TDMs para cada corte y las guarda en una lista
buildTDM <- function(d){
    if(is(d, "Corpus")){
        tdm <- TermDocumentMatrix(d, control = list(wordLengths = c(3, Inf)))
        return(tdm)
        
    }else{
        return(NULL)
    }
}


buildClusters <- function(d, nombre, lang){
    if(!is.null(d)){
        tdm <- removeSparseTerms(d, sparse = 0.95)
        m <- as.matrix(tdm)
        distMatrix <- dist(scale(m))
        fit <- hclust(distMatrix, method = "ward.D")
        f.name <- paste(lang, "clusters", nombre, sep = "_")
        f.name <- paste(f.name,"png", sep = ".")
        f.name <- paste("images", f.name, sep = "/")
        png(f.name, width=1280,height=800)
        plot(fit)
        dev.off()
        return(TRUE)
    }else{
        return(FALSE)
    }
}
