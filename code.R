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

cleanCorpora <- function(d, nombre, language, myStopWords){
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

cleanCorpus <- function(d, language, myStopWords){
    d = tm_map(d, content_transformer(tolower))
    if(language == "english"){
        d = tm_map(d, removeWords,stopwords("english"),
                          mc.cores = 1)
    }else {
        d = tm_map(d, removeWords,stopwords("spanish"),
                          mc.cores = 1)
    }
    d = tm_map(d, removeWords, myStopWords)
    d = tm_map(d, removePunctuation)
    d = tm_map(d, removeNumbers)
    d = tm_map(d, content_transformer(removeURL))
    d = tm_map(d, PlainTextDocument)
    return(d)
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

mergeText <- function(d){
    return(paste(unlist(d$Texto), collapse = " "))
}


topicmodels_json_ldavis <- function(fitted, corpus, doc_term){
    # Required packages
    library(topicmodels)
    library(dplyr)
    library(stringi)
    library(tm)
    library(LDAvis)

    # Find required quantities
    phi <- posterior(fitted)$terms %>% as.matrix
    theta <- posterior(fitted)$topics %>% as.matrix
    vocab <- colnames(phi)
    doc_length <- vector()
    for (i in 1:length(corpus)) {
        temp <- paste(corpus[[i]]$content, collapse = ' ')
        doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
    }
    temp_frequency <- inspect(doc_term)
    freq_matrix <- data.frame(ST = colnames(temp_frequency),
                              Freq = colSums(temp_frequency))
    rm(temp_frequency)

    # Convert to json
    json_lda <- LDAvis::createJSON(phi = phi, theta = theta,
                            vocab = vocab,
                            doc.length = doc_length,
                            term.frequency = freq_matrix$Freq)

    return(json_lda)
}
