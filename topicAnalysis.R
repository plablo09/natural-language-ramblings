#!/usr/bin/env Rscript

data_file <- "data/tuits_sample.csv"
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})
# Leer funciones del archivo
funcs <- c("pkgTest","ftime","parseDetectLanguage","sliceData","buildCloud",
           "cleanCorpus", "mergeText")
for(f in funcs){
    if(!exists(f, mode="function")) source("code.R")
}

# Librerías requeridas:
reqs <- c("dplyr","tm","SnowballC","wordcloud","textcat","parallel",
          "RColorBrewer","foreach","doParallel","topicmodels","rlist", "LDAvis")
#Checo si están instaladas
for (pkg in reqs){
    pkgTest(pkg)
}


library(cldr)
# para seleccionar filas al azar, consistentemente
set.seed(12)
#Hago mi cluster
no_cores <- detectCores() -2
cl <- makeCluster(no_cores)
registerDoParallel(cl)
# clusterEvalQ(cl, runif(10))
clusterExport(cl,"textcat")
clusterExport(cl,"detectLanguage")
clusterExport(cl,"parseDetectLanguage")
clusterExport(cl,"Corpus")
clusterExport(cl,"tm_map")
clusterExport(cl,"PlainTextDocument")
clusterExport(cl,"removePunctuation")
clusterExport(cl,"content_transformer")
clusterExport(cl,"wordcloud")
clusterExport(cl,"removeWords")
clusterExport(cl,"removeNumbers")
clusterExport(cl,"png")
clusterExport(cl,"VectorSource")
clusterExport(cl,"stopwords")
clusterExport(cl,"buildTDM")
clusterExport(cl,"TermDocumentMatrix")
clusterExport(cl,"buildClusters")
clusterExport(cl,"hclust")
clusterExport(cl,"removeSparseTerms")
#clusterExport(cl,"buildCloud")

# Leo archivo
#setwd('~Dropbox/data/trump/')
print("Leyendo tuits")
#tuits <- read.csv('/home/plablo/Dropbox/merida_ws/trump/trump_mex_2016-11-09T16_52_24_402Z.csv', stringsAsFactors = FALSE)
tuits <- read.csv(data_file, stringsAsFactors = FALSE)

# Formateo el timestamp
print("Formateando fechas")
tuits$Fecha_tweet <- parLapply(cl,tuits$Fecha_tweet, ftime)
# Cambio el tipo de la columna con la fecha 
tuits$Fecha_tweet <- as.POSIXct(strptime(tuits$Fecha_tweet, "%Y-%m-%d %H.%M.%OS"),format="%Y-%m-%d %H.%M.%OS")
# Renombro la columna
names(tuits)[names(tuits) == 'Fecha_tweet'] <- 'tstamp'
# Extraigo la fecha
tuits$fecha <- as.Date(tuits$tstamp)

print("Clasificando lenguages")
# Clasifico inglés y español
tuits$lang <- parLapply(cl,tuits$Texto,parseDetectLanguage)

#Por alguna razón, la función regresa lista y no texto. Lo convertimos
tuits$lang <- as.character(tuits$lang)


# Separo en inglés y español
english <- subset(tuits, lang == "ENGLISH")
spanish <- subset(tuits, lang == "SPANISH")

print("Separando en intervalos")
fechas = c("2014-04-03","2015-06-16","2015-06-25","2015-06-30",
           "2015-07-06","2015-07-23","2015-12-07" ,"2016-05-03",
           "2016-07-18","2016-07-25", "2016-08-26","2016-08-31",
           "2016-09-01","2016-10-03","2016-10-07","2016-10-08",
           "2016-10-10","2016-10-15","2016-10-19","2016-11-09")

# Separo en intervalos:
cortes.spanish <- sliceData(spanish, fechas)
cortes.english <- sliceData(english, fechas)

print("Creando y limpiando corpus")

corpus.english <- foreach(x = cortes.english) %dopar%{
    if(nrow(x) > 10){
        return(Corpus(VectorSource(x$Texto)))
    }else{
        return(NULL)
    }
}


corpus.english <- foreach(x = corpus.english, n = names(cortes.english)) %dopar%{
    myStopWords <- c("trump", "donald", "realdonaldtrump",
                 "amp", "httptcolrziuoh6tk", "rt")
    cleanCorpora(x, n, "english", myStopWords)
}

names(corpus.english) <- names(cortes.english)

#Quitamos los elementos nulos:
corpus.english <- list.clean(corpus.english, fun = is.null, recursive=FALSE)
#guardo los nombres para usarlos después:
names.english <- names(corpus.english)

corpus.spanish <- foreach(x = cortes.spanish) %dopar%{
    if(nrow(x) > 10){
        return(Corpus(VectorSource(x$Texto)))
    }else{
        return(NULL)
    }
}


corpus.spanish <- foreach(x = corpus.spanish, n = names(cortes.spanish)) %dopar%{
    myStopWords <- c("trump", "donald", "realdonaldtrump",
                 "amp", "httptcolrziuoh6tk", "rt")
    cleanCorpora(x, n, "spanish", myStopWords)
}

names(corpus.spanish) <- names(cortes.spanish)

## Ya con los tuits limpios, vamos a crear un sólo corpus para cada corte temporal

## Una lista con todo el texto da cada corpus unido en un solo character
merged.english = list()
for (name in names(corpus.english)){
    l <- lapply(corpus.english[[name]], as.character)
    merged.english[[name]] = paste(l, collapse = " ")
}

## Reescribo el corpus
corpus.english <- Corpus(VectorSource(merged.english))

## Calculo la DTM (y ya en eso le pongo nombres a los documentos)
dtm <- DocumentTermMatrix(corpus.english)
rownames(dtm) <- names.english

## Ahora sí podemos correr el modelo

#Set parameters for Gibbs sampling
burnin <- 4000
iter <- 2000
thin <- 500
seed <-list(2003,5,63,100001,765)
nstart <- 5
best <- TRUE

k <- 10


ldaOut <-LDA(dtm,k, method="Gibbs", control=list(nstart=nstart, seed = seed, best=best, burnin = burnin, iter = iter, thin=thin))
