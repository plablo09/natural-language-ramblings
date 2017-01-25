local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})
# Leer funciones del archivo
funcs <- c("pkgTest","ftime","parseDetectLanguage","sliceData","buildCloud")
for(f in funcs){
    if(!exists(f, mode="function")) source("code.R")
}

# Librerías requeridas:
reqs <- c("dplyr","tm","SnowballC","wordcloud","textcat","parallel",
          "RColorBrewer","foreach","doParallel")
#Checo si están instaladas
for (pkg in reqs){
    pkgTest(pkg)
}
if (!require("cldr",character.only = TRUE)){
    url <- "http://cran.us.r-project.org/src/contrib/Archive/cldr/cldr_1.1.0.tar.gz"
    pkgFile<-"cldr_1.1.0.tar.gz"
    download.file(url = url, destfile = pkgFile)
    install.packages(pkgs=pkgFile, type="source", repos=NULL)
    unlink(pkgFile)    
}
library(cldr)
# para seleccionar filas al azar, consistentemente
set.seed(12)
#Hago mi cluster
no_cores <- detectCores() -2
cl <- makeCluster(no_cores)
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
clusterExport(cl,"png")
clusterExport(cl,"VectorSource")

#clusterExport(cl,"buildCloud")

# Leo archivo
#setwd('~Dropbox/data/trump/')
print("Leyendo tuits")
#tuits <- read.csv('/home/plablo/Dropbox/merida_ws/trump/trump_mex_2016-11-09T16_52_24_402Z.csv', stringsAsFactors = FALSE)
tuits <- read.csv('data/tuits_sample.csv', stringsAsFactors = FALSE)

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

# Creo las nubes de palabras
paleta <- brewer.pal(8,"Dark2")
lang <- "english"
clusterExport(cl,varlist = c("paleta","lang"))
registerDoParallel(cl)
print("Haciendo nubes en inglés")
foo <- foreach(x = cortes.english, n = names(cortes.english), .combine = c) %dopar%{
    buildCloud(x, n)
}

paleta <- brewer.pal(8,"Dark2")
lang <- "spanish"
clusterExport(cl,varlist = c("paleta","lang"))
print("Haciendo nubes en español")
foo <- foreach(x = cortes.spanish, n = names(cortes.spanish), .combine = c) %dopar%{
    buildCloud(x, n)
}
