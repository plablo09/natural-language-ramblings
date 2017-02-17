for (d in names(cortes.english)){
    print(d)
    print(nrow(cortes.english[[d]]))
}


un.corpus = corpus.english[["2016-08-31"]]
dtm <- DocumentTermMatrix(un.corpus, control = list(wordLengths = c(3, Inf)))
tdm <- TermDocumentMatrix(un.corpus, control = list(wordLengths = c(3, Inf)))
# top 100 terms
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = TRUE)
top.100 <- head(v,100)
term.freq <- rowSums(as.matrix(top.100))
#term.freq <- subset(term.freq, term.freq >=300)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") +coord_flip()
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix, method = "ward.D")
plot(fit)
rect.hclust(fit, k = 6)
