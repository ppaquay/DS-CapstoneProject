## Necessary packages

#Sys.setlocale(locale = "en_GB.UTF-8")

options(java.parameters = "-Xmx8000m")

library(tm)
library(qdap)
library(RWeka)
library(stringr)
library(stringi)
library(dplyr)
library(data.table)
#library(parallel)

options(mc.cores = 1)

## Corpus creation (with package "tm")

setwd("~/Docs/Coursera/Capstone Project/Capstone")
dir <- DirSource("final/en_US/", encoding = "UTF-8")
corpus <- Corpus(dir, readerControl = list(reader = readPlain))

## Extracting a training set and test set (70% and 30% of the corpus)

set.seed(29)
blogs <- corpus[[1]]$content
news <- corpus[[2]]$content
twitter <- corpus[[3]]$content
prop <- 70 / 100
sample1 <- sample(1:length(blogs), length(blogs) * prop)
sample2 <- sample(1:length(news), length(news) * prop)
sample3 <- sample(1:length(twitter), length(twitter) * prop)

blogs.train <- blogs[sample1]
news.train <- news[sample2]
twitter.train <- twitter[sample3]

blogs.test <- blogs[-sample1]
news.test <- news[-sample2]
twitter.test <- twitter[-sample3]

rm(corpus)

## Processing and cleaning function

ReplaceAbbr <- function(x) {
    # Replaces every occurence of typical abbreviations in english (like "Mr.", "a.m.", "o.k.", ...)
    
    # Returns a corpus which is the result of the previous replacements
    
    x <- gsub("Mr\\.", "Mr", x)
    x <- gsub("Ms\\.", "Ms", x)
    x <- gsub("Mrs\\.", "Mrs", x)
    x <- gsub("i\\.e\\.", "ie", x)
    x <- gsub("A\\.D\\.", "AD", x)
    x <- gsub("B\\.C\\.", "BC", x)
    x <- gsub("A\\.M\\.", "am", x)
    x <- gsub("P\\.M\\.", "pm", x)
    x <- gsub("a\\.m\\.", "am", x)
    x <- gsub("p\\.m\\.", "pm", x)
    x <- gsub("et al\\.", "et al", x)
    x <- gsub("Jr\\.", "Jr", x)
    x <- gsub("Sr\\.", "Sr", x)
    x <- gsub("Dr\\.", "Dr", x)
    x <- gsub("Sen\\.", "Sen", x)
    x <- gsub("Rep\\.", "Sen", x)
    x <- gsub("Gov\\.", "Sen", x)
    x <- gsub("PhD\\.", "PhD", x)
    x <- gsub("Sgt\\.", "Sgt", x)
    x <- gsub("Lt\\.", "Lt", x)
    x <- gsub("Gen\\.", "Gen", x)
    x <- gsub("U\\.S\\.", "US", x)
    x <- gsub("U\\.K\\.", "UK", x)
    x <- gsub("U\\.S\\.A\\.", "USA", x)
    x <- gsub("U\\.N\\.", "UN", x)
    x <- gsub("N\\.Y\\.", "NY", x)
    x <- gsub("L\\.A\\.", "LA", x)
    x <- gsub("Co\\.", "Co", x)
    x <- gsub("p\\.s\\.", "ps", x)
    x <- gsub("St\\.", "St", x)
    x <- gsub("Ave\\.", "Ave", x)
    x <- gsub("Blvd\\.", "Blvd", x)
    x <- gsub("o\\.k\\.", "ok", x)
    x <- gsub("vs\\.", "vs", x)
    x <- gsub("Inc\\.", "Inc", x)
    x <- gsub("Corp\\.", "Corp", x)
    x <- gsub("W\\.", "W", x)
    x <- gsub("A\\.", "A", x)
    x <- gsub("E\\.", "E", x)
    x <- gsub("J\\.", "J", x)
    x <- gsub("S\\.", "S", x)
    x <- gsub("N\\.", "N", x)
    x <- gsub("M\\.", "M", x)
    x <- gsub("L\\.", "L", x)
    x <- gsub("C\\.", "C", x)
    x <- gsub("F\\.", "F", x)
    x <- gsub("B\\.", "B", x)
    x <- gsub("G\\.", "G", x)
    x <- gsub("H\\.", "H", x)
    x <- gsub("P\\.", "P", x)
    x <- gsub("T\\.", "T", x)
    x <- gsub("w\\/", "with", x)
    x <- gsub("\\&", "and", x)
    x <- gsub("m\\.r\\.i\\.", "mri", x)
    x <- gsub("\\/", " ", x)
    x <- gsub("Jan\\.", "Jan", x)
    x <- gsub("Feb\\.", "Feb", x)
    x <- gsub("Mar\\.", "Mar", x)
    x <- gsub("Apr\\.", "Apr", x)
    x <- gsub("Aug\\.", "Aug", x)
    x <- gsub("Sept\\.", "Sept", x)
    x <- gsub("Oct\\.", "Oct", x)
    x <- gsub("Nov\\.", "Nov", x)
    x <- gsub("Dec\\.", "Dec", x)
    
    return(x)
}


RemoveDotsNums <- function(x) gsub("([0-9]*)\\.([0-9]+)", "\\1\\2", x)
# Removes every occurence of dots in decimal numbers in the corpus

# Returns a corpus which is the result of the previous removals


RemoveCommaNums <- function(x) gsub("([0-9]+),([0-9]+)", "\\1\\2", x)
# Removes every occurence of spaces in numbers in the corpus

# Returns a corpus which is the result of the previous removals


#ReplaceNumbers <- function(x) gsub("[0-9]+", "99", x)
# Replaces every occurence of numbers in the corpus with "99"

# Returns a corpus which is the result of the previous replacements


RemoveEmail <- function(x) gsub("[A-z0-9._%+-]+@([A-z0-9-]+)\\.(?:[A-z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)", "\\1", x)
# Removes every occurence of email addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveURL <- function(x) gsub("(((https?|ftp)://)?www\\.)?([A-z0-9]+)\\.[A-z0-9]{2,}", "\\4", x)
# Removes every occurence of URLs addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveLRSQuotation <- function(x) {
    # Replaces every occurence of single left and right quotation marks with "\'" in the corpus
    # Preserves single left quotation mark used to abbreviate a word and replaces it with "\'"
    
    # Returns a corpus which is the result of the previous replacements
    
    x <- gsub("\u2018(([A-z0-9]+\\s?)+)\u2019", "\\1", x)
    x <- gsub("\u2018|\u2019", "\'", x)
    
    return(x)
}


RemoveLRDQuotation <- function(x) gsub("\u201c|\u201d", "\"", x)
# Replaces every occurence of double left and right quotation marks with "\'" in the corpus

# Returns a corpus which is the result of the previous replacements


RemoveDashes <- function(x) gsub("-+|—", " ", x)
# Removes every occurence of of one or more dashes in the corpus

# Returns a corpus which is the result of the previous removals


TransformIs <- function(x) {
    # Transforms every occurence of low case "i'" with the equivalent "I'"
    
    # Returns a corpus which is the result of the previous transformations
    
    x <- gsub("^i ", "I ", x)
    x <- gsub("^i'", "I'", x)
    x <- gsub(" i ", " I ", x)
    x <- gsub(" i'", " I'", x)
    x <- gsub(".i ", ". I ", x)
    x <- gsub(".i'", ". I'", x)
    
    return(x)
}


TransformDots <- function(x) gsub("[:?!|\u2026\\.]+", ".", x)
# Transforms every occurence of sentence termination marks a dot in the corpus

# Returns a corpus which is the result of the previous transformations


RemovePunc <- function(x) gsub('[])(,#%$^*\\~{}[&+=@/"`|<>_]+', " ", x)
# Removes every occurence of punctuation marks in the corpus

# Returns a corpus which is the result of the previous removals


SplitSentence <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = "\\r\\n\\t.:?!;|\u2026"))
# Splits every sentence according to termination marks in the corpus

# Returns a corpus which is the result of the sentence splitting


RemoveTrailingSpaces <- function(x) gsub("^\\s|\\s$", "", x)
# Removes every occurence of trailing spaces in the corpus

# Returns a corpus which is the result of the previous removals


RemoveEmptyStrings <- function(x) {
    # Removes every empty string in the corpus
    
    # Returns a corpus which is the result of the previous removals
    
    return(x[x != ""])
}

CleanCorpus <- function(text) {
    temp <- sapply(text, ReplaceAbbr, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveDotsNums, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveCommaNums, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveEmail, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveURL, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveLRSQuotation, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveLRDQuotation, USE.NAMES = FALSE)
    temp <- sapply(temp, RemoveDashes, USE.NAMES = FALSE)
    temp <- sapply(temp, tolower, USE.NAMES = FALSE)
    temp <- sapply(temp, TransformIs, USE.NAMES = FALSE)
    temp <- sapply(temp, iconv, from = "latin1", to = "ASCII", sub = "", USE.NAMES = FALSE)
    temp <- sapply(temp, TransformDots, USE.NAMES = FALSE)
    temp <- sapply(temp, RemovePunc, USE.NAMES = FALSE)
    temp <- sapply(temp, stripWhitespace, USE.NAMES = FALSE)
    temp <- unlist(sapply(temp, SplitSentence, USE.NAMES = FALSE))
    temp <- sapply(temp, RemoveTrailingSpaces, USE.NAMES = FALSE)
    temp <- unlist(sapply(temp, RemoveEmptyStrings, USE.NAMES = FALSE))
    
    return(unique(temp))
}

## Create text chunks

setwd("~/Docs/Coursera/Capstone Project/Capstone/final/en_US/train/4/")
l <- length(blogs.train)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- blogs.train[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("blogs-train-", i + 1, ".txt"))
}
l <- length(news.train)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- news.train[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("news-train-", i + 1, ".txt"))
}
l <- length(twitter.train)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- twitter.train[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("twitter-train-", i + 1, ".txt"))
}
rm(temp)
rm(text)

setwd("~/Docs/Coursera/Capstone Project/Capstone/final/en_US/test/4/")
l <- length(blogs.test)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- blogs.test[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("blogs-test-", i + 1, ".txt"))
}
l <- length(news.test)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- news.test[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("news-test-", i + 1, ".txt"))
}
l <- length(twitter.test)
chunks <- 10
for (i in 0:(chunks - 1)) {
    text <- twitter.test[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
    temp <- CleanCorpus(text)
    write(temp, file = paste0("twitter-test-", i + 1, ".txt"))
}

## TDM and freq vectors

dir <- DirSource("~/Docs/Coursera/Capstone Project/Capstone/rdata/final/corpus/no-unk/", encoding = "UTF-8")
corpus.train <- Corpus(dir, readerControl = list(reader = readPlain))

OnegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = " \\r\\n\\t.,;:\"()?!|"))
TwogramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ThreegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!|"))
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!|"))
FivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.1 <- list(tokenize = OnegramTokenizer, wordLengths = c(1, Inf), bounds = list(global = c(3, Inf)))
ctrl.2 <- list(tokenize = TwogramTokenizer, bounds = list(global = c(3, Inf)))
ctrl.3 <- list(tokenize = ThreegramTokenizer, bounds = list(global = c(3, Inf)))
ctrl.4 <- list(tokenize = FourgramTokenizer, bounds = list(global = c(3, Inf)))
ctrl.5 <- list(tokenize = FivegramTokenizer, bounds = list(global = c(3, Inf)))

corpus.preproc <- corpus.train

TDM.onegrams.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.1)
save(TDM.onegrams.train, file = "rdata/final/TDM-onegrams-def.RData")
freq.onegrams.train <- sort(rowSums(as.matrix(TDM.onegrams.train)), decreasing = TRUE)
vocabulary <- freq.onegrams.train[freq.onegrams.train > 5]
infreq.words <- freq.onegrams.train[freq.onegrams.train < 6]
vocabulary <- append(vocabulary, length(infreq.words))
names(vocabulary)[length(vocabulary)] <- "<unk>"
vocabulary <- sort(vocabulary, decreasing = TRUE)
save(vocabulary, file = "rdata/final/vocabulary-def2.RData")
freq.onegrams <- data.table(Term = names(vocabulary), Freq = vocabulary)
#freq.onegrams <- freq.onegrams[with(freq.onegrams, order(-Freq)), ]
#freq.onegrams <- freq.onegrams[order(-Freq)]
#row.names(freq.onegrams) <- NULL
save(freq.onegrams, file = "rdata/final/freq-onegrams-def2.RData")

#infreq.words <- freq.onegrams.train[freq.onegrams.train < 6]
#df <- data.frame(term = names(infreq.words), n = nchar(names(infreq.words)), stringsAsFactors = FALSE)
#replace <- as.character(df[order(df$n, decreasing = TRUE), ]$term)
#set.seed(1)
#sample <- sample(1:length(replace), 50000)
#replace <- replace[sample]
#sed.args <- paste0("s/ ", replace, " / <UNK> /")
#l <- length(sed.args)
#chunks <- 200
#setwd("~/Docs/Coursera/Capstone Project/Capstone/rdata/final/infreq/")
#for (i in 0:(chunks - 1)) {
#    text <- sed.args[(trunc(i * l / chunks) + 1):trunc((i + 1) * l / chunks)]
#    write(text, file = paste0("replacements-", i + 1, ".patterns"))
#}

#freq.onegrams.df <- tbl_df(freq.onegrams)

## Processing corpus with bash sed

## Extraction of TDM and freq table for ngrams

AddBOSandEOS <- function (x) gsub("(^.+$)", "<BOS> \\1 <EOS>", x)

corpus.preproc <- tm_map(corpus.train, content_transformer(AddBOSandEOS))
#TDM.onegrams.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.1)
#save(TDM.onegrams.train, file = "rdata/final/TDM-onegrams-train.RData")
#freq.onegrams.train <- sort(rowSums(as.matrix(TDM.onegrams.train)), decreasing = TRUE)
#freq.onegrams.train <- freq.onegrams.train[freq.onegrams.train != 1]
#save(freq.onegrams.train, file = "rdata/final/freq-onegrams-train.RData")

TDM.twograms.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.2)
save(TDM.twograms.train, file = "rdata/final/TDM-twograms-def.RData")
freq.twograms.train <- sort(rowSums(as.matrix(TDM.twograms.train)), decreasing = TRUE)
freq.twograms.train <- freq.twograms.train[freq.twograms.train > 4]
freq.twograms <- data.table(Term = names(freq.twograms.train), Freq = freq.twograms.train)
#row.names(freq.twograms) <- NULL
#save(freq.twograms, file = "rdata/final/freq-twograms.RData")
freq.twograms.unk <- freq.twograms[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])]
#freq.twograms.unk2 <- freq.twograms.unk
voc <- names(vocabulary)
voc <- voc[voc != "<unk>"]
save(voc, file = "vocab-def3.RData")
freq.twograms.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")]
freq.twograms.unk <- freq.twograms.unk[, lapply(.SD, sum), by = "Term,Word1,Word2"]
freq.twograms.unk <- freq.twograms.unk[order(-Freq)]
save(freq.twograms.unk, file = "rdata/final/freq-twograms-def.RData")

TDM.threegrams.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.3)
save(TDM.threegrams.train, file = "rdata/final/TDM-threegrams-def.RData")
freq.threegrams.train <- sort(rowSums(as.matrix(TDM.threegrams.train)), decreasing = TRUE)
freq.threegrams.train <- freq.threegrams.train[freq.threegrams.train > 4]
freq.threegrams <- data.table(Term = names(freq.threegrams.train), Freq = freq.threegrams.train)
freq.threegrams.unk <- freq.threegrams[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])][, Word3 := sapply(Term, function(x) unlist(str_split(x, " "))[3])]
freq.threegrams.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")]
freq.threegrams.unk <- freq.threegrams.unk[, lapply(.SD, sum), by = "Term,Word1,Word2,Word3"]
freq.threegrams.unk <- freq.threegrams.unk[order(-Freq)]
save(freq.threegrams.unk, file = "rdata/final/freq-threegrams-def.RData")

TDM.fourgrams.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.4)
save(TDM.fourgrams.train, file = "rdata/final/TDM-fourgrams-def.RData")
freq.fourgrams.train <- sort(rowSums(as.matrix(TDM.fourgrams.train)), decreasing = TRUE)
freq.fourgrams.train <- freq.fourgrams.train[freq.fourgrams.train > 4]
freq.fourgrams <- data.table(Term = names(freq.fourgrams.train), Freq = freq.fourgrams.train)
freq.fourgrams.unk <- freq.fourgrams[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])][, Word3 := sapply(Term, function(x) unlist(str_split(x, " "))[3])][, Word4 := sapply(Term, function(x) unlist(str_split(x, " "))[4])]
freq.fourgrams.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")][, Word3 := ifelse(Word3 %in% voc, Word3, "<unk>")]
freq.fourgrams.unk <- freq.fourgrams.unk[, lapply(.SD, sum), by = "Term,Word1,Word2,Word3,Word4"]
freq.fourgrams.unk <- freq.fourgrams.unk[order(-Freq)]
save(freq.fourgrams.unk, file = "rdata/final/freq-fourgrams.RData")

TDM.fivegrams.train <- TermDocumentMatrix(corpus.preproc, control = ctrl.5)
save(TDM.fivegrams.train, file = "rdata/final/TDM-fivegrams-train.RData")
freq.fivegrams.train <- sort(rowSums(as.matrix(TDM.fivegrams.train)), decreasing = TRUE)
freq.fivegrams.train <- freq.fivegrams.train[freq.fivegrams.train > 5]
freq.fivegrams <- data.table(Term = names(freq.fivegrams.train), Freq = freq.fivegrams.train)
freq.fivegrams.unk <- freq.fivegrams[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])][, Word3 := sapply(Term, function(x) unlist(str_split(x, " "))[3])][, Word4 := sapply(Term, function(x) unlist(str_split(x, " "))[4])][, Word5 := sapply(Term, function(x) unlist(str_split(x, " "))[5])]
freq.fivegrams.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")][, Word3 := ifelse(Word3 %in% voc, Word3, "<unk>")][, Word4 := ifelse(Word4 %in% voc, Word4, "<unk>")]
freq.fivegrams.unk <- freq.fivegrams.unk[, lapply(.SD, sum), by = "Term,Word1,Word2,Word3,Word4,Word5"]
freq.fivegrams.unk <- freq.fivegrams.unk[order(-Freq)]
save(freq.fivegrams.unk, file = "rdata/final/freq-fivegrams.RData")

rm(TDM.onegrams.train)
rm(TDM.twograms.train)
rm(TDM.threegrams.train)
rm(TDM.fourgrams.train)
rm(TDM.fivegrams.train)

setkey(freq.onegrams, Term)
save(freq.onegrams, file = "rdata/final/onegrams-def2.RData")

freq.twograms.unk <- freq.twograms.unk[, First1 := Word1][, Last1 := Word2]
freq.twograms.unk <- freq.twograms.unk[, Word1 := NULL][, Word2 := NULL]
setkey(freq.twograms.unk, First1)
save(freq.twograms.unk, file = "rdata/final/twograms-def2.RData")

freq.threegrams.unk <- freq.threegrams.unk[, First2 := paste(Word1, Word2)][, Last1 := Word3]
freq.threegrams.unk <- freq.threegrams.unk[, Word1 := NULL][, Word2 := NULL][, Word3 := NULL]
setkey(freq.threegrams.unk, First2)
save(freq.threegrams.unk, file = "rdata/final/threegrams-def2.RData")

freq.fourgrams.unk <- freq.fourgrams.unk[, First3 := paste(Word1, Word2, Word3)][, Last1 := Word4]
freq.fourgrams.unk <- freq.fourgrams.unk[, Word1 := NULL][, Word2 := NULL][, Word3 := NULL][, Word4 := NULL]
setkey(freq.fourgrams.unk, First3)
save(freq.fourgrams.unk, file = "rdata/final/fourgrams-def2.RData")

freq.fivegrams.unk <- freq.fivegrams.unk[, First4 := paste(Word1, Word2, Word3, Word4)][, Last1 := Word5]
freq.fivegrams.unk <- freq.fivegrams.unk[, Word1 := NULL][, Word2 := NULL][, Word3 := NULL][, Word4 := NULL][, Word5 := NULL]
setkey(freq.fivegrams.unk, First4)
save(freq.fivegrams.unk, file = "rdata/final/fivegrams.RData")

## SGT tables for ngrams

SimpleGoodTuringTable <- function(freq.table) {
    vec <- freq.table$Freq
    names(vec) <- freq.table$Term
    table <- table(vec)
    SGT.table <- data.table(r = as.numeric(names(table)), n = as.vector(table))
    N <- sum(SGT.table$r * SGT.table$n)
    P0 <- SGT.table$n[1] / N
    for (m in 1:nrow(SGT.table)) {
        if (m == 1)
            i <- 0
        else
            i <- SGT.table$r[m - 1]
        if (m == nrow(SGT.table))
            k <- 2 * SGT.table$r[m] - i
        else
            k <- SGT.table$r[m + 1]
        SGT.table$Z[m] <- 2 * SGT.table$n[m] / (k - i)
    }
    SGT.table$logr <- log(SGT.table$r)
    SGT.table$logZ <- log(SGT.table$Z)
    reg.line <- lm(SGT.table$logZ ~ SGT.table$logr)
    a <- as.numeric(reg.line$coefficients[1])
    b <- as.numeric(reg.line$coefficients[2])
    S <- function(r) exp(a + b * log(r))
    cond <- 0
    for (m in 1:(nrow(SGT.table) - 1)) {
        r.plus <- SGT.table$r[m] + 1
        y <- r.plus * S(SGT.table$r[m] + 1) / S(SGT.table$r[m])
        if (cond == 1)
            SGT.table$r.star[m] <- y
        else {
            nr.plus <- SGT.table$n[SGT.table$r == (SGT.table$r[m] + 1)]
            if (length(nr.plus) == 0) {
                SGT.table$r.star[m] <- y
                cond <- 1
            }
            else {
                nr <- SGT.table$n[m]
                x <- r.plus * nr.plus / nr
                if (abs(x - y) > 1.96 * sqrt((r.plus^2) * (nr.plus / nr^2) * ( 1 + nr.plus / nr)))
                    SGT.table$r.star[m] <- x
                else {
                    SGT.table$r.star[m] <- y
                    cond <- 1
                }
            }
        }
        if (m == (nrow(SGT.table) - 1))
            SGT.table$r.star[m + 1] <- y
    }
    N.prime <- sum(SGT.table$r.star * SGT.table$n)
    SGT.table$p <- (1 - P0) * SGT.table$r.star / N.prime
    
    return(SGT.table)
}

SGT.onegrams <- SimpleGoodTuringTable(freq.onegrams)
setkey(SGT.onegrams, r)
save(SGT.onegrams, file = "rdata/final/SGT-onegrams-def2.RData")

SGT.twograms <- SimpleGoodTuringTable(freq.twograms.unk)
setkey(SGT.twograms, r)
save(SGT.twograms, file = "rdata/final/SGT-twograms-def2.RData")

SGT.threegrams <- SimpleGoodTuringTable(freq.threegrams.unk)
setkey(SGT.threegrams, r)
save(SGT.threegrams, file = "rdata/final/SGT-threegrams-def2.RData")

SGT.fourgrams <- SimpleGoodTuringTable(freq.fourgrams.unk)
setkey(SGT.fourgrams, r)
save(SGT.fourgrams, file = "rdata/final/SGT-fourgrams-def2.RData")

SGT.fivegrams <- SimpleGoodTuringTable(freq.fivegrams.unk)
setkey(SGT.fivegrams, r)
save(SGT.fivegrams, file = "rdata/final/SGT-fivegrams.RData")

## Prediction test

profanities <- c("acrotomophilia", "anal", "anulingus", "anus", "arsehole", "ass", "asses", "asshole", "assholes", "assmunch", "autoerotic", "babeland", "bareback", "barenaked", "bastardo", "bastinado", "bbw", "bdsm", "bimbos", "birdlock", "bitch", "bitches", "blumpkin", "bollocks", "bondage", "boner", "boob", "boobs", "bukkake", "bulldyke", "bunghole", "busty", "butt", "butts", "buttcheeks", "butthole", "cameltoe", "cameltoes", "camgirl", "camslut", "camwhore", "carpet muncher", "carpetmuncher", "circlejerk", "clit", "clitoris", "clusterfuck", "cock", "cocks", "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", "cunnilingus", "cunt", "cunts", "darkie", "daterape", "deepthroat", "dick", "dicks", "dildo", "doggiestyle", "doggystyle", "dolcett", "dominatrix", "dommes", "ecchi", "ejaculation", "erotic", "erotism", "escort", "eunuch", "faggot", "fecal", "felch", "fellatio", "feltch", "femdom", "figging", "fingering", "fisting", "footjob", "frotting", "fuck", "fuckoff", "fuckit", "fucked", "fucker", "fuckers", "fuckin", "fucking", "fuckyou", "futanari", "gangbang", "genitals", "gokkun", "goodpoop", "grope", "gspot", "guro", "handjob", "hardcore", "hentai", "hooker", "humping", "incest", "intercourse", "jailbait", "jigaboo", "jiggaboo", "jiggerboo", "jizz", "juggs", "kike", "kinbaku", "kinkster", "kinky", "knobbing", "lolita", "lovemaking", "masturbate", "milf", "motherfucker", "motherfuckers", "mound of venus", "muffdiving", "nambla", "nawashi", "negro", "neonazi", "nigga", "niggas", "nigger", "niggers", "nymphomania", "nipple", "nipples", "nude", "nudity", "nympho", "octopussy", "omorashi", "orgasm", "orgy", "orgies", "paedophile", "panties", "panty", "pedobear", "pedophile", "pegging", "penis", "phone sex", "pissing", "pisspig", "playboy", "ponyplay", "poof", "poopchute", "porn", "porno", "pornography", "pthc", "pubes", "pussy", "queaf", "raghead", "rape", "raping", "rapist", "rectum", "rimjob", "rimming", "sadism", "scat", "schlong", "scissoring", "semen", "sex", "sexo", "sexy", "shemale", "shibari", "shit", "shota", "shrimping", "slanteye", "slut", "sluts", "s&m", "smut", "snatch", "sodomize", "sodomy", "spic", "spooge", "strapon", "suck", "sucks", "swastika", "swinger", "threesome", "throating", "tit", "tits", "titties", "titty", "topless", "tosser", "tranny", "tribadism", "tubgirl", "tushy", "twat", "twink", "twinkie", "undressing", "upskirt", "urophilia", "vagina", "vibrator", "violet wand", "vorarephilia", "voyeur", "vulva", "wank", "wetback", "xx", "xxx", "yaoi", "yiffy", "zoophilia")

PredictNextWord <- function(sentence, nbre.pred = 4) {
    
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    split <- sapply(split, function(x) ifelse(x %in% voc, x, "<unk>"), USE.NAMES = FALSE)
    
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    trigram <- gsub("^\\s+([A-z0-9<>])", "\\1", trigram)
    bigram <- paste(split[len - 1], split[len])
    bigram <- gsub("^\\s+([A-z0-9<>])", "\\1", bigram)
    unigram <- split[len]
    
    d4 <- 1
    d3 <- 1
    d2 <- 1
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
    c.trigram <- freq.threegrams.unk[Term == trigram]
    if (nrow(subs.fourgrams) > 0) {
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$r.star[SGT.fourgrams$r == x]) / c.trigram$Freq, Word = subs.fourgrams$Last1)        
        d4 <- 1 - sum(predictions.four$Prob)
    }
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
    c.bigram <- freq.twograms.unk[Term == bigram]
    if (nrow(subs.threegrams) > 0) {
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / c.bigram$Freq, Word = subs.threegrams$Last1)
        predictions.three <- predictions.three[, Prob := d4 * Prob]
        d3 <- 1 - sum(predictions.three$Prob)
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<unk>", "<eos>", profanities))]
    c.unigram <- freq.onegrams[Term == unigram]
    if (nrow(subs.twograms) > 0) {
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / c.unigram$Freq, Word = subs.twograms$Last1)
        predictions.two <- predictions.two[, Prob := d3 * d4 * Prob]
        d2 <- 1 - sum(predictions.two$Prob)
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    predictions.one <- data.table(Prob = subs.onegrams$Freq / sum(freq.onegrams$Freq), Word = subs.onegrams$Term)
    predictions.one <- predictions.one[, Prob := d2 * d3 * d4 * Prob]
    
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[, Probas := max(Prob), by = list(Word)]
    predictions <- predictions[, Prob := NULL]
    predictions <- subset(unique(predictions))
    predictions <- predictions[order(-Probas)]
    
    return(predictions[1:min(nbre.pred, nrow(predictions))])
}


PredictNextWord <- function(sentence, nbre.pred = 4) {
    
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    split <- sapply(split, function(x) ifelse(x %in% voc, x, "<unk>"), USE.NAMES = FALSE)
    len <- length(split)
    quadrigram <- paste(split[len - 3], split[len - 2], split[len - 1], split[len])
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    d5 <- 1
    d4 <- 1
    d3 <- 1
    d2 <- 1
    
    subs.fivegrams <- freq.fivegrams.unk[First4 == quadrigram]
    subs.fivegrams <- subs.fivegrams[!(Last1 %in% c("<eos>", profanities))]
    c.quadrigram <- freq.fourgrams.unk[Term == quadrigram]
    if (nrow(subs.fivegrams) > 0) {
        subs.fivegrams <- subs.fivegrams[1:min(100, nrow(subs.fivegrams))]
        predictions.five <- data.table(Prob = sapply(subs.fivegrams$Freq, FUN = function(x) SGT.fivegrams$r.star[SGT.fivegrams$r == x]) / c.quadrigram$Freq, Word = subs.fivegrams$Last1)        
        d5 <- 1 - sum(predictions.five$Prob)
    }
    else
        predictions.five <- data.table(Prob = c(0), Word = c(""))
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    c.trigram <- freq.threegrams.unk[Term == trigram]
    if (nrow(subs.fourgrams) > 0) {
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$r.star[SGT.fourgrams$r == x]) / c.trigram$Freq, Word = subs.fourgrams$Last1)        
        predictions.four <- predictions.four[, Prob := d5 * Prob]
        d4 <- 1 - sum(predictions.four$Prob)
    }
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    c.bigram <- freq.twograms.unk[Term == bigram]
    if (nrow(subs.threegrams) > 0) {
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / c.bigram$Freq, Word = subs.threegrams$Last1)
        predictions.three <- predictions.three[, Prob := d4 * d5 * Prob]
        d3 <- 1 - sum(predictions.three$Prob)
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    c.unigram <- freq.onegrams[Term == unigram]
    if (nrow(subs.twograms) > 0) {
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / c.unigram$Freq, Word = subs.twograms$Last1)
        predictions.two <- predictions.two[, Prob := d3 * d4 * d5 * Prob]
        d2 <- 1 - sum(predictions.two$Prob)
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    predictions.one <- data.table(Prob = subs.onegrams$Freq / sum(freq.onegrams$Freq), Word = subs.onegrams$Term)
    predictions.one <- predictions.one[, Prob := d2 * d3 * d4 * d5 * Prob]
    
    Probs <- c(predictions.five$Prob, predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.five$Word, predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[, Probas := max(Prob), by = list(Word)]
    predictions <- predictions[, Prob := NULL]
    predictions <- subset(unique(predictions))
    predictions <- predictions[order(-Probas)]
    
    return(predictions[1:min(nbre.pred, nrow(predictions))])
}

## Model validation

dir <- DirSource("~/Docs/Coursera/Capstone Project/Capstone/rdata/final/corpus/test/", encoding = "UTF-8")
corpus.test <- Corpus(dir, readerControl = list(reader = readPlain))

TwogramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test2 <- list(tokenize = TwogramTokenizer, bounds = list(global = c(3, Inf)))
ThreegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test3 <- list(tokenize = ThreegramTokenizer, bounds = list(global = c(3, Inf)))
FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test4 <- list(tokenize = FourgramTokenizer, bounds = list(global = c(3, Inf)))
FivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test5 <- list(tokenize = FivegramTokenizer, bounds = list(global = c(4, Inf)))
SixgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 6, max = 6, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test6 <- list(tokenize = SixgramTokenizer, bounds = list(global = c(4, Inf)))
SevengramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 7, max = 7, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test7 <- list(tokenize = SevengramTokenizer, bounds = list(global = c(4, Inf)))
EightgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 8, max = 8, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test8 <- list(tokenize = EightgramTokenizer, bounds = list(global = c(5, Inf)))
NinegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 9, max = 9, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test9 <- list(tokenize = NinegramTokenizer, bounds = list(global = c(5, Inf)))
TengramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 10, max = 10, delimiters = " \\r\\n\\t.,;:\"()?!|"))
ctrl.test10 <- list(tokenize = TengramTokenizer, bounds = list(global = c(5, Inf)))

TDM.twograms.test <- TermDocumentMatrix(corpus.test, control = ctrl.test2)
freq.twograms.test <- rowSums(as.matrix(TDM.twograms.test))
TDM.threegrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test3)
freq.threegrams.test <- rowSums(as.matrix(TDM.threegrams.test))
TDM.fourgrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test4)
freq.fourgrams.test <- rowSums(as.matrix(TDM.fourgrams.test))
TDM.fivegrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test5)
freq.fivegrams.test <- rowSums(as.matrix(TDM.fivegrams.test))
TDM.sixgrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test6)
freq.sixgrams.test <- rowSums(as.matrix(TDM.sixgrams.test))
TDM.sevengrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test7)
freq.sevengrams.test <- rowSums(as.matrix(TDM.sevengrams.test))
TDM.eightgrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test8)
freq.eightgrams.test <- rowSums(as.matrix(TDM.eightgrams.test))
TDM.ninegrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test9)
freq.ninegrams.test <- rowSums(as.matrix(TDM.ninegrams.test))
TDM.tengrams.test <- TermDocumentMatrix(corpus.test, control = ctrl.test10)
freq.tengrams.test <- rowSums(as.matrix(TDM.tengrams.test))

twograms.test <- names(freq.twograms.test)
save(twograms.test, file = "rdata/final/twograms-test.RData")
threegrams.test <- names(freq.threegrams.test)
save(threegrams.test, file = "rdata/final/threegrams-test.RData")
fourgrams.test <- names(freq.fourgrams.test)
save(fourgrams.test, file = "rdata/final/fourgrams-test.RData")
fivegrams.test <- names(freq.fivegrams.test)
save(fivegrams.test, file = "rdata/final/fivegrams-test.RData")
sixgrams.test <- names(freq.sixgrams.test)
save(sixgrams.test, file = "rdata/final/sixgrams-test.RData")
sevengrams.test <- names(freq.sevengrams.test)
save(sevengrams.test, file = "rdata/final/sevengrams-test.RData")
eightgrams.test <- names(freq.eightgrams.test)
save(eightgrams.test, file = "rdata/final/eightgrams-test.RData")
ninegrams.test <- names(freq.ninegrams.test)
save(ninegrams.test, file = "rdata/final/ninegrams-test.RData")
tengrams.test <- names(freq.tengrams.test)
save(tengrams.test, file = "rdata/final/tengrams-test.RData")

Firstn <- function(ngram) {
    wordvec <- unlist(strsplit(ngram, " "))
    n <- length(wordvec)
    wordvec <- wordvec[1:(n - 1)]
    wordvec <- paste(wordvec, sep = "", collapse = " ")
    
    return(wordvec)
}

Last1 <- function(ngram) {
    wordvec <- unlist(strsplit(ngram, " "))
    n <- length(wordvec)
    wordvec <- wordvec[n]
    
    return(wordvec)
}

ngrams.test <- c(twograms.test, threegrams.test, fourgrams.test, fivegrams.test, sixgrams.test, sevengrams.test, eightgrams.test, ninegrams.test, tengrams.test)

set.seed(1989)
mse <- vector()
for (i in 1:100) {
    sample <- sample(1:length(ngrams.test), 100)
    validation.set <- ngrams.test[sample]
    #print(validation.set)
    validation <- data.table(Term = validation.set)
    validation <- validation[, Test := sapply(Term, function(x) Firstn(x))][, Verif := sapply(Term, function(x) Last1(x))]
    predicted <- sapply(validation$Test, function(x) PredictNextWordApprox(x, nbre.pred = 1)$Word)
    #predicted2 <- sapply(validation$Test, function(x) PredictNextWord2(x, nbre.pred = 1)$Word)
    #predicted <- sapply(validation$Test, PredictNextWord20, nbre.pred = 1)
    #print(predicted)
    mse[i] <- sum(validation$Verif %in% predicted)
    #mse[i] <- sum(predicted == validation$Verif)
    #print(mse[i])
}
mean(mse) # 1: 13.75% 2: 18.68% 3: 4.67%

## Other prediction algorithm with Katz backoff

PredictNextWord21 <- function(sentence, nbre.pred = 4) {
    #print(sentence)
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.fourgrams)
    n4 <- nrow(subs.fourgrams)
    if (n4 > 0) {
        #print("Fourgrams used")
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        #print(subs.fourgrams)
        c.trigram <- freq.threegrams.unk[Term == trigram]
        #print(c.trigram)
        predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$p[SGT.fourgrams$r == x]) / SGT.threegrams$p[SGT.threegrams$r == c.trigram$Freq], Word = subs.fourgrams$Last1)
    }
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.threegrams)
    n3 <- nrow(subs.threegrams)
    if (n3 > 0) {
        #print("Threegrams used")
        #print(subs.threegrams)
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        #print(subs.threegrams)
        c.bigram <- freq.twograms.unk[Term == bigram]
        predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$p[SGT.threegrams$r == x]) / SGT.twograms$p[SGT.twograms$r == c.bigram$Freq], Word = subs.threegrams$Last1)
        alpha3 <- (1 - sum(predictions.four$Prob)) / (1 - sum(predictions.three$Prob))
        predictions.three <- predictions.three[, Prob := alpha3 * Prob]
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.twograms)
    n2 <- nrow(subs.twograms)
    if (n2 > 0) {
        #print("Twograms used")
        #print(subs.twograms)
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        #print(subs.twograms)
        #print(tail(sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x])))
        c.unigram <- freq.onegrams[Term == unigram]
        #print(c.unigram)
        predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x]) / SGT.onegrams$p[SGT.onegrams$r == c.unigram$Freq], Word = subs.twograms$Last1)
        alpha2 <- (1 - sum(predictions.three$Prob)) / (1 - sum(predictions.two$Prob))
        predictions.two <- predictions.two[, Prob := alpha2 * Prob]
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    #print(subs.onegrams)
    predictions.one <- data.table(Prob = sapply(subs.onegrams$Freq, FUN = function(x) SGT.onegrams$p[SGT.onegrams$r == x]), Word = subs.onegrams$Term)
    alpha1 <- (1 - sum(predictions.two$Prob)) / (1 - sum(predictions.one$Prob))
    predictions.one <- predictions.one[, Prob := alpha1 * Prob]
    #subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print("Fourgrams used")
    #print(predictions.four)
    #subs.fourgrams <- subs.fourgrams[order(-p)]
    #predictions <- subs.fourgrams$Last1
    #print(predictions.three)
    #print(predictions.two)
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    #print(Probs)
    #print(Words)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[order(-Prob)]
    #print(predictions)

    return(unique(predictions$Word)[1:min(nbre.pred, nrow(predictions))])
}

PredictNextWord <- function(sentence, nbre.pred = 4) {
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    split <- sapply(split, function(x) ifelse(x %in% voc, x, "<unk>"), USE.NAMES = FALSE)
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    d4 <- 1
    d3 <- 1
    d2 <- 1
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    c.trigram <- freq.threegrams.unk[Term == trigram]
    if (nrow(subs.fourgrams) > 0) {
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$r.star[SGT.fourgrams$r == x]) / c.trigram$Freq, Word = subs.fourgrams$Last1)        
        d4 <- 1 - sum(predictions.four$Prob)
    }
    
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    c.bigram <- freq.twograms.unk[Term == bigram]
    if (nrow(subs.threegrams) > 0) {
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / c.bigram$Freq, Word = subs.threegrams$Last1)
        predictions.three <- predictions.three[, Prob := d4 * Prob]
        d3 <- 1 - sum(predictions.three$Prob)
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    c.unigram <- freq.onegrams[Term == unigram]
    if (nrow(subs.twograms) > 0) {
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / c.unigram$Freq, Word = subs.twograms$Last1)
        predictions.two <- predictions.two[, Prob := d3 * d4 * Prob]
        d2 <- 1 - sum(predictions.two$Prob)
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    predictions.one <- data.table(Prob = subs.onegrams$Freq / sum(freq.onegrams$Freq), Word = subs.onegrams$Term)
    predictions.one <- predictions.one[, Prob := d2 * d3 * d4 * Prob]
    
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[, Probas := max(Prob), by = list(Word)]
    predictions <- predictions[, Prob := NULL]
    predictions <- subset(unique(predictions))
    predictions <- predictions[order(-Probas)]
    
    return(predictions[1:min(nbre.pred, nrow(predictions))])
}

df <- PredictNextWord("what's the")
df[, Word := as.factor(Word)]
df[, Word := factor(Word, levels = df[order(Probas), "xcv"])]
p <- ggplot(df, aes(y = Probas))
p + geom_bar(aes(x = Word), stat = "identity") + xlab("Words predicted") + ylab("Probability") + theme(axis.text.x=element_text(angle=45, hjust=1))
df[, Word := reorder(Word, -Probas)]
p + geom_bar(aes(x = Word), stat = "identity") + xlab("Words predicted") + ylab("Probability") + theme(axis.text.x=element_text(angle=45, hjust=1))


PredictNextWord20 <- function(sentence, nbre.pred = 4) {
    #print(sentence)
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    
    split <- sapply(split, function(x) ifelse(x %in% voc, x, "<unk>"), USE.NAMES = FALSE)
    #print(split)
    
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.fourgrams)
    #n4 <- nrow(subs.fourgrams)
    if (nrow(subs.fourgrams) > 0) {
        #print("Fourgrams used")
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        #print(subs.fourgrams)
        c.trigram <- freq.threegrams.unk[Term == trigram]
        #print(c.trigram)
        #print(subs.fourgrams)
        Probs <- sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$p[SGT.fourgrams$r == x])
        #print(Probs)
        predictions.four <- data.table(Prob = Probs / sum(Probs), Word = subs.fourgrams$Last1)
        #print(predictions.four)
    }
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.threegrams)
    #n3 <- nrow(subs.threegrams)
    if (nrow(subs.threegrams) > 0) {
        #print("Threegrams used")
        #print(subs.threegrams)
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        #print(subs.threegrams)
        #c.bigram <- freq.twograms.unk[Term == bigram]
        Probs <- sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$p[SGT.threegrams$r == x])
        #print(Probs)
        predictions.three <- data.table(Prob = 0.4 * Probs / sum(Probs), Word = subs.threegrams$Last1)
        #alpha3 <- (1 - sum(predictions.four$Prob)) / (1 - sum(predictions.three$Prob))
        #predictions.three <- predictions.three[, Prob := alpha3 * Prob]
        #print(predictions.three)
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.twograms)
    #n2 <- nrow(subs.twograms)
    if (nrow(subs.twograms) > 0) {
        #print("Twograms used")
        #print(subs.twograms)
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        #print(subs.twograms)
        #print(tail(sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x])))
        #c.unigram <- freq.onegrams[Term == unigram]
        #print(c.unigram)
        Probs <- sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x])
        #print(Probs)
        predictions.two <- data.table(Prob = 0.3 * Probs / sum(Probs), Word = subs.twograms$Last1)
        #alpha2 <- (1 - sum(predictions.three$Prob)) / (1 - sum(predictions.two$Prob))
        #predictions.two <- predictions.two[, Prob := alpha2 * Prob]
        #print(predictions.two)
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    #print(subs.onegrams)
    Probs <- sapply(subs.onegrams$Freq, FUN = function(x) SGT.onegrams$p[SGT.onegrams$r == x])
    predictions.one <- data.table(Prob = 0.2 * Probs / sum(Probs), Word = subs.onegrams$Term)
    #print(predictions.one)
    #alpha1 <- (1 - sum(predictions.two$Prob)) / (1 - sum(predictions.one$Prob))
    #predictions.one <- predictions.one[, Prob := alpha1 * Prob]
    #subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print("Fourgrams used")
    #print(predictions.four)
    #subs.fourgrams <- subs.fourgrams[order(-p)]
    #predictions <- subs.fourgrams$Last1
    #print(predictions.three)
    #print(predictions.two)
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    #print(Probs)
    #print(Words)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[order(-Prob)]
    #print(predictions)
    
    return(unique(predictions$Word)[1:min(nbre.pred, nrow(predictions))])
}

PredictNextWord23 <- function(sentence, nbre.pred = 4) {
    #print(sentence)
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    
    split <- sapply(split, function(x) ifelse(x %in% voc, x, "<unk>"), USE.NAMES = FALSE)
    #print(split)
    
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.fourgrams)
    #n4 <- nrow(subs.fourgrams)
    if (nrow(subs.fourgrams) > 0) {
        #print("Fourgrams used")
        subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
        #print(subs.fourgrams)
        c.trigram <- freq.threegrams.unk[Term == trigram]
        print(c.trigram)
        print(subs.fourgrams)
        predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$p[SGT.fourgrams$r == x]) / nrow(c.trigram), Word = subs.fourgrams$Last1)
        print(predictions.four)
    }
    else
        predictions.four <- data.table(Prob = c(0), Word = c(""))
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.threegrams)
    #n3 <- nrow(subs.threegrams)
    if (nrow(subs.threegrams) > 0) {
        #print("Threegrams used")
        #print(subs.threegrams)
        subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
        #print(subs.threegrams)
        c.bigram <- freq.twograms.unk[Term == bigram]
        predictions.three <- data.table(Prob = 0.4 * sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / nrow(c.bigram), Word = subs.threegrams$Last1)
        #alpha3 <- (1 - sum(predictions.four$Prob)) / (1 - sum(predictions.three$Prob))
        #predictions.three <- predictions.three[, Prob := alpha3 * Prob]
        #print(predictions.three)
    }
    else
        predictions.three <- data.table(Prob = c(0), Word = c(""))
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.twograms)
    #n2 <- nrow(subs.twograms)
    if (nrow(subs.twograms) > 0) {
        #print("Twograms used")
        #print(subs.twograms)
        subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
        #print(subs.twograms)
        #print(tail(sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x])))
        c.unigram <- freq.onegrams[Term == unigram]
        #print(c.unigram)
        predictions.two <- data.table(Prob = 0.3 * sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / nrow(c.unigram), Word = subs.twograms$Last1)
        #alpha2 <- (1 - sum(predictions.three$Prob)) / (1 - sum(predictions.two$Prob))
        #predictions.two <- predictions.two[, Prob := alpha2 * Prob]
        #print(predictions.two)
    }
    else
        predictions.two <- data.table(Prob = c(0), Word = c(""))
    
    subs.onegrams <- freq.onegrams[-c(1, 2)]
    subs.onegrams <- subs.onegrams[1:nbre.pred]
    #print(subs.onegrams)
    predictions.one <- data.table(Prob = 0.2 * sapply(subs.onegrams$Freq, FUN = function(x) SGT.onegrams$r.star[SGT.onegrams$r == x]) / length(voc), Word = subs.onegrams$Term)
    #print(predictions.one)
    #alpha1 <- (1 - sum(predictions.two$Prob)) / (1 - sum(predictions.one$Prob))
    #predictions.one <- predictions.one[, Prob := alpha1 * Prob]
    #subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print("Fourgrams used")
    #print(predictions.four)
    #subs.fourgrams <- subs.fourgrams[order(-p)]
    #predictions <- subs.fourgrams$Last1
    #print(predictions.three)
    #print(predictions.two)
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    #print(Probs)
    #print(Words)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[order(-Prob)]
    print(predictions)
    
    return(unique(predictions$Word)[1:min(nbre.pred, nrow(predictions))])
}

## Stupid backoff

PredictNextWord3 <- function(sentence, nbre.pred = 4) {
    split <- unlist(str_split(sentence, " "))
    #print(split)
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    #print("split = ")
    #print(split)
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    #print(unigram)
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print("subs.fourgrams = ")
    #print(subs.fourgrams)
    #print(subs.fourgrams)
    subs.threegrams <- freq.threegrams.unk[First2 == bigram]
    subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
    #print("subs.threegrams = ")
    #print(subs.threegrams)
    #print(subs.threegrams)
    #print("subs.twograms = ")
    subs.twograms <- freq.twograms.unk[First1 == unigram]
    subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
    #print(subs.twograms)
    subs.onegrams <- freq.onegrams[3:(nbre.pred + 2)]
    #print(subs.twograms)
    c.trigram <- freq.threegrams.unk[Term == trigram]
    #print("c.trigram = ")
    #print(c.trigram)
    c.bigram <- freq.twograms.unk[Term == bigram]
    #print("c.bigram = ")
    #print(c.bigram)
    c.unigram <- freq.onegrams[Term == unigram]
    #print("c.unigram = ")
    #print(c.unigram)
    #subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    #print("Fourgrams used")
    predictions.four <- data.table(Prob = subs.fourgrams$Freq / c.trigram$Freq, Word = subs.fourgrams$Last1)
    #print(predictions.four)
    #subs.fourgrams <- subs.fourgrams[order(-p)]
    #predictions <- subs.fourgrams$Last1
    predictions.three <- data.table(Prob = 0.4 * subs.threegrams$Freq / c.bigram$Freq, Word = subs.threegrams$Last1)
    #print(predictions.three)
    predictions.two <- data.table(Prob = 0.4 * subs.twograms$Freq / c.unigram$Freq, Word = subs.twograms$Last1)
    #print(predictions.two)
    predictions.one <- data.table(Prob = 0.4 * subs.onegrams$Freq / nrow(freq.onegrams), Word = subs.onegrams$Term)
    Probs <- c(predictions.four$Prob, predictions.three$Prob, predictions.two$Prob, predictions.one$Prob)
    Words <- c(predictions.four$Word, predictions.three$Word, predictions.two$Word, predictions.one$Word)
    predictions <- data.table(Word = Words, Prob = Probs)
    predictions <- predictions[order(-Prob)]
    #print(predictions)
        
    return(unique(predictions$Word)[1:min(nbre.pred, nrow(predictions))])
}

PredictNextWord4 <- function(sentence, nbre.pred = 4) {
    split <- unlist(str_split(sentence, " "))
    if (split[1] == "") {
        split <- c("<bos>")
    }
    else {   
        split <- c("<bos>", split)
    }
    len <- length(split)
    trigram <- paste(split[len - 2], split[len - 1], split[len])
    bigram <- paste(split[len - 1], split[len])
    unigram <- split[len]
    
    subs.fourgrams <- freq.fourgrams.unk[First3 == trigram]
    subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<eos>", profanities))]
    if (nrow(subs.fourgrams) > 2) {
        #print("Fourgrams used")
        subs.fourgrams$p <- sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$p[SGT.fourgrams$r == x])
        subs.fourgrams <- subs.fourgrams[order(-p)]
        predictions <- subs.fourgrams$Last1
        return(predictions[1:min(nbre.pred, length(predictions))])
    }
    else {
        subs.threegrams <- freq.threegrams.unk[First2 == bigram]
        subs.threegrams <- subs.threegrams[!(Last1 %in% c("<eos>", profanities))]
        if (nrow(subs.threegrams) > 3) {
            #print("Trigrams used")
            subs.threegrams$p <- sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$p[SGT.threegrams$r == x])
            subs.threegrams <- subs.threegrams[order(-p)]
            predictions <- subs.threegrams$Last1
            return(predictions[1:min(nbre.pred, length(predictions))])
        }
        else {
            subs.twograms <- freq.twograms.unk[First1 == unigram]
            subs.twograms <- subs.twograms[!(Last1 %in% c("<eos>", profanities))]
            if (nrow(subs.twograms) > 4) {
                #print("Bigrams used")
                subs.twograms$p <- sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$p[SGT.twograms$r == x])
                subs.twograms <- subs.twograms[order(-p)]
                predictions <- subs.twograms$Last1
                return(predictions[1:min(nbre.pred, length(predictions))])
            }
            else
                return(freq.onegrams$Term[1:nbre.pred])
        }
    }
}

# Processing of sentence entered

ReplaceAbbr <- function(x) {
    # Replaces every occurence of typical abbreviations in english (like "Mr.", "a.m.", "o.k.", ...)
    
    # Returns a corpus which is the result of the previous replacements
    
    x <- gsub("Mr\\.", "Mr", x)
    x <- gsub("Ms\\.", "Ms", x)
    x <- gsub("Mrs\\.", "Mrs", x)
    x <- gsub("i\\.e\\.", "ie", x)
    x <- gsub("A\\.D\\.", "AD", x)
    x <- gsub("B\\.C\\.", "BC", x)
    x <- gsub("A\\.M\\.", "am", x)
    x <- gsub("P\\.M\\.", "pm", x)
    x <- gsub("a\\.m\\.", "am", x)
    x <- gsub("p\\.m\\.", "pm", x)
    x <- gsub("et al\\.", "et al", x)
    x <- gsub("Jr\\.", "Jr", x)
    x <- gsub("Sr\\.", "Sr", x)
    x <- gsub("Dr\\.", "Dr", x)
    x <- gsub("Sen\\.", "Sen", x)
    x <- gsub("Rep\\.", "Sen", x)
    x <- gsub("Gov\\.", "Sen", x)
    x <- gsub("PhD\\.", "PhD", x)
    x <- gsub("Sgt\\.", "Sgt", x)
    x <- gsub("Lt\\.", "Lt", x)
    x <- gsub("Gen\\.", "Gen", x)
    x <- gsub("U\\.S\\.", "US", x)
    x <- gsub("U\\.K\\.", "UK", x)
    x <- gsub("U\\.S\\.A\\.", "USA", x)
    x <- gsub("U\\.N\\.", "UN", x)
    x <- gsub("N\\.Y\\.", "NY", x)
    x <- gsub("L\\.A\\.", "LA", x)
    x <- gsub("Co\\.", "Co", x)
    x <- gsub("p\\.s\\.", "ps", x)
    x <- gsub("St\\.", "St", x)
    x <- gsub("Ave\\.", "Ave", x)
    x <- gsub("Blvd\\.", "Blvd", x)
    x <- gsub("o\\.k\\.", "ok", x)
    x <- gsub("vs\\.", "vs", x)
    x <- gsub("Inc\\.", "Inc", x)
    x <- gsub("Corp\\.", "Corp", x)
    x <- gsub("W\\.", "W", x)
    x <- gsub("A\\.", "A", x)
    x <- gsub("E\\.", "E", x)
    x <- gsub("J\\.", "J", x)
    x <- gsub("S\\.", "S", x)
    x <- gsub("N\\.", "N", x)
    x <- gsub("M\\.", "M", x)
    x <- gsub("L\\.", "L", x)
    x <- gsub("C\\.", "C", x)
    x <- gsub("F\\.", "F", x)
    x <- gsub("B\\.", "B", x)
    x <- gsub("G\\.", "G", x)
    x <- gsub("H\\.", "H", x)
    x <- gsub("P\\.", "P", x)
    x <- gsub("T\\.", "T", x)
    x <- gsub("w\\/", "with", x)
    x <- gsub("\\&", "and", x)
    x <- gsub("m\\.r\\.i\\.", "mri", x)
    x <- gsub("\\/", " ", x)
    x <- gsub("Jan\\.", "Jan", x)
    x <- gsub("Feb\\.", "Feb", x)
    x <- gsub("Mar\\.", "Mar", x)
    x <- gsub("Apr\\.", "Apr", x)
    x <- gsub("Aug\\.", "Aug", x)
    x <- gsub("Sept\\.", "Sept", x)
    x <- gsub("Oct\\.", "Oct", x)
    x <- gsub("Nov\\.", "Nov", x)
    x <- gsub("Dec\\.", "Dec", x)
    
    return(x)
}


RemoveDotsNums <- function(x) gsub("([0-9]*)\\.([0-9]+)", "\\1\\2", x)
# Removes every occurence of dots in decimal numbers in the corpus

# Returns a corpus which is the result of the previous removals


RemoveCommaNums <- function(x) gsub("([0-9]+),([0-9]+)", "\\1\\2", x)
# Removes every occurence of spaces in numbers in the corpus

# Returns a corpus which is the result of the previous removals


#ReplaceNumbers <- function(x) gsub("[0-9]+", "99", x)
# Replaces every occurence of numbers in the corpus with "99"

# Returns a corpus which is the result of the previous replacements


RemoveEmail <- function(x) gsub("[A-z0-9._%+-]+@([A-z0-9-]+)\\.(?:[A-z]{2}|com|org|net|edu|gov|mil|biz|info|mobi|name|aero|asia|jobs|museum)", "\\1", x)
# Removes every occurence of email addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveURL <- function(x) gsub("(((https?|ftp)://)?www\\.)?([A-z0-9]+)\\.[A-z0-9]{2,}", "\\4", x)
# Removes every occurence of URLs addresses in the corpus

# Returns a corpus which is the result of the previous removals


RemoveLRSQuotation <- function(x) {
    # Replaces every occurence of single left and right quotation marks with "\'" in the corpus
    # Preserves single left quotation mark used to abbreviate a word and replaces it with "\'"
    
    # Returns a corpus which is the result of the previous replacements
    
    x <- gsub("\u2018(([A-z0-9]+\\s?)+)\u2019", "\\1", x)
    x <- gsub("\u2018|\u2019", "\'", x)
    
    return(x)
}


RemoveLRDQuotation <- function(x) gsub("\u201c|\u201d", "\"", x)
# Replaces every occurence of double left and right quotation marks with "\'" in the corpus

# Returns a corpus which is the result of the previous replacements


RemoveDashes <- function(x) gsub("-+|—", " ", x)
# Removes every occurence of of one or more dashes in the corpus

# Returns a corpus which is the result of the previous removals


TransformIs <- function(x) {
    # Transforms every occurence of low case "i'" with the equivalent "I'"
    
    # Returns a corpus which is the result of the previous transformations
    
    x <- gsub("^i ", "I ", x)
    x <- gsub("^i'", "I'", x)
    x <- gsub(" i ", " I ", x)
    x <- gsub(" i'", " I'", x)
    x <- gsub(".i ", ". I ", x)
    x <- gsub(".i'", ". I'", x)
    
    return(x)
}


TransformDots <- function(x) gsub("[:?!|\u2026\\.]+", ".", x)
# Transforms every occurence of sentence termination marks a dot in the corpus

# Returns a corpus which is the result of the previous transformations


RemovePunc <- function(x) gsub('[])(,#%$^*\\~{}[&+=@/"`|<>_]+', " ", x)
# Removes every occurence of punctuation marks in the corpus

# Returns a corpus which is the result of the previous removals


SplitSentence <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1, delimiters = "\\r\\n\\t.:?!;|\u2026"))
# Splits every sentence according to termination marks in the corpus

# Returns a corpus which is the result of the sentence splitting


RemoveTrailingSpaces <- function(x) gsub("^\\s|\\s$", "", x)
# Removes every occurence of trailing spaces in the corpus

# Returns a corpus which is the result of the previous removals


RemoveEmptyStrings <- function(x) {
    # Removes every empty string in the corpus
    
    # Returns a corpus which is the result of the previous removals
    
    return(x[x != ""])
}

CleanSentence <- function(sentence) {
    temp <- ReplaceAbbr(sentence)
    temp <- RemoveDotsNums(temp)
    temp <- RemoveCommaNums(temp)
    temp <- RemoveEmail(temp)
    temp <- RemoveURL(temp)
    temp <- RemoveLRSQuotation(temp)
    temp <- RemoveLRDQuotation(temp)
    temp <- RemoveDashes(temp)
    temp <- tolower(temp)
    #temp <- TransformIs(temp)
    temp <- iconv(temp, from = "latin1", to = "ASCII", sub = "")
    temp <- TransformDots(temp)
    temp <- RemovePunc(temp)
    temp <- stripWhitespace(temp)
    temp <- SplitSentence(temp)
    temp <- RemoveTrailingSpaces(temp)
    temp <- RemoveEmptyStrings(temp)
    temp <- unique(temp)
    l <- length(temp)
    
    return(temp[l])
}

#freq.twograms.unk <- freq.twograms.unk[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])]
#freq.twograms.unk <- freq.twograms.unk[, First1 := NULL][, Last1 := NULL]
#freq.twograms.unk <- freq.twograms.unk[freq.twograms.unk$Freq > 5]
freq.twograms.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")]
freq.twograms.unk <- freq.twograms.unk[, Term := paste(Word1, Word2)]
freq.twograms.unk <- freq.twograms.unk[, lapply(.SD, sum), by = "Term,Word1,Word2"]
freq.twograms.unk <- freq.twograms.unk[order(-Freq)]
save(freq.twograms.unk, file = "rdata/final/freq-twograms-def2.RData")

#freq.threegrams.unk <- freq.threegrams.unk[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])][, Word3 := sapply(Term, function(x) unlist(str_split(x, " "))[3])]
#freq.threegrams.unk <- freq.threegrams.unk[, First2 := NULL][, Last1 := NULL]
freq.threegrams.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")][, Word3 := ifelse(Word3 %in% voc, Word3, "<unk>")]
freq.threegrams.unk <- freq.threegrams.unk[, Term := paste(Word1, Word2, Word3)]
freq.threegrams.unk <- freq.threegrams.unk[, lapply(.SD, sum), by = "Term,Word1,Word2,Word3"]
freq.threegrams.unk <- freq.threegrams.unk[order(-Freq)]
save(freq.threegrams.unk, file = "rdata/final/freq-threegrams-def2.RData")

#freq.fourgrams.unk <- freq.fourgrams.unk[, Word1 := sapply(Term, function(x) unlist(str_split(x, " "))[1])][, Word2 := sapply(Term, function(x) unlist(str_split(x, " "))[2])][, Word3 := sapply(Term, function(x) unlist(str_split(x, " "))[3])][, Word4 := sapply(Term, function(x) unlist(str_split(x, " "))[4])]
#freq.fourgrams.unk <- freq.fourgrams.unk[, First3 := NULL][, Last1 := NULL]
freq.fourgrams.unk[, Word1 := ifelse(Word1 %in% voc, Word1, "<unk>")][, Word2 := ifelse(Word2 %in% voc, Word2, "<unk>")][, Word3 := ifelse(Word3 %in% voc, Word3, "<unk>")][, Word4 := ifelse(Word4 %in% voc, Word4, "<unk>")]
freq.fourgrams.unk <- freq.fourgrams.unk[, Term := paste(Word1, Word2, Word3, Word4)]
freq.fourgrams.unk <- freq.fourgrams.unk[, lapply(.SD, sum), by = "Term,Word1,Word2,Word3,Word4"]
freq.fourgrams.unk <- freq.fourgrams.unk[order(-Freq)]
save(freq.fourgrams.unk, file = "rdata/final/freq-fourgrams-def2.RData")

