## Loading libraries

library(tm)
library(RWeka)
library(data.table)
library(stringr)
library(ggplot2)


## Processing sentence

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
    # Applies the different steps of preprocessing to a texy string
    
    # Returns the processed first sentence of the text string entered
    
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

## Prediction of next word

profanities <- c("acrotomophilia", "anal", "anulingus", "anus", "arsehole", "ass", "asses", "asshole", "assholes", "assmunch", "autoerotic", "babeland", "bareback", "barenaked", "bastardo", "bastinado", "bbw", "bdsm", "bimbos", "birdlock", "bitch", "bitches", "blumpkin", "bollocks", "bondage", "boner", "boob", "boobs", "bukkake", "bulldyke", "bunghole", "busty", "butt", "butts", "buttcheeks", "butthole", "cameltoe", "cameltoes", "camgirl", "camslut", "camwhore", "carpet muncher", "carpetmuncher", "circlejerk", "clit", "clitoris", "clusterfuck", "cock", "cocks", "coprolagnia", "coprophilia", "cornhole", "cum", "cumming", "cunnilingus", "cunt", "cunts", "darkie", "daterape", "deepthroat", "dick", "dicks", "dildo", "doggiestyle", "doggystyle", "dolcett", "dominatrix", "dommes", "ecchi", "ejaculation", "erotic", "erotism", "escort", "eunuch", "faggot", "fecal", "felch", "fellatio", "feltch", "femdom", "figging", "fingering", "fisting", "footjob", "frotting", "fuck", "fuckoff", "fuckit", "fucked", "fucker", "fuckers", "fuckin", "fucking", "fuckyou", "futanari", "gangbang", "genitals", "gokkun", "goodpoop", "grope", "gspot", "guro", "handjob", "hardcore", "hentai", "hooker", "humping", "incest", "intercourse", "jailbait", "jigaboo", "jiggaboo", "jiggerboo", "jizz", "juggs", "kike", "kinbaku", "kinkster", "kinky", "knobbing", "lolita", "lovemaking", "masturbate", "milf", "motherfucker", "motherfuckers", "mound of venus", "muffdiving", "nambla", "nawashi", "negro", "neonazi", "nigga", "niggas", "nigger", "niggers", "nymphomania", "nipple", "nipples", "nude", "nudity", "nympho", "octopussy", "omorashi", "orgasm", "orgy", "orgies", "paedophile", "panties", "panty", "pedobear", "pedophile", "pegging", "penis", "phone sex", "pissing", "pisspig", "playboy", "ponyplay", "poof", "poopchute", "porn", "porno", "pornography", "pthc", "pubes", "pussy", "queaf", "raghead", "rape", "raping", "rapist", "rectum", "rimjob", "rimming", "sadism", "scat", "schlong", "scissoring", "semen", "sex", "sexo", "sexy", "shemale", "shibari", "shit", "shota", "shrimping", "slanteye", "slut", "sluts", "s&m", "smut", "snatch", "sodomize", "sodomy", "spic", "spooge", "strapon", "suck", "sucks", "swastika", "swinger", "threesome", "throating", "tit", "tits", "titties", "titty", "topless", "tosser", "tranny", "tribadism", "tubgirl", "tushy", "twat", "twink", "twinkie", "undressing", "upskirt", "urophilia", "vagina", "vibrator", "violet wand", "vorarephilia", "voyeur", "vulva", "wank", "wetback", "xx", "xxx", "yaoi", "yiffy", "zoophilia")

PredictNextWord <- function(sentence, nbre.pred = 4) {
    # Predicts the next word in a sentence with Simple Good-Turing and Katz Back-off procedures
    # Works with *grams-def3.RData files  
  
    # Returns a data table with the different possible predictions and their probabilities
    
    split <- unlist(str_split(sentence, " "))
    if ((split[1] == "") || (is.null(split))) {
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
    subs.onegrams <- freq.onegrams[order(-Freq)]
    subs.onegrams <- subs.onegrams[-c(1, 2)]
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

PredictNextWordApprox <- function(sentence, nbre.pred = 4) {
  # Predicts the next word in a sentence with Simple Good-Turing and Katz Back-off procedures
  # WARNING : This function uses approximate frequencies so it may overestimate the probabilities
  # Works with *grams-final.RData files
  
  # Returns a data table with the different possible predictions and their probabilities
  
  split <- unlist(str_split(sentence, " "))
  if ((split[1] == "") || (is.null(split))) {
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
  c.trigram <- sum(subs.fourgrams$Freq)
  subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.fourgrams) > 0) {
    subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
    predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$r.star[SGT.fourgrams$r == x]) / c.trigram, Word = subs.fourgrams$Last1)        
    d4 <- 1 - sum(predictions.four$Prob)
  }
  else
    predictions.four <- data.table(Prob = c(0), Word = c(""))
  subs.threegrams <- freq.threegrams.unk[First2 == bigram]
  c.bigram <- sum(subs.threegrams$Freq)
  subs.threegrams <- subs.threegrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.threegrams) > 0) {
    subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
    predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / c.bigram, Word = subs.threegrams$Last1)
    predictions.three <- predictions.three[, Prob := d4 * Prob]
    d3 <- 1 - sum(predictions.three$Prob)
  }
  else
    predictions.three <- data.table(Prob = c(0), Word = c(""))
  subs.twograms <- freq.twograms.unk[First1 == unigram]
  c.unigram <- sum(subs.twograms$Freq)
  subs.twograms <- subs.twograms[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.twograms) > 0) {
    subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
    predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / c.unigram, Word = subs.twograms$Last1)
    predictions.two <- predictions.two[, Prob := d3 * d4 * Prob]
    d2 <- 1 - sum(predictions.two$Prob)
  }
  else
    predictions.two <- data.table(Prob = c(0), Word = c(""))
  subs.onegrams <- freq.onegrams[order(-Freq)]
  subs.onegrams <- subs.onegrams[-c(1, 2)]
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

freq.fourgrams.unk$idnum <- seq_along(freq.fourgrams.unk$First3)
w123 <- tapply(freq.fourgrams.unk$idnum, freq.fourgrams.unk$First3, c)
n123 <- hash(w123)
save(n123, file = "hash123.RData")

freq.threegrams.unk$idnum <- seq_along(freq.threegrams.unk$First2)
w12 <- tapply(freq.threegrams.unk$idnum, freq.threegrams.unk$First2, c)
n12 <- hash(w12)
save(n12, file = "hash12.RData")

freq.twograms.unk$idnum <- seq_along(freq.twograms.unk$First1)
w1 <- tapply(freq.twograms.unk$idnum, freq.twograms.unk$First1, c)
n1 <- hash(w1)
save(n1, file = "hash1.RData")

PredictNextWordHash <- function(sentence, nbre.pred = 4) {
  # Predicts the next word in a sentence with Simple Good-Turing and Katz Back-off procedures
  # WARNING : This function uses approximate frequencies so it may overestimate the probabilities
  # This function uses hash files
  # Works with *grams-final.RData files
  
  # Returns a data table with the different possible predictions and their probabilities
  
  split <- unlist(str_split(sentence, " "))
  if ((split[1] == "") || (is.null(split))) {
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
  
  subs.fourgrams <- freq.fourgrams.unk[idnum %in% n123[[trigram]]]
  c.trigram <- sum(subs.fourgrams$Freq)
  subs.fourgrams <- subs.fourgrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.fourgrams) > 0) {
    subs.fourgrams <- subs.fourgrams[1:min(100, nrow(subs.fourgrams))]
    predictions.four <- data.table(Prob = sapply(subs.fourgrams$Freq, FUN = function(x) SGT.fourgrams$r.star[SGT.fourgrams$r == x]) / c.trigram, Word = subs.fourgrams$Last1)        
    d4 <- 1 - sum(predictions.four$Prob)
  }
  else
    predictions.four <- data.table(Prob = c(0), Word = c(""))
  subs.threegrams <- freq.threegrams.unk[idnum %in% n12[[bigram]]]
  c.bigram <- sum(subs.threegrams$Freq)
  subs.threegrams <- subs.threegrams[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.threegrams) > 0) {
    subs.threegrams <- subs.threegrams[1:min(100, nrow(subs.threegrams))]
    predictions.three <- data.table(Prob = sapply(subs.threegrams$Freq, FUN = function(x) SGT.threegrams$r.star[SGT.threegrams$r == x]) / c.bigram, Word = subs.threegrams$Last1)
    predictions.three <- predictions.three[, Prob := d4 * Prob]
    d3 <- 1 - sum(predictions.three$Prob)
  }
  else
    predictions.three <- data.table(Prob = c(0), Word = c(""))
  subs.twograms <- freq.twograms.unk[idnum %in% n1[[unigram]]]
  c.unigram <- sum(subs.twograms$Freq)
  subs.twograms <- subs.twograms[!(Last1 %in% c("<unk>", "<eos>", profanities))]
  if (nrow(subs.twograms) > 0) {
    subs.twograms <- subs.twograms[1:min(100, nrow(subs.twograms))]
    predictions.two <- data.table(Prob = sapply(subs.twograms$Freq, FUN = function(x) SGT.twograms$r.star[SGT.twograms$r == x]) / c.unigram, Word = subs.twograms$Last1)
    predictions.two <- predictions.two[, Prob := d3 * d4 * Prob]
    d2 <- 1 - sum(predictions.two$Prob)
  }
  else
    predictions.two <- data.table(Prob = c(0), Word = c(""))
  subs.onegrams <- freq.onegrams[order(-Freq)]
  subs.onegrams <- subs.onegrams[-c(1, 2)]
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