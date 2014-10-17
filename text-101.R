library(tm)
library(gtools)
library(textir)
setwd("<Directory Path>")
dataBL = read.csv("XX", header = TRUE)
dataTP = read.csv("XX", header = TRUE)
dataBL = na.omit(dataBL)
dataTP = na.omit(dataTP)
colnames(dataTP) = c("INC_ID", "EDATE", "ETIME", "INC_TYPE", "MSG", "PCODE" ,"Noise_Type")
colnames(dataBL) = c("INC_ID", "EDATE", "ETIME", "INC_TYPE", "MSG", "PCODE" ,"Noise_Type")
dataBLTP = smartbind(dataBL, dataTP)
#.simpleCap <- function(x) {
#    s <- strsplit(x, " ")[[1]]
#    paste(toupper(substring(s, 1, 1)), substring(s, 2),
#          sep = "", collapse = " ")
#}
# To standardize tags
dataBLTP[7] = toupper(dataBLTP$Noise_Type)
dataBLTP[7] = sub("^CONG.*","CONGREGATION", dataBLTP$Type)
dataBLTP[7] = sub("^EVE.*","EVENT", dataBLTP$Type)
dataBLTP[7] = sub("^F.*","FOODCOURT", dataBLTP$Type)
dataBLTP[7] = sub("^NEIG.*","NEIGHBOUR", dataBLTP$Type)
corpusBLTP = Corpus(VectorSource(dataBLTP$MG))
#corpusBL = Corpus(VectorSource(dataBL$MSG))
#corpusTP = Corpus(VectorSource(dataTP$MSG))
corpusBLTP = tm_map(corpusBLTP , tolower)
#corpusBL = tm_map(corpusBL , tolower)
#corpusTP = tm_map(corpusTP , tolower)
stopword = c("amp","a","able","about","across","after","all","almost","also","am",
            "among","an","and","any","are","as","at","be","because","been","but",
            "by","can","cannot","could","dear","did","do","does","either","else",
            "ever","every","for","from","get","got","had","has","have","he","her",
            "hers","him","his","how","however","i","if","in","into","is","it","its",
            "just","least","let","like","likely","may","me","might","most","must",
            "my","neither","no","nor","not","of","off","often","on","only","or",
            "other","our","own","rather","said","say","says","she","should","since",
            "so","some","than","that","the","their","them","then","there","these",
            "they","this","tis","to","too","twas","us","wants","was","we","were",
            "what","when","where","which","while","who","whom","why","will","with",
            "would","yet","you","your")
corpusBLTP = tm_map(corpusBLTP, removeWords, stopword)
#corpusBL = tm_map(corpusBL, removeWords, stopword)
#corpusTP = tm_map(corpusTP, removeWords, stopword)
corpusBLTP = tm_map(corpusBLTP, removeWords, "[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+(\\.[A-Za-z]{2,4}){1,3}")
corpusBLTP = tm_map(corpusBLTP, removeWords, "http\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(/\\S*)?")
#corpusBL = tm_map(corpusBL, removeWords, "[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+(\\.[A-Za-z]{2,4}){1,3}")
#corpusBL = tm_map(corpusBL, removeWords, "http\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(/\\S*)?")
#corpusTP = tm_map(corpusTP, removeWords, "[A-Za-z0-9\\._%+-]+@[A-Za-z0-9\\.-]+(\\.[A-Za-z]{2,4}){1,3}")
#corpusTP = tm_map(corpusTP, removeWords, "http\\://[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(/\\S*)?")
corpusBLTP = tm_map(corpusBLTP, removeWords, stopwords("english"))
corpusBLTP = tm_map(corpusBLTP, stripWhitespace)
corpusBLTP = tm_map(corpusBLTP, removeNumbers)
corpusBLTP = tm_map(corpusBLTP, removePunctuation)
corpusDTM = DocumentTermMatrix(corpusBLTP, control = list(minWordLength = 3))
corpusDTM = removeSparseTerms(corpusDTM, sparse = 0.98)
corpusMatrix = data.frame(as.matrix(corpusDTM))
dataR = cbind(dataBLTP, corpusMatrix)
corpusLDA = LDA(corpusDTM, )





#####################   SCRATCH GROUND ########################






test4 = sub("^Neighb.* n.*","Neighbourly Noise", test3)
table(test4)
