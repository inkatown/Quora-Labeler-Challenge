#This is necessary
#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

if (length(args)==0){
  stop("Input file must be supplied (file should be place in the same folder as code)", call. = FALSE)
}else if(length(args)==1){
  inputFile = args[1]
  
}

#inputFile <- "/home/jose/Downloads/labeler_sample.in"
con  <- file(inputFile, open = "r")

dataList <- vector()

oneLine <- readLines(con, n = 1, warn = FALSE)
parametrs <- (strsplit(oneLine, " "))
parametrs <- unlist(as.numeric(parametrs[[1]]))
#print(parametrs[1])
class(parametrs)
tope <-parametrs[1]
tope2 <-parametrs[2]

df <- vector("list", tope)
for (i in 1:tope){
  oneLine <- readLines(con, n = 1, warn = FALSE)
  myVector <- (strsplit(oneLine, " "))
  myVector <- unlist(as.numeric(myVector[[1]]))
 
  oneLine <- readLines(con, n = 1, warn = FALSE)
  dataList <- c(dataList,oneLine)
  df[[i]] <- myVector
  i <- i + 1
  
}

for (y in 1:tope2){
 
  oneLine <- readLines(con, n = 1, warn = FALSE)
  dataList <- c(dataList,oneLine)
  #print(oneLine)
}

close(con)


library(tm)   

converttobagofwords <- function(dataList) 
{
  

df = as.data.frame(dataList)


quest = Corpus(VectorSource(df$dataList)) 


quest <-tm_map(quest,content_transformer(tolower))


toSpace <- content_transformer(function(x, pattern) { return (gsub(pattern, " ", x))})
#remove punctuation
quest <- tm_map(quest, removePunctuation)
#Strip digits
quest <- tm_map(quest, removeNumbers)
#remove stopwords
quest <- tm_map(quest, removeWords, stopwords("english"))

#quest <- tm_map(quest, removeWords, c("dr","instance","id", "dn", "discharge", "summary","job","ssn","date","admission","broward", "general", "medical" ,"center","immediately"
#                                    ,"patient", "name", "medical" ,"record" ,"dob", "docproperty" ,"mrn" ,"mergeformat", "insurance", "pup" ,"medicaid" ,"accountpolicy", 
#                                    "docproperty", "custom","account","please","contact", "diagnoses","physician","hospital","dd","dt","cid","flacsbd","mg" ,"md","pcpcenter","page"))
#remove whitespace
quest <- tm_map(quest, stripWhitespace)
#Good practice to check every now and then
#Stem document
quest <- tm_map(quest,stemDocument)

#Create document-term matrix
return (DocumentTermMatrix(quest,  control = list(weighting = weightTfIdf)))
#return (DocumentTermMatrix(quest,
#                           control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE),
#                                          stopwords = TRUE)))
}



dtmTraining<- converttobagofwords(dataList)
smalldtm <- removeSparseTerms(dtmTraining , sparse= 0.9998)
remove(dataList)
remove(dtmTraining)
freqtra <- as.matrix(smalldtm)
remove(smalldtm)
library(lsa)

#final_results <- data.frame()
for (j in (tope+1):(tope+tope2)){
  #get consine values
  tempito <- outer(j:j,1:tope, FUN = Vectorize( function(i,j) cosine(freqtra[i,],freqtra[j,]) ) )
  allneigh <- match(tail(sort(as.numeric(tempito[1,])),100),tempito[1,])
  neigh <- vector()
  for(x in 1:length(allneigh)){
    indexx <- allneigh[x]
    neigh <- append(neigh,df[[indexx]][2:length(df[[indexx]])])
  }
  recomend <- as.matrix(sort(table(neigh),decreasing = TRUE))
  
  
  #final_results <- rbind(final_results , as.numeric(rownames(recomend)[1:10]))
  cat(as.numeric(rownames(recomend)[1:10]))
  cat("\n")
 
}

#write.csv(final_results, file=paste("final_label_pred.csv",sep=""),quote = FALSE)

