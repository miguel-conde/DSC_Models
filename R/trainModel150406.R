source("./R/dfNames.R")
source("./R/makeTTSets.R")
source("./R/sampleFiles.R")
source("./R/managePunct.R") 
source("./R/statsNWordGrams.R")
source("./R/regExpr.R")
source("./R/manageOOV.R")
source("./R/makeTFLs.R")
library(tm)
library(RWeka)
library(data.table)


create_dir <- function(newDir)
if (!file.exists(newDir)) {
  dir.create(newDir)
}


## 1 - Make training and testing sets ######################################
create_dir(setsDirectory)
create_dir(trainDirectory)
create_dir(testDirectory)


makeTTSets(blogsFile,   blogsTrainingFile,   blogsTestingFile,   p = 0.8)
makeTTSets(newsFile,    newsTrainingFile,    newsTestingFile,    p = 0.8)
makeTTSets(twitterFile, twitterTrainingFile, twitterTestingFile, p = 0.8)

## 2 - Sample files 1-G ####################################################

print(sprintf("%s - 2 - Sample files 1-G - ", date()))

create_dir(modelDirectory)
create_dir(samplesDirectory)
create_dir(samplesTrainDirectory)
create_dir(samplesTestDirectory)

# p_blogs   <- 2*0.4/100
# p_news    <- 2*0.4/100
# p_twitter <- 2*0.5/100

# GOOD
p_blogs   <- 5*1.4/100
p_news    <- 5*1.5/100
p_twitter <- 5*1.7/100

# 15%:
# p_blogs   <- 14/100
# p_news    <- 15/100
# p_twitter <- 17/100

sampleFiles(blogsTrainingFile,   blogsSampleTrainFile_1G,   p = p_blogs)
sampleFiles(newsTrainingFile,    newsSampleTrainFile_1G,    p = p_news)
sampleFiles(twitterTrainingFile, twitterSampleTrainFile_1G, p = p_twitter)

print(sprintf("%s - 2 - Sample files 1-G - END", date()))

## 3 - Corpus and TDM for 1G ###############################################

print(sprintf("%s - 3 - Corpus and TDM for 1G", date()))

create_dir(corpusTDMsDirectory)
create_dir(trainingCorpusTDMsDirectory)

# To manage punctuation
goodPunct <- content_transformer(function(x) managePunct(x))

# Tokenizer for 1-WordGrams
makeNWordgrams <- function (x, N = 3) 
  NGramTokenizer(x, Weka_control(min = N, max = N,
                                 delimiters = ' \r\n\t"'))
make1Wordgrams <- function(x) makeNWordgrams(x, 1)

# Create first 1gram corpus
(train_en_US_1Ga <- VCorpus(DirSource(samplesTrainDirectory, encoding = "UTF-8"),
                  readerControl = list(language = "en")))

# Process punctuation
train_en_US_1Ga <- tm_map(train_en_US_1Ga, goodPunct)

# Save corpus
save(train_en_US_1Ga, file = trainCorpusFile_1Ga)

## Create and save 1-GRAMS TDM
ctrl1G <- list(
  content_transformer(tolower),
  stripWhitespace = TRUE,
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make1Wordgrams)

train_tdm_1Ga <- TermDocumentMatrix(train_en_US_1Ga, control = ctrl1G)

save(train_tdm_1Ga, file = trainTdmFile_1Ga)

print(sprintf("%s - 3 - Corpus and TDM for 1G - END", date()))

## 95% tokens coverage ###################################################
#cutOffBlogs <- 4; cutOffNews <- 3; cutOffTwitter <- 4; cutOffTotal <- 7
#cutOffBlogs <- 2; cutOffNews <- 1; cutOffTwitter <- 1; cutOffTotal <- 3
# 1.5% :
cutOffBlogs <- 2; cutOffNews <- 2; cutOffTwitter <- 2; cutOffTotal <- 3
# 15% :
#cutOffBlogs <- 11; cutOffNews <- 7; cutOffTwitter <- 8; cutOffTotal <- 25

statsNWordGrams(train_tdm_1Ga, "blogsSampleTrainFile_1G.txt",   count = cutOffBlogs)
statsNWordGrams(train_tdm_1Ga, "newsSampleTrainFile_1G.txt",   count = cutOffNews)
statsNWordGrams(train_tdm_1Ga, "twitterSampleTrainFile_1G.txt", count = cutOffTwitter)
statsNWordGrams(train_tdm_1Ga, count = cutOffTotal)

statsCorpus(train_tdm_1Ga, c(cutOffBlogs, cutOffNews, cutOffTwitter, cutOffTotal))

## Dictionaries 1G #######################################################

print(sprintf("%s - Dictionaries 1G", date()))

# Build a 1-Wordgrams matrixes with just the types in the dictionaries (for
# a 95% coverage)
m_train_tdm_1Ga <- as.matrix(train_tdm_1Ga)
m_train_tdm_1Ga <- cbind(m_train_tdm_1Ga, TOTAL = apply(m_train_tdm_1Ga,1,sum))

dictionaryBlogs_1Ga   <- 
  names(m_train_tdm_1Ga[m_train_tdm_1Ga[, 1] > cutOffBlogs - 1,  1])
dictionaryNews_1Ga    <- 
  names(m_train_tdm_1Ga[m_train_tdm_1Ga[, 2] > cutOffNews - 1,    2])
dictionaryTwitter_1Ga <- 
  names(m_train_tdm_1Ga[m_train_tdm_1Ga[, 3] > cutOffTwitter - 1, 3])
dictionaryTotal_1Ga   <- 
  names(m_train_tdm_1Ga[m_train_tdm_1Ga[, 4] > cutOffTotal - 1,   4])

save(dictionaryBlogs_1Ga,   file = dictionaryBlogsFile_1Ga)
save(dictionaryNews_1Ga,    file = dictionaryNewsFile_1Ga)
save(dictionaryTwitter_1Ga, file = dictionaryTwitterFile_1Ga)
save(dictionaryTotal_1Ga,   file = dictionaryTotalFile_1Ga)

## ESTO NO VALE
m_train_tdm_blogs_1Ga   <- m_train_tdm_1Ga[dictionaryBlogs_1Ga, 1]
m_train_tdm_blogs_1Ga   <- m_train_tdm_blogs_1Ga[order(m_train_tdm_blogs_1Ga,
                                                       decreasing = TRUE)]
m_train_tdm_news_1Ga    <- m_train_tdm_1Ga[dictionaryNews_1Ga, 2]
m_train_tdm_news_1Ga    <- m_train_tdm_news_1Ga[order(m_train_tdm_news_1Ga,
                                                      decreasing = TRUE)]
m_train_tdm_twitter_1Ga <- m_train_tdm_1Ga[dictionaryTwitter_1Ga, 3]
m_train_tdm_twitter_1Ga <- m_train_tdm_twitter_1Ga[order(m_train_tdm_twitter_1Ga,
                                                         decreasing = TRUE)]
m_train_tdm_total_1Ga   <- m_train_tdm_1Ga[dictionaryTotal_1Ga, 4]
m_train_tdm_total_1Ga   <- m_train_tdm_total_1Ga[order(m_train_tdm_total_1Ga,
                                                       decreasing = TRUE)]

save(m_train_tdm_blogs_1Ga,   file = m_trainTdmBlogsFile_1Ga)
save(m_train_tdm_news_1Ga,    file = m_trainTdmNewsFile_1Ga)
save(m_train_tdm_twitter_1Ga, file = m_trainTdmTwitterFile_1Ga)
save(m_train_tdm_total_1Ga,   file = m_trainTdmTotalFile_1Ga)
##

print(sprintf("%s - Dictionaries 1G - END", date()))

## Extract OOV dictionaries from sampled text files ###########################

print(sprintf("%s - Extract OOV dictionaries from sampled text files", date()))

# BLOGS
OOVdictionaryBlogs_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 1, cutOffBlogs)

# NEWS
OOVdictionaryNews_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 2, cutOffNews)

# TWITTER
OOVdictionaryTwitter_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 3, cutOffTwitter)

# TOTAL - ESTE ES PRESCINDIBLE
OOVdictionaryTotal_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 4, cutOffTotal)

print(sprintf("%s - Extract OOV dictionaries from sampled text files - END", date()))

## Create and save N-GRAMS TDMs #############################################

print(sprintf("%s - Create and save N-GRAMS TDMs", date()))

make2Wordgrams <- function(x) makeNWordgrams(x, 2)
make3Wordgrams <- function(x) makeNWordgrams(x, 3)
make4Wordgrams <- function(x) makeNWordgrams(x, 4)


# 1-GRAMS
ctrl <- list(
  content_transformer(tolower),
  stripWhitespace = TRUE,
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make1Wordgrams)

tdm_train_1G <- TermDocumentMatrix(train_en_US_1Ga, control = ctrl)

save(tdm_train_1G, file = tdm_trainFile_1G)

print(sprintf("%s - Create and save N-GRAMS TDMs - 1GRAMS END", date()))

# 2-GRAMS
ctrl <- list(
  content_transformer(tolower),
  stripWhitespace = TRUE,
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make2Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX ", grep("^.*$", x, value = TRUE), " ETX")
})

corpus_en_US_train_2G <- tm_map(train_en_US_1Ga, addLimits)

tdm_train_2G <- TermDocumentMatrix(corpus_en_US_train_2G, control = ctrl)

save(tdm_train_2G, file = tdm_trainFile_2G)

# 3-GRAMS
ctrl <- list(
  content_transformer(tolower),
  stripWhitespace = TRUE,
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make3Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX STX ", grep("^.*$", x, value = TRUE), " ETX ETX")
})

corpus_en_US_train_3G <- tm_map(train_en_US_1Ga, addLimits)

tdm_train_3G <- TermDocumentMatrix(corpus_en_US_train_3G, control = ctrl)

save(tdm_train_3G, file = tdm_trainFile_3G)

# 4-GRAMS
ctrl <- list(
  content_transformer(tolower),
  stripWhitespace = TRUE,
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make4Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX STX STX ", grep("^.*$", x, value = TRUE), " ETX ETX ETX")
})

corpus_en_US_train_4G <- tm_map(train_en_US_1Ga, addLimits)

tdm_train_4G <- TermDocumentMatrix(corpus_en_US_train_4G, control = ctrl)

save(tdm_train_4G, file = tdm_trainFile_4G)

print(sprintf("%s - Create and save N-GRAMS TDMs - END", date()))

## Build TFLs #############################################################

print(sprintf("%s - Build TFLs", date()))

create_dir(modelsDirectory)
create_dir(trainingModelsDirectory)
create_dir(trainingBlogsModelDirectory)
create_dir(trainingNewsModelDirectory)
create_dir(trainingTwitterModelDirectory)
create_dir(trainingTotalModelDirectory)

# BLOGS

tfl_Blogs_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 1, 
                        counts = TRUE, log = FALSE)

save(tfl_Blogs_4G, file = trainingBlogsModelFile_4G)

# NEWS

tfl_News_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 2, 
                        counts = TRUE, log = FALSE)

save(tfl_News_4G, file = trainingNewsModelFile_4G)

# TWITTER

tfl_Twitter_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 3, 
                       counts = TRUE, log = FALSE)

save(tfl_Twitter_4G, file = trainingTwitterModelFile_4G)

## TOTAL
aux4 <- as.matrix(tdm_train_4G)
aux4 <- cbind(aux4, TOTAL = apply(aux4, 1, sum))

tfl_Total_4G <- makeTFL(aux4, N = 4, numDoc = 4, 
                          counts = TRUE, log = FALSE)

rm(aux4)

save(tfl_Total_4G, file = trainingTotalModelFile_4G)

print(sprintf("%s - Build TFLs - END", date()))

## Reemplace OOV words by <unk> #########################################

print(sprintf("%s - Reemplace OOV words by <unk>", date()))

# BLOGS
# setkey(tfl_Blogs_4G, WG4)
# tfl_Blogs_4G[.(OOVdictionaryBlogs_1Ga)]$WG4 <- "<unk>"
# setkey(tfl_Blogs_4G, WG3)
# tfl_Blogs_4G[.(OOVdictionaryBlogs_1Ga)]$WG3 <- "<unk>"
# setkey(tfl_Blogs_4G, WG2)
# tfl_Blogs_4G[.(OOVdictionaryBlogs_1Ga)]$WG2 <- "<unk>"
# setkey(tfl_Blogs_4G, WG1)
# tfl_Blogs_4G[.(OOVdictionaryBlogs_1Ga)]$WG1 <- "<unk>"
# setkey(tfl_Blogs_4G, WG1, WG2, WG3, WG4)
# tfl_Blogs_4G <- tfl_Blogs_4G[, sum(Count), by=key(tfl_Blogs_4G)]
# setnames(tfl_Blogs_4G, "V1", "Count")

tfl_Blogs_4G <- changeWords4G(tfl_Blogs_4G, OOVdictionaryBlogs_1Ga)

# NEWS
tfl_News_4G <- changeWords4G(tfl_News_4G, OOVdictionaryNews_1Ga)

# TWITTER
tfl_Twitter_4G <- changeWords4G(tfl_Twitter_4G, OOVdictionaryTwitter_1Ga)

# TOTAL
tfl_Total_4G <- changeWords4G(tfl_Total_4G, OOVdictionaryTotal_1Ga)

print(sprintf("%s - Reemplace OOV words by <unk> - END", date()))

## Reemplace profanity ##################################################

# TO DO #


## BLOGS

# NEWS

# TWITTER

# TOTAL

## SAVE MODELS ##########################################################

print(sprintf("%s - SAVE MODELS - ", date()))

save(tfl_Blogs_4G,     file = m.BlogsFile)
save(tfl_News_4G,      file = m.NewsFile)
save(tfl_Twitter_4G,   file = m.TwitterFile)
save(tfl_Total_4G,     file = m.TotalFile)

print(sprintf("%s - SAVE MODELS - END", date()))

## Create TFLs for 1,2 ang 3-grams ######################################

print(sprintf("%s - Create TFLs for 1,2 ang 3-grams - ", date()))

## BLOGS

# 1G
# tfl_Blogs_1G <- tfl_Blogs_4G[, sum(Count), by=WG4]
# setkey(tfl_Blogs_1G, WG4)
# tfl_Blogs_1G <- tfl_Blogs_1G[!.("etx")]
# setnames(tfl_Blogs_1G, "V1", "Count")
tfl_Blogs_1G <- makeTFL1G(tfl_Blogs_4G)
tfl_Blogs_2G <- makeTFL2G(tfl_Blogs_4G)
tfl_Blogs_3G <- makeTFL3G(tfl_Blogs_4G)

# NEWS
tfl_News_1G <- makeTFL1G(tfl_News_4G)
tfl_News_2G <- makeTFL2G(tfl_News_4G)
tfl_News_3G <- makeTFL3G(tfl_News_4G)

# TWITTER
tfl_Twitter_1G <- makeTFL1G(tfl_Twitter_4G)
tfl_Twitter_2G <- makeTFL2G(tfl_Twitter_4G)
tfl_Twitter_3G <- makeTFL3G(tfl_Twitter_4G)

# TOTAL
tfl_Total_1G <- makeTFL1G(tfl_Total_4G)
tfl_Total_2G <- makeTFL2G(tfl_Total_4G)
tfl_Total_3G <- makeTFL3G(tfl_Total_4G)

print(sprintf("%s - Create TFLs for 1,2 ang 3-grams - END", date()))

## Add frequency and logprob columns ####################################

print(sprintf("%s - Add frequency and logprob columns - ", date()))

## BLOGS
# setkey(tfl_Blogs_4G, WG1, WG2, WG3)
# tfl_Blogs_4G$Freq <- 
#   tfl_Blogs_4G[tfl_Blogs_4G[,sum(Count), by=key(tfl_Blogs_4G)]][, Count/V1]
# tfl_Blogs_4G$logProb <- log2(tfl_Blogs_4G$Freq)

tfl_Blogs_1G <- addProbs1G(tfl_Blogs_1G)
tfl_Blogs_2G <- addProbs2G(tfl_Blogs_2G)
tfl_Blogs_3G <- addProbs3G(tfl_Blogs_3G)
tfl_Blogs_4G <- addProbs4G(tfl_Blogs_4G)

# NEWS
tfl_News_1G <- addProbs1G(tfl_News_1G)
tfl_News_2G <- addProbs2G(tfl_News_2G)
tfl_News_3G <- addProbs3G(tfl_News_3G)
tfl_News_4G <- addProbs4G(tfl_News_4G)

# TWITTER
tfl_Twitter_1G <- addProbs1G(tfl_Twitter_1G)
tfl_Twitter_2G <- addProbs2G(tfl_Twitter_2G)
tfl_Twitter_3G <- addProbs3G(tfl_Twitter_3G)
tfl_Twitter_4G <- addProbs4G(tfl_Twitter_4G)

# TOTAL
tfl_Total_1G <- addProbs1G(tfl_Total_1G)
tfl_Total_2G <- addProbs2G(tfl_Total_2G)
tfl_Total_3G <- addProbs3G(tfl_Total_3G)
tfl_Total_4G <- addProbs4G(tfl_Total_4G)

print(sprintf("%s - Add frequency and logprob columns - END", date()))

## Build models #########################################################

# m.Blogs   <- list(m.1G = tfl_Blogs_1G, m.2G = tfl_Blogs_2G,
#                 m.3G = tfl_Blogs_3G, m.4G = tfl_Blogs_4G)
# m.News    <- list(m.1G = tfl_News_1G, m.2G = tfl_News_2G,
#                 m.3G = tfl_News_3G, m.4G = tfl_News_4G)
# m.Twitter <- list(m.1G = tfl_Twitter_1G, m.2G = tfl_Twitter_2G,
#                 m.3G = tfl_Twitter_3G, m.4G = tfl_Twitter_4G)
# m.Total   <- list(m.1G = tfl_Total_1G, m.2G = tfl_Total_2G,
#                 m.3G = tfl_Total_3G, m.4G = tfl_Total_4G)
# 
# save(m.Blogs,   file = m.BlogsFile)
# save(m.News,    file = m.NewsFile)
# save(m.Twitter, file = m.TwitterFile)
# save(m.Total,   file = m.TotalFile)

date()
