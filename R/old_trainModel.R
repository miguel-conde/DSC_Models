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
create_dir(modelDirectory)
create_dir(samplesDirectory)
create_dir(samplesTrainDirectory)
create_dir(samplesTestDirectory)

# p_blogs   <- 2*0.4/100
# p_news    <- 2*0.4/100
# p_twitter <- 2*0.5/100

# GOOD
# p_blogs   <- 1.4/100
# p_news    <- 1.5/100
# p_twitter <- 1.7/100

p_blogs   <- 1.4/1000
p_news    <- 1.5/1000
p_twitter <- 1.7/1000

sampleFiles(blogsTrainingFile,   blogsSampleTrainFile_1G,   p = p_blogs)
sampleFiles(newsTrainingFile,    newsSampleTrainFile_1G,    p = p_news)
sampleFiles(twitterTrainingFile, twitterSampleTrainFile_1G, p = p_twitter)


## 3 - Corpus and TDM for 1G ###############################################
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

## 95% tokens coverage ###################################################
#cutOffBlogs <- 4; cutOffNews <- 3; cutOffTwitter <- 4; cutOffTotal <- 7
#cutOffBlogs <- 2; cutOffNews <- 1; cutOffTwitter <- 1; cutOffTotal <- 3
cutOffBlogs <- 2; cutOffNews <- 2; cutOffTwitter <- 2; cutOffTotal <- 3

statsNWordGrams(train_tdm_1Ga, "blogsSampleTrainFile_1G.txt",   count = cutOffBlogs)
statsNWordGrams(train_tdm_1Ga, "newssSampleTrainFile_1G.txt",   count = cutOffNews)
statsNWordGrams(train_tdm_1Ga, "twitterSampleTrainFile_1G.txt", count = cutOffTwitter)
statsNWordGrams(train_tdm_1Ga, count = cutOffTotal)

statsCorpus(train_tdm_1Ga, c(cutOffBlogs, cutOffNews, cutOffTwitter, cutOffTotal))

## Dictionaries 1G #######################################################

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

## Substitute OOV by UNK in sampled text files ##############################

# BLOGS
OOVdictionaryBlogs_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 1, cutOffBlogs)
train_en_US_Blogs      <- unifyOOV(train_en_US_1Ga, 1, OOVdictionaryBlogs_1Ga)
save(train_en_US_Blogs, file = trainCorpusBlogsFile_1G)

# Just to check:
ctrl <- list(
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make1Wordgrams)
test_tdm <- TermDocumentMatrix(train_en_US_Blogs, control = ctrl)

m_test_tdm<-as.matrix(test_tdm)
m_test_tdm<-m_test_tdm[order(m_test_tdm[,1], decreasing = T),]
m_test_tdm["<unk>"]

# NEWS
OOVdictionaryNews_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 2, cutOffNews)
train_en_US_News      <- unifyOOV(train_en_US_1Ga, 2, OOVdictionaryNews_1Ga)
save(train_en_US_News, file = trainCorpusNewsFile_1G)

# TWITTER
OOVdictionaryTwitter_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 3, cutOffTwitter)
train_en_US_Twitter      <- unifyOOV(train_en_US_1Ga, 3, OOVdictionaryTwitter_1Ga)
save(train_en_US_Twitter, file = trainCorpusTwitterFile_1G)

# TOTAL - ESTE ES PRESCINDIBLE
# OOVdictionaryTotal_1Ga <- OOV_dictionary(m_train_tdm_1Ga, 4, cutOffTotal)
# train_en_US_Total      <- unifyOOV(train_en_US_1Ga, 4, OOVdictionaryTotal_1Ga)
# save(train_en_US_Total, file = trainCorpusTotalFile_1G)


## Rebuild corpus with 3 files ##############################################

# Extract from various corpus the PlainTextDocuments 
ptd_blogs   <- PlainTextDocument(train_en_US_Blogs[[1]])
ptd_news    <- PlainTextDocument(train_en_US_News[[1]])
ptd_twitter <- PlainTextDocument(train_en_US_Twitter[[1]])

# Build a new corpus with just this document
corpus_en_US_train <- VCorpus(VectorSource(c(ptd_blogs,  ptd_news, ptd_twitter))) 
meta(corpus_en_US_train[[1]],    tag = "id") <- 
  meta(train_en_US_Blogs[[1]],   tag = "id")
meta(corpus_en_US_train[[2]],    tag = "id") <- 
  meta(train_en_US_News[[1]],    tag = "id")
meta(corpus_en_US_train[[3]],    tag = "id") <- 
  meta(train_en_US_Twitter[[1]], tag = "id")

# SAVE IT!
save(corpus_en_US_train, file = corpus_en_US_trainFile)

## Create and save N-GRAMS TDMs #############################################
make2Wordgrams <- function(x) makeNWordgrams(x, 2)
make3Wordgrams <- function(x) makeNWordgrams(x, 3)
make4Wordgrams <- function(x) makeNWordgrams(x, 4)

# 1-GRAMS
ctrl <- list(
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make1Wordgrams)

tdm_train_1G <- TermDocumentMatrix(corpus_en_US_train, control = ctrl)

save(tdm_train_1G, file = tdm_trainFile_1G)

# 2-GRAMS
ctrl <- list(
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make2Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX ", grep("^.*$", x, value = TRUE), " ETX")
})

corpus_en_US_train_2G <- tm_map(corpus_en_US_train, addLimits)

tdm_train_2G <- TermDocumentMatrix(corpus_en_US_train_2G, control = ctrl)

save(tdm_train_2G, file = tdm_trainFile_2G)

# 3-GRAMS
ctrl <- list(
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make3Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX STX ", grep("^.*$", x, value = TRUE), " ETX ETX")
})

corpus_en_US_train_3G <- tm_map(corpus_en_US_train, addLimits)

tdm_train_3G <- TermDocumentMatrix(corpus_en_US_train_3G, control = ctrl)

save(tdm_train_3G, file = tdm_trainFile_3G)

# 4-GRAMS
ctrl <- list(
  stemming = FALSE,
  wordLengths = c(1, Inf),
  tokenizer = make4Wordgrams)

addLimits <- content_transformer(function(x) {
  paste0("STX STX STX ", grep("^.*$", x, value = TRUE), " ETX ETX ETX")
})

corpus_en_US_train_4G <- tm_map(corpus_en_US_train, addLimits)

tdm_train_4G <- TermDocumentMatrix(corpus_en_US_train_4G, control = ctrl)

save(tdm_train_4G, file = tdm_trainFile_4G)

## Build TFLs #############################################################
create_dir(modelsDirectory)
create_dir(trainingModelsDirectory)
create_dir(trainingBlogsModelDirectory)
create_dir(trainingNewsModelDirectory)
create_dir(trainingTwitterModelDirectory)
create_dir(trainingTotalModelDirectory)

# BLOGS
tfl_Blogs_1G <- makeTFL(as.matrix(tdm_train_1G), N = 1, numDoc = 1, 
                        counts = TRUE, log = FALSE)
tfl_Blogs_2G <- makeTFL(as.matrix(tdm_train_2G), N = 2, numDoc = 1, 
                        counts = TRUE, log = FALSE)
tfl_Blogs_3G <- makeTFL(as.matrix(tdm_train_3G), N = 3, numDoc = 1, 
                        counts = TRUE, log = FALSE)
tfl_Blogs_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 1, 
                        counts = TRUE, log = FALSE)

save(tfl_Blogs_1G, file = trainingBlogsModelFile_1G)
save(tfl_Blogs_2G, file = trainingBlogsModelFile_2G)
save(tfl_Blogs_3G, file = trainingBlogsModelFile_3G)
save(tfl_Blogs_4G, file = trainingBlogsModelFile_4G)

# NEWS
tfl_News_1G <- makeTFL(as.matrix(tdm_train_1G), N = 1, numDoc = 2, 
                        counts = TRUE, log = FALSE)
tfl_News_2G <- makeTFL(as.matrix(tdm_train_2G), N = 2, numDoc = 2, 
                        counts = TRUE, log = FALSE)
tfl_News_3G <- makeTFL(as.matrix(tdm_train_3G), N = 3, numDoc = 2, 
                        counts = TRUE, log = FALSE)
tfl_News_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 2, 
                        counts = TRUE, log = FALSE)

save(tfl_News_1G, file = trainingNewsModelFile_1G)
save(tfl_News_2G, file = trainingNewsModelFile_2G)
save(tfl_News_3G, file = trainingNewsModelFile_3G)
save(tfl_News_4G, file = trainingNewsModelFile_4G)

# TWITTER
tfl_Twitter_1G <- makeTFL(as.matrix(tdm_train_1G), N = 1, numDoc = 3, 
                       counts = TRUE, log = FALSE)
tfl_Twitter_2G <- makeTFL(as.matrix(tdm_train_2G), N = 2, numDoc = 3, 
                       counts = TRUE, log = FALSE)
tfl_Twitter_3G <- makeTFL(as.matrix(tdm_train_3G), N = 3, numDoc = 3, 
                       counts = TRUE, log = FALSE)
tfl_Twitter_4G <- makeTFL(as.matrix(tdm_train_4G), N = 4, numDoc = 3, 
                       counts = TRUE, log = FALSE)

save(tfl_Twitter_1G, file = trainingTwitterModelFile_1G)
save(tfl_Twitter_2G, file = trainingTwitterModelFile_2G)
save(tfl_Twitter_3G, file = trainingTwitterModelFile_3G)
save(tfl_Twitter_4G, file = trainingTwitterModelFile_4G)

## TOTAL
aux1 <- as.matrix(tdm_train_1G)
aux1 <- cbind(aux1, TOTAL = apply(aux1, 1, sum))
aux2 <- as.matrix(tdm_train_2G)
aux2 <- cbind(aux2, TOTAL = apply(aux2, 1, sum))
aux3 <- as.matrix(tdm_train_3G)
aux3 <- cbind(aux3, TOTAL = apply(aux3, 1, sum))
aux4 <- as.matrix(tdm_train_4G)
aux4 <- cbind(aux4, TOTAL = apply(aux4, 1, sum))

tfl_Total_1G <- makeTFL(aux1, N = 1, numDoc = 4, 
                          counts = TRUE, log = FALSE)
tfl_Total_2G <- makeTFL(aux2, N = 2, numDoc = 4, 
                          counts = TRUE, log = FALSE)
tfl_Total_3G <- makeTFL(aux3, N = 3, numDoc = 4, 
                          counts = TRUE, log = FALSE)
tfl_Total_4G <- makeTFL(aux4, N = 4, numDoc = 4, 
                          counts = TRUE, log = FALSE)

rm(aux1); rm(aux2); rm(aux3); rm(aux4)

save(tfl_Total_1G, file = trainingTotalModelFile_1G)
save(tfl_Total_2G, file = trainingTotalModelFile_2G)
save(tfl_Total_3G, file = trainingTotalModelFile_3G)
save(tfl_Total_4G, file = trainingTotalModelFile_4G)

m.Blogs   <- list(m.1G = tfl_Blogs_1G, m.2G = tfl_Blogs_2G,
                m.3G = tfl_Blogs_3G, m.4G = tfl_Blogs_4G)
m.News    <- list(m.1G = tfl_News_1G, m.2G = tfl_News_2G,
                m.3G = tfl_News_3G, m.4G = tfl_News_4G)
m.Twitter <- list(m.1G = tfl_Twitter_1G, m.2G = tfl_Twitter_2G,
                m.3G = tfl_Twitter_3G, m.4G = tfl_Twitter_4G)
m.Total   <- list(m.1G = tfl_Total_1G, m.2G = tfl_Total_2G,
                m.3G = tfl_Total_3G, m.4G = tfl_Total_4G)

save(m.Blogs,   file = m.BlogsFile)
save(m.News,    file = m.NewsFile)
save(m.Twitter, file = m.TwitterFile)
save(m.Total,   file = m.TotalFile)
