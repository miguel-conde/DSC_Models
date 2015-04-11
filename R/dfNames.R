#'
#' MODULE dfNames.R
#' 

## Data files and directory
dataDirectory <- file.path("..", "..", "data")
blogsFile     <- file.path(dataDirectory, "en_US.blogs.txt")
newsFile      <- file.path(dataDirectory, "en_US.news.txt")
twitterFile   <- file.path(dataDirectory, "en_US.twitter.txt")

## Training and test sets - files and directories names
setsDirectory       <- file.path(".", "sets")
trainDirectory      <- file.path(setsDirectory,   "training")
testDirectory       <- file.path(setsDirectory,   "testing")
blogsTrainingFile   <- file.path(trainDirectory,  "en_US.blogs.train.txt")
blogsTestingFile    <- file.path(testDirectory,   "en_US.blogs.test.txt")
newsTrainingFile    <- file.path(trainDirectory,  "en_US.news.train.txt")
newsTestingFile     <- file.path(testDirectory,   "en_US.news.test.txt")
twitterTrainingFile <- file.path(trainDirectory,  "en_US.twitter.train.txt")
twitterTestingFile  <- file.path(testDirectory,   "en_US.twitter.test.txt")

## Sampled files and directories
modelDirectory             <- file.path(".", "modelBO_7.5")
samplesDirectory           <- file.path(modelDirectory, "samples")
samplesTrainDirectory      <- file.path(samplesDirectory, "train_samples")
samplesTestDirectory       <- file.path(samplesDirectory, "test_samples")
blogsSampleTrainFile_1G    <- 
  file.path(samplesTrainDirectory, "blogsSampleTrainFile_1G.txt")
newsSampleTrainFile_1G     <- 
  file.path(samplesTrainDirectory, "newsSampleTrainFile_1G.txt")
twitterSampleTrainFile_1G <- 
  file.path(samplesTrainDirectory, "twitterSampleTrainFile_1G.txt")
blogsSampleTestFile_1G    <- 
  file.path(samplesTestDirectory, "blogsSampleTestFile_1G.txt")
newsSampleTestFile_1G     <- 
  file.path(samplesTestDirectory, "newsSampleTestFile_1G.txt")
twitterSampleTestFile_1G <- 
  file.path(samplesTestDirectory, "twitterSampleTestFile_1G.txt")


## Corpus and TDMs files and directories
corpusTDMsDirectory         <- file.path(modelDirectory, "corpusTDMs")
trainingCorpusTDMsDirectory <- file.path(corpusTDMsDirectory, "trainCorpusTDMs")
testingCorpusTDMsDirectory  <- file.path(corpusTDMsDirectory, "testCorpusTDMs")
trainCorpusFile_1Ga <- 
  file.path(trainingCorpusTDMsDirectory,  "en_US.train_1Ga.RData")
testCorpusFile_1Ga <- 
  file.path(testingCorpusTDMsDirectory,  "en_US.test_1Ga.RData")
trainTdmFile_1Ga <- 
  file.path(trainingCorpusTDMsDirectory,  "tdm.train_1Ga.RData")
testTdmFile_1Ga <- 
  file.path(testingCorpusTDMsDirectory,  "tdm.test_1Ga.RData")
trainCorpusBlogsFile_1G <- 
  file.path(trainingCorpusTDMsDirectory,  "en_US.train_Blogs.RData")
trainCorpusNewsFile_1G <- 
  file.path(trainingCorpusTDMsDirectory,  "en_US.train_News.RData")
trainCorpusTwitterFile_1G <- 
  file.path(trainingCorpusTDMsDirectory,  "en_US.train_Twitter.RData")
trainCorpusTotalFile_1G <- 
  file.path(trainingCorpusTDMsDirectory,  "en_US.train_Total.RData")

corpus_en_US_trainFile <- 
  file.path(trainingCorpusTDMsDirectory,  "corpus_en_US_train.RData")

tdm_trainFile_1G <- 
  file.path(trainingCorpusTDMsDirectory,  "tdm_trainFile_1G.RData")
tdm_trainFile_2G <- 
  file.path(trainingCorpusTDMsDirectory,  "tdm_trainFile_2G.RData")
tdm_trainFile_3G <- 
  file.path(trainingCorpusTDMsDirectory,  "tdm_trainFile_3G.RData")
tdm_trainFile_4G <- 
  file.path(trainingCorpusTDMsDirectory,  "tdm_trainFile_4G.RData")
tdm_testFile_4G <- 
  file.path(testingCorpusTDMsDirectory,  "tdm_testFile_4G.RData")




## Dictionaries and m_tdms files
dictionaryBlogsFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "dictionaryBlogsFile_1Ga.RData")
dictionaryNewsFile_1Ga    <- 
  file.path(trainingCorpusTDMsDirectory,  "dictionaryNewsFile_1Ga.RData")
dictionaryTwitterFile_1Ga <- 
  file.path(trainingCorpusTDMsDirectory,  "dictionaryTwitterFile_1Ga.RData")
dictionaryTotalFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "dictionaryTotalFile_1Ga.RData")

m_trainTdmBlogsFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "m_trainTdmBlogsFile_1Ga.RData")
m_trainTdmNewsFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "m_trainTdmNewsFile_1Ga.RData")
m_trainTdmTwitterFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "m_trainTdmTwitterFile_1Ga.RData")
m_trainTdmTotalFile_1Ga   <- 
  file.path(trainingCorpusTDMsDirectory,  "m_trainTdmTotalFile_1Ga.RData")



## Model's TFLs
modelsDirectory         <- file.path(modelDirectory, "modelsObj")
trainingModelsDirectory <- file.path(modelsDirectory, "trainModels")
trainingBlogsModelDirectory <- file.path(trainingModelsDirectory, "trainBlogModels")
trainingBlogsModelFile_1G <- file.path(trainingBlogsModelDirectory, "trainBlogsModel1G.RData")
trainingBlogsModelFile_2G <- file.path(trainingBlogsModelDirectory, "trainBlogsModel2G.RData")
trainingBlogsModelFile_3G <- file.path(trainingBlogsModelDirectory, "trainBlogsModel3G.RData")
trainingBlogsModelFile_4G <- file.path(trainingBlogsModelDirectory, "trainBlogsModel4G.RData")
m.BlogsFile <- file.path(trainingBlogsModelDirectory, "m.Blogs.RData")

trainingNewsModelDirectory <- file.path(trainingModelsDirectory, "trainNewsModels")
trainingNewsModelFile_1G <- file.path(trainingNewsModelDirectory, "trainNewsModel1G.RData")
trainingNewsModelFile_2G <- file.path(trainingNewsModelDirectory, "trainNewsModel2G.RData")
trainingNewsModelFile_3G <- file.path(trainingNewsModelDirectory, "trainNewsModel3G.RData")
trainingNewsModelFile_4G <- file.path(trainingNewsModelDirectory, "trainNewsModel4G.RData")
m.NewsFile <- file.path(trainingNewsModelDirectory, "m.News.RData")

trainingTwitterModelDirectory <- file.path(trainingModelsDirectory, "trainTwitterModels")
trainingTwitterModelFile_1G <- file.path(trainingTwitterModelDirectory, "trainTwitterModel1G.RData")
trainingTwitterModelFile_2G <- file.path(trainingTwitterModelDirectory, "trainTwitterModel2G.RData")
trainingTwitterModelFile_3G <- file.path(trainingTwitterModelDirectory, "trainTwitterModel3G.RData")
trainingTwitterModelFile_4G <- file.path(trainingTwitterModelDirectory, "trainTwitterModel4G.RData")
m.TwitterFile <- file.path(trainingTwitterModelDirectory, "m.Twitter.RData")

trainingTotalModelDirectory <- file.path(trainingModelsDirectory, "trainTotalModels")
trainingTotalModelFile_1G <- file.path(trainingTotalModelDirectory, "trainTotalModel1G.RData")
trainingTotalModelFile_2G <- file.path(trainingTotalModelDirectory, "trainTotalModel2G.RData")
trainingTotalModelFile_3G <- file.path(trainingTotalModelDirectory, "trainTotalModel3G.RData")
trainingTotalModelFile_4G <- file.path(trainingTotalModelDirectory, "trainTotalModel4G.RData")
m.TotalFile <- file.path(trainingTotalModelDirectory, "m.Total.RData")


testingModelsDirectory <- file.path(modelsDirectory, "testModels")
testingBlogsModelDirectory <- file.path(testingModelsDirectory, "testBlogModels")
testingNewsModelDirectory <- file.path(testingModelsDirectory, "testNewsModels")
testingTwitterModelDirectory <- file.path(testingModelsDirectory, "testTwitterModels")
testingTotalModelDirectory <- file.path(testingModelsDirectory, "testTotalModels")


testingBlogsModelFile_4G <- file.path(testingBlogsModelDirectory, "testBlogsModel4G.RData")
mT.BlogsFile <- file.path(testingBlogsModelDirectory, "mT.Blogs.RData")

testingNewsModelFile_4G <- file.path(testingNewsModelDirectory, "testNewsModel4G.RData")
mT.NewsFile <- file.path(testingNewsModelDirectory, "mT.News.RData")

testingTwitterModelFile_4G <- file.path(testingTwitterModelDirectory, "testTwitterModel4G.RData")
mT.TwitterFile <- file.path(testingTwitterModelDirectory, "mT.Twitter.RData")

testingTotalModelFile_4G <- file.path(testingTotalModelDirectory, "testTotalModel4G.RData")
mT.TotalFile <- file.path(testingTotalModelDirectory, "mT.Total.RData")

