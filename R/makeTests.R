#' test testingModels
#' 
source("./R/makeTFLs.R")
source("./R/testingModels.R")
source("./R/predict.R")

## NO INTERPOLATION TESTS ####################################################
date()

sizeSample = 300
nSamples = 30

## BLOGS
# Load train model
trainModel <- loadModelBlogs()
load(mT.BlogsFile)
testModel <- tfl_Blogs_4G
rm(tfl_Blogs_4G)
gc()

# No final sample
system.time(
  resBlogs <- testModels (trainModel, testModel, 
                   sizeSample = sizeSample, nSamples = nSamples,
                   finalSample = FALSE) 
)

resBlogs 

# Final Sample
system.time(
  resBlogsFS <- testModels (trainModel, testModel, 
                     sizeSample = sizeSample, nSamples = nSamples,
                     finalSample = TRUE) 
)

resBlogsFS

## NEWS
# Load train model
trainModel <- loadModelNews()
load(mT.NewsFile)
testModel <- tfl_News_4G
rm(tfl_News_4G)
gc()

# No final sample
system.time(
  resNews <- testModels (trainModel, testModel, 
                     sizeSample = sizeSample, nSamples = nSamples,
                     finalSample = FALSE) 
)

resNews

# Final Sample
system.time(
  resNewsFS <- testModels (trainModel, testModel, 
                            sizeSample = sizeSample, nSamples = nSamples,
                            finalSample = TRUE) 
)

resNewsFS

## TWITTER
# Load train model
trainModel <- loadModelTwitter()
load(mT.TwitterFile)
testModel <- tfl_Twitter_4G
rm(tfl_Twitter_4G)
gc()

# No final sample
system.time(
  resTwitter <- testModels (trainModel, testModel, 
                         sizeSample = sizeSample, nSamples = nSamples,
                         finalSample = FALSE) 
)

resTwitter

# Final Sample
system.time(
  resTwitterFS <- testModels (trainModel, testModel, 
                           sizeSample = sizeSample, nSamples = nSamples,
                           finalSample = TRUE) 
)

resTwitterFS

## TOTAL
# Load train model
trainModel <- loadModelTotal()
load(mT.TotalFile)
testModel <- tfl_Total_4G
rm(tfl_Total_4G)
gc()

# No final sample
system.time(
  resTotal <- testModels (trainModel, testModel, 
                            sizeSample = sizeSample, nSamples = nSamples,
                            finalSample = FALSE) 
)

resTotal

# Final Sample
system.time(
  resTotalFS <- testModels (trainModel, testModel, 
                              sizeSample = sizeSample, nSamples = nSamples,
                              finalSample = TRUE) 
)

resTotalFS

tbl_res <- 
  data.frame(Model = c("Blogs", "Blogs", "News", "News", 
                       "Twitter", "Twitter", "Total", "Total"),
             Final.Sample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
             Inf.Conf.Int = c(resBlogs$conf.int[1],   resBlogsFS$conf.int[1],
                              resNews$conf.int[1],    resNewsFS$conf.int[1],
                              resTwitter$conf.int[1], resTwitterFS$conf.int[1],
                              resTotal$conf.int[1],   resTotalFS$conf.int[1]),
             Mean = c(resBlogs$estimate,   resBlogsFS$estimate,
                      resNews$estimate,    resNewsFS$estimate,
                      resTwitter$estimate, resTwitterFS$estimate,
                      resTotal$estimate,   resTotalFS$estimate), 
             Sup.Conf.Int = c(resBlogs$conf.int[2],   resBlogsFS$conf.int[2],
                              resNews$conf.int[2],    resNewsFS$conf.int[2],
                              resTwitter$conf.int[2], resTwitterFS$conf.int[2],
                              resTotal$conf.int[2],   resTotalFS$conf.int[2]))

tbl_res

t.test(tbl_res$Mean)

t.test(tbl_res[tbl_res$Final.Sample == FALSE,]$Mean)

t.test(tbl_res[tbl_res$Final.Sample == TRUE,]$Mean)

plot(tbl_res[tbl_res$Final.Sample == FALSE,]$Mean, type="o", ylim = c(0.08,0.30), col = "blue")
lines(tbl_res[tbl_res$Final.Sample == FALSE,]$Inf.Conf.Int, type="l", col = "blue")
lines(tbl_res[tbl_res$Final.Sample == FALSE,]$Sup.Conf.Int, type="l", col = "blue")
lines(tbl_res[tbl_res$Final.Sample == TRUE,]$Mean, type="o", col = "red")
lines(tbl_res[tbl_res$Final.Sample == TRUE,]$Inf.Conf.Int, type="l", col = "red")
lines(tbl_res[tbl_res$Final.Sample == TRUE,]$Sup.Conf.Int, type="l", col = "red")

date()

## INTERPOLATION TESTS ####################################################
date()
sizeSample = 300
nSamples = 30

l_Blogs   <- c(0.01561975, 0.03848051, 0.04571407, 0.90018566)
l_News    <- c(0.02095653, 0.05361416, 0.06220533, 0.86322398)
l_Twitter <- c(0.05549261, 0.10944140, 0.15447238, 0.68059362)
l_Total   <- c(0.01906878, 0.05788172, 0.06563039, 0.85741911)

## BLOGS
l <- l_Blogs
# No final sample
resBlogs <- modelStats(loadModelBlogs, mT.BlogsFile, finalSample = FALSE,
                       interpol = TRUE, sizeSample = sizeSample,
                       nSamples = nSamples)
resBlogs 

# Final Sample
# resBlogsFS <- modelStats(loadModelBlogs, mT.BlogsFile, finalSample = TRUE,
#                          interpol = TRUE, sizeSample = sizeSample,
#                          nSamples = nSamples)
# resBlogsFS

## NEWS
l <- l_News
# No final sample
resNews <- modelStats(loadModelNews, mT.NewsFile, finalSample = FALSE,
                      interpol = TRUE, sizeSample = sizeSample,
                      nSamples = nSamples)
resNews

# Final Sample
# resNewsFS <- modelStats(loadModelNews, mT.NewsFile, finalSample = TRUE,
#                         interpol = TRUE, sizeSample = sizeSample,
#                         nSamples = nSamples)
# resNewsFS

## TWITTER
l <- l_Twitter
# No final sample
resTwitter <- modelStats(loadModelTwitter, mT.TwitterFile, finalSample = FALSE,
                         interpol = TRUE, sizeSample = sizeSample,
                         nSamples = nSamples)
resTwitter

# Final Sample
# resTwitterFS <- modelStats(loadModelTwitter, mT.TwitterFile, finalSample = TRUE,
#                            interpol = TRUE, sizeSample = sizeSample,
#                            nSamples = nSamples)
# resTwitterFS

## TOTAL
l <- l_Total
# No final sample
resTotal <- modelStats(loadModelTotal, mT.TotalFile, finalSample = FALSE,
                       interpol = TRUE, sizeSample = sizeSample,
                       nSamples = nSamples)
resTotal

# Final Sample
# resTotalFS <- modelStats(loadModelTotal, mT.TotalFile, finalSample = TRUE,
#                          interpol = TRUE, sizeSample = sizeSample,
#                          nSamples = nSamples)
# resTotalFS

## Gather stats
# tbl_res_inter <- 
#   data.frame(Model = c("Blogs", "Blogs", "News", "News", 
#                        "Twitter", "Twitter", "Total", "Total"),
#              Final.Sample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
#              Inf.Conf.Int = c(resBlogs$conf.int[1],   resBlogsFS$conf.int[1],
#                               resNews$conf.int[1],    resNewsFS$conf.int[1],
#                               resTwitter$conf.int[1], resTwitterFS$conf.int[1],
#                               resTotal$conf.int[1],   resTotalFS$conf.int[1]),
#              Mean = c(resBlogs$estimate,   resBlogsFS$estimate,
#                       resNews$estimate,    resNewsFS$estimate,
#                       resTwitter$estimate, resTwitterFS$estimate,
#                       resTotal$estimate,   resTotalFS$estimate), 
#              Sup.Conf.Int = c(resBlogs$conf.int[2],   resBlogsFS$conf.int[2],
#                               resNews$conf.int[2],    resNewsFS$conf.int[2],
#                               resTwitter$conf.int[2], resTwitterFS$conf.int[2],
#                               resTotal$conf.int[2],   resTotalFS$conf.int[2]))

tbl_res_inter <- 
  data.frame(Model = c("Blogs", "Blogs", "News", "News", 
                       "Twitter", "Twitter", "Total", "Total"),
             Final.Sample = c(FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE),
             Inf.Conf.Int = c(resBlogs$conf.int[1],   resNews$conf.int[1],
                              resTwitter$conf.int[1], resTotal$conf.int[1]),
             Mean = c(resBlogs$estimate,   resNews$estimate,   
                      resTwitter$estimate, resTotal$estimate), 
             Sup.Conf.Int = c(resBlogs$conf.int[2],   resNews$conf.int[2],    
                              resTwitter$conf.int[2], resTotal$conf.int[2])

tbl_res_inter

## Some exploration

t.test(tbl_res_inter$Mean)

t.test(tbl_res_inter[tbl_res_inter$Final.Sample == FALSE,]$Mean)

t.test(tbl_res_inter[tbl_res_inter$Final.Sample == TRUE,]$Mean)

plot(tbl_res_inter[tbl_res_inter$Final.Sample == FALSE,]$Mean, type="o", ylim = c(0.08,0.30), col = "blue")
lines(tbl_res_inter[tbl_res_inter$Final.Sample == FALSE,]$Inf.Conf.Int, type="l", col = "blue")
lines(tbl_res_inter[tbl_res_inter$Final.Sample == FALSE,]$Sup.Conf.Int, type="l", col = "blue")
lines(tbl_res_inter[tbl_res_inter$Final.Sample == TRUE,]$Mean, type="o", col = "red")
lines(tbl_res_inter[tbl_res_inter$Final.Sample == TRUE,]$Inf.Conf.Int, type="l", col = "red")
lines(tbl_res_inter[tbl_res_inter$Final.Sample == TRUE,]$Sup.Conf.Int, type="l", col = "red")

date()
