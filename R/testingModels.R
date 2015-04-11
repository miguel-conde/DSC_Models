#'
#' MODULE testingModels.R
#' 
#' 

source("./R/dfnames.R")
source("./R/predict.R")


testModels <- function(trainModel, testModel, 
                       sizeSample = 1000, nSamples = 100,
                       finalSample = FALSE,
                       interpol = FALSE) {
  v_acc <- NULL
  
  for (iter in 1:nSamples) {
    print(sprintf("Sample %d", iter))
    # Sample sizeSample 4G tokens from testModel
    idxTestSample <- sample(dim(testModel)[1], sizeSample)
    testSample <- testModel[idxTestSample]
    
    # Predict on the sample
        prediction <- apply(testSample, 1,
                            function(x) {
                              predictNG_v2(trainModel,x[1:3],
                                           fullRes = FALSE, 
                                           finalSample = finalSample,
                                           interpol = interpol)[[1]]
                              })
    
    ###########
#         testSample$WG1 <- as.character(testSample$WG1)
#         testSample$WG2 <- as.character(testSample$WG2)
#         testSample$WG3 <- as.character(testSample$WG3)
#         testSample$WG4 <- as.character(testSample$WG4)
#      
#          setkey(testSample, WG1, WG2, WG3)
    # 
    #     prediction <-
    #       testSample[, funPredict(trainModel, WG1, WG2, WG3,
    #                                 fullRes = FALSE,
    #                                 finalSample = finalSample,
    #                                 interpol = interpol)[[1]], 
    #                  by = 1:nrow(testSample)]$V1
    #############
    
    realResults <- testSample$WG4
    
    hits <- sum((prediction == realResults)*testSample$Count)
    
    accuracy <- hits / sum(testSample$Count)
    v_acc <- append(v_acc, accuracy)
  }
  t <- t.test(v_acc)
  
  return(t)
}

testModels_v2 <- function(trainModel, testModel, 
                          sizeSample = 1000, nSamples = 100,
                          finalSample = FALSE,
                          interpol = FALSE) {
  v_acc <- NULL
  
  for (iter in 1:nSamples) {
    print(sprintf("Sample %d", iter))
    # Sample sizeSample 4G tokens from testModel
    idxTestSample <- sample(dim(testModel)[1], sizeSample)
    testSample <- testModel[idxTestSample]
    
    ##########
    testSample$WG1 <- as.character(testSample$WG1)
    testSample$WG2 <- as.character(testSample$WG2)
    testSample$WG3 <- as.character(testSample$WG3)
    testSample$WG4 <- as.character(testSample$WG4)
    
    setkey(testSample, WG1, WG2, WG3)
    
    prediction <-
      testSample[, predictNG_v2(trainModel, c(WG1, WG2, WG3),
                                fullRes = FALSE, finalSample = finalSample,
                                interpol = interpol)[[1]], 
                 by = 1:nrow(testSample)]$V1
    ############
    
    realResults <- testSample$WG4
    
    hits <- sum((prediction == realResults)*testSample$Count)
    
    accuracy <- hits / sum(testSample$Count)
    v_acc <- append(v_acc, accuracy)
  }
  t <- t.test(v_acc)
  
  return(t)
}

funPredict <- function(trainModel, w1, w2, w3, fullRes, finalSample, interpol) {

  predictNG_v2(trainModel, c(w1, w2, w3),
                             fullRes = FALSE,
                             finalSample = finalSample,
                             interpol = interpol)[[1]]

}

modelStats <- function(loadModelFun, inTrainFile, finalSample = FALSE,
                       interpol = FALSE, sizeSample = 100,
                       nSamples = 10) {
  
  # Load train model
  trainModel    <- loadModelFun()
  # Load test 4-gram TFL
  testModelName <- load(inTrainFile)
  
  # Get accuracy stats 
  res <- testModels (trainModel, eval(parse(text = testModelName)), 
                     sizeSample = sizeSample, nSamples = nSamples,
                     finalSample = finalSample, interpol = interpol) 
  res
}