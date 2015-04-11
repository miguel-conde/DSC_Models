#'
#' Function makeTTSets
#' 
#' This function takes a text file and creates two random and disjoint 
#' partitions: a training partition and a testing partition. Both are stored 
#' in two file texts. 
#' 
#' @param inFile  string; text file to partition in a training and a test files
#' @param outFileTraining string; were to put the training file
#' @param outFileTraining string; were to put the testing file
#' @param p       numeric (0-1); % to include in training file
#' 
makeTTSets <- function(inFile, outFileTraining, outFileTesting, p = 0.7) {
  
  conR <- file(inFile, open = "rb", encoding = "UTF-8")
  lines <- readLines(conR, encoding = "UTF-8", skipNul = TRUE)
  close(conR)
  
  nLines <- length(lines)
  nSampledLines <- sample(seq(1, nLines, 1), round(p*nLines))
  
  conW <- file(outFileTraining, open = "w")
  writeLines(lines[nSampledLines], con = conW, useBytes = TRUE)
  close(conW)
  
  conW <- file(outFileTesting, open = "w")
  writeLines(lines[-nSampledLines], con = conW, useBytes = TRUE)
  close(conW)
}

