#'
#' MODULE perpelexity
#' 

source("./R/managePunct.R")
source("./R/makeTFLs.R")
source("./R/predict.R")

library(tm)
library(RWeka)
library(data.table)

#'
#' @name linePerplex4G(model, line)
#' 
#' @description This function calculates the perplexity of the phrase contained
#' in "line" using the LM "model". "model" MUST be loaded before calling this 
#' function and key must be stablished:
#' @usage model <- loadModelTotal()
#' setkey(model$m.1G, WG1)
#' setkey(model$m.2G, WG1, WG2)
#' setkey(model$m.3G, WG1, WG2, WG3)
#' setkey(model$m.4G, WG1, WG2, WG3, WG4)
#'

linePerplex4G <- function(model, line) {
  
  # Process punctuation
  s <- managePunct(line)
  
  # tolower
  s <- tolower(s)
  
  # stripwhitespaces
  s <- stripWhitespace(s)
  
  # Add seudowords
  s <- paste0("<stx> <stx> <stx> ", grep("^.*$", s, value = TRUE), " <etx> <etx> <etx>")
  
  # Tokenize
  NGrams <- NGramTokenizer(s, Weka_control(min = 4, max = 4,
                                           delimiters = ' \r\n\t"'))
  
  # Split into tokens and reemplace OOV words by "<unk>"
  NGrams <- strsplit(NGrams, " ")
  nNGrams <- length(NGrams)
  
  for(i in 1:nNGrams) {
    for(j in 1:4) {
      if (NGrams[[i]][j] != "<stx>" & NGrams[[i]][j] != "<etx>" & 
            probCond1G(model, NGrams[[i]][j]) == -Inf) {
        #print(NGrams[[i]][j])
        NGrams[[i]][j] <- "<unk>"
        #print(NGrams[[i]][j])
      }
    }
  }
  
  # Calc perplexity
  perplexity <- 0
  
#   for (i in 1: nNGrams) {
#     t <- NGrams[i]
#     
#     if(t[[1]][4] == "<etx>")
#       break
#     
#     lProb <- probCond4G(model, t[[1]][1], t[[1]][2], t[[1]][3], t[[1]][4])
#     
#     #     print(t[[1]])
#     #     print(lProb)
#     
#     perplexity <- perplexity + lProb
#   }
  
  sapply(NGrams, function(x) {
    
    if(x[4] == "<etx>")
      return()
    
    lProb <- probCond4G(model, x[1], x[2], x[3], x[4])
    
    perplexity <<- perplexity + lProb
    
  })
  
  perplexity <- (-1/nNGrams)*perplexity
  perplexity <- 2^perplexity
  
  return(perplexity)
}


probCond4G <- function(model, w1, w2, w3, w4, log = TRUE) {
  
  #setkey(model$m.4G, WG1, WG2, WG3, WG4)
  
  Prob <- model$m.4G[.(w1, w2, w3, w4)]
  if (is.na(Prob$logProb)) {
    Prob <- probCond3G(model, w2, w3, w4, log)
  }
  else {
    if(log == TRUE) 
      Prob <- Prob$logProb
    else
      Prob <- Prob$Freq
  }
  
  return(Prob)
}

probCond3G <- function(model, w1, w2, w3, log = TRUE) {
  
  #setkey(model$m.3G, WG1, WG2, WG3)
  
  Prob <- model$m.3G[.(w1, w2, w3)]
  if (is.na(Prob$logProb)) {
    Prob <- probCond2G(model, w2, w3, log)
  }
  else {
    if(log == TRUE) 
      Prob <- Prob$logProb
    else
      Prob <- Prob$Freq
  }
  
  return(Prob)
}

probCond2G <- function(model, w1, w2, log = TRUE) {
  
  #setkey(model$m.2G, WG1, WG2)
  
  Prob <- model$m.2G[.(w1, w2)]
  if (is.na(Prob$logProb)) {
    Prob <- probCond1G(model, w2, log)
  }
  else {
    if(log == TRUE) 
      Prob <- Prob$logProb
    else
      Prob <- Prob$Freq
  }
  
  return(Prob)
}


probCond1G <- function(model, w1, log = TRUE) {
  
  #setkey(model$m.1G, WG1)
  
  Prob <- model$m.1G[.(w1)]
  
  if(is.na(Prob$logProb)) {
    if(log == TRUE) {
      return(-Inf) 
    }
    else return(0)
  }
  
  if (log == TRUE) {
    return(Prob$logProb)
  }
  else {
    return(Prob$Freq)
  }
}

probPredict4G <- function(model, w1, w2, w3, w4, log = TRUE) {
#   setkey(model$m.1G, WG1)
#   setkey(model$m.2G, WG1, WG2)
#   setkey(model$m.3G, WG1, WG2, WG3)
#   setkey(model$m.4G, WG1, WG2, WG3, WG4)
  
  Prob1G <- model$m.1G[.(w4)]
  if (is.na(Prob1G$logProb))
    Prob1G <- 0
  else
    Prob1G <- Prob1G$Freq
  
  Prob2G <- model$m.2G[.(w3, w4)]
  if (is.na(Prob2G$logProb))
    Prob2G <- 0
  else
    Prob2G <- Prob2G$Freq
  
  Prob3G <- model$m.3G[.(w2, w3, w4)]
  if (is.na(Prob3G$logProb))
    Prob3G <- 0
  else
    Prob3G <- Prob3G$Freq
  
  Prob4G <- model$m.4G[.(w1, w2, w3, w4)]
  if (is.na(Prob4G$logProb))
    Prob4G <- 0
  else
    Prob4G <- Prob4G$Freq
  
  Prob <- 0.12*Prob1G + 0.16*Prob2G + 0.24*Prob3G + 0.48*Prob4G
  
  if(log == TRUE)
    return(log2(Prob))
  else
    return(Prob)
}

linePerplex4G_v2 <- function(model, line) {
  
#   setkey(model$m.1G, WG1)
#   setkey(model$m.2G, WG1, WG2)
#   setkey(model$m.3G, WG1, WG2, WG3)
#   setkey(model$m.4G, WG1, WG2, WG3, WG4)
  
  # Process punctuation
  s <- managePunct(line)
  
  # tolower
  s <- tolower(s)
  
  # stripwhitespaces
  s <- stripWhitespace(s)
  
  # Add seudowords
  s <- paste0("<stx> <stx> <stx> ", grep("^.*$", s, value = TRUE), " <etx> <etx> <etx>")
  
  # Tokenize
  NGrams <- NGramTokenizer(s, Weka_control(min = 4, max = 4,
                                           delimiters = ' \r\n\t"'))
  
  # Split into tokens and reemplace OOV words by "<unk>"
  NGrams <- strsplit(NGrams, " ")
  nNGrams <- length(NGrams)
  
  for(i in 1:nNGrams) {
    for(j in 1:4) {
      if (NGrams[[i]][j] != "<stx>" & NGrams[[i]][j] != "<etx>" & 
            probCond1G(model, NGrams[[i]][j]) == -Inf) {
        NGrams[[i]][j] <- "<unk>"
      }
    }
  }
  
  # Calc perplexity
  perplexity <- 0
  
  sapply(NGrams, function(x) {
    
    if(x[4] == "<etx>")
      return()
    
    lProb <- probPredict4G(model, x[1], x[2], x[3], x[4])
    
    perplexity <<- perplexity + lProb
    
  })
  
  perplexity <- (-1/nNGrams)*perplexity
  perplexity <- 2^perplexity
  
  return(perplexity)
}

## To finally calc lambdas ###################################################


linePerplex4G_v3 <- function(model, line, l = c(0.25, 0.25, 0.25, 0.25)) {
  
  # Get 4-Grams
  NGrams <- get4Grams(model, line)
  
  # Get probs table
  NGProbs <- getProbs <- function(model, NGrams)
  
  # Calc perplexity
  return(getPerplexity(model, NGProbs, l))
  
}


get4Grams <- function(line, model) {
  # Process punctuation
  s <- managePunct(line)
  
  # tolower
  s <- tolower(s)
  
  # stripwhitespaces
  s <- stripWhitespace(s)
  
  # Add seudowords
  s <- paste0("<stx> <stx> <stx> ", grep("^.*$", s, value = TRUE), " <etx> <etx> <etx>")
  
  # Tokenize
  NGrams <- NGramTokenizer(s, Weka_control(min = 4, max = 4,
                                           delimiters = ' \r\n\t"'))
  
  # Split into tokens and reemplace OOV words by "<unk>"
  NGrams <- strsplit(NGrams, " ")
  NGrams <- as.data.table(t(as.matrix(as.data.frame(NGrams))))
  setkey(NGrams, V1, V2, V3, V4)
  NGrams[V1!="<stx>" & V1!="<etx>" & 
           is.na(model[["m.1G"]][V1]$Count)]$V1 <- "<unk>"
  NGrams[V2!="<stx>" & V2!="<etx>" & 
           is.na(model[["m.1G"]][V2]$Count)]$V2 <- "<unk>"
  NGrams[V3!="<stx>" & V3!="<etx>" & 
           is.na(model[["m.1G"]][V3]$Count)]$V3 <- "<unk>"
  NGrams[V4!="<stx>" & V4!="<etx>" & 
           is.na(model[["m.1G"]][V4]$Count)]$V4 <- "<unk>"
  
#   NGrams <-
#     sapply(NGrams, function(x) {
#       existsNo1Gram <- sapply(x, function(y) probCond1G(trainModel, y) == -Inf)
#       x[x != "<stx>" & x != "<etx>" & existsNo1Gram] <- "<unk>"
#       x
#   })

  #NGrams <- NGrams[1:(dim(NGrams)[1]-3),]
  return(NGrams[V4!="<etx>"])
  
  #return(NGrams)
}

getProbs <- function(model, NGrams) {

  NGrams$P1 <- model[["m.1G"]][NGrams[,V4]]$Freq
  NGrams[is.na(NGrams$P1)]$P1 <- 0

  setkey(model[["m.2G"]], WG1, WG2)
  NGrams$P2 <- model[["m.2G"]][.(NGrams$V3, NGrams$V4)]$Freq
  NGrams[is.na(NGrams$P2)]$P2 <- 0
  
  setkey(model[["m.3G"]], WG1, WG2, WG3)
  NGrams$P3 <- model[["m.3G"]][.(NGrams$V2, NGrams$V3, NGrams$V4)]$Freq
  NGrams[is.na(NGrams$P3)]$P3 <- 0
  
  setkey(model[["m.4G"]], WG1, WG2, WG3, WG4)
  NGrams$P4 <- model[["m.4G"]][.(NGrams$V1, NGrams$V2, NGrams$V3, NGrams$V4)]$Freq
  NGrams[is.na(NGrams$P4)]$P4 <- 0
  
  
  return(NGrams)
}

getPerplexity <- function(model, NGProbs, 
                          l = c(0.25, 0.25, 0.25, 0.25), log = FALSE) {
  
  nNGrams <- dim(NGProbs)[2]
  
  # Calc perplexity
  perplexity <- sum(log2(as.matrix(NGProbs[,.(P1,P2,P3,P4)]) %*%  l))
 
  perplexity <- (-1/nNGrams)*perplexity
  
  if (log == FALSE)
    perplexity <- 2^perplexity
  
  return(perplexity)
}

## Not necessary now:
probPredict4G_v2 <- function(model, w, log = TRUE) {
  
  Prob <- c(model$m.1G[.(w[4])]$Freq,
            model$m.2G[.(w[3], w[4])]$Freq,
            model$m.3G[.(w[2], w[3], w[4])]$Freq,
            model$m.4G[.(w[1], w[2], w[3], w[4])]$Freq)
  Prob[is.na(Prob)] <- 0
  
  if(log == TRUE)
    return(log2(Prob))
  else
    return(Prob)
}