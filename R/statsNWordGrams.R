library(tm)

#'
#' Function statsNWordGrams()
#' 
#' This function calculates N-WordGrams statistics from Term-Document Matrixes
#' 
#' @param tdm
#' @param inFile string; the Corpus file whose data must be used to calculate statistics
#' If inFile = "TOTAL" all Corpus statistics are calculated
#' @param count numeric; count number beyond which a term is considered 
#' Frequent Term
#' @param seq numeric vector; vector of probabilities with values to calculate
#' quantiles
#' @param freq logical; if TRUE, use each term count; if FALSE, use the % of
#' counts over the total document length (tokens)
#' @return
#' 
#' @details
#' 
statsNWordGrams <- function(tdm, inFile = "TOTAL", 
                            count = 50, s = seq(0.5,1,0.01), freq = TRUE)
{
  m_tdm <- as.matrix(tdm)
  
  if (inFile == "TOTAL") 
    m_tdm <- cbind(m_tdm, TOTAL = apply(m_tdm,1,sum))
  else 
    m_tdm <- m_tdm[m_tdm[ , inFile] > 0, ]
  
  tokens  <- sum(m_tdm[,inFile])
  types   <- dim(m_tdm)[1]
  
  if (inFile == "TOTAL")
    freqTerms <- findFreqTerms(tdm, count)
  else
    freqTerms <- findFreqTerms(tdm[, inFile], count)
  
  coverageFreqTerms <- tail(cumsum(m_tdm[freqTerms, inFile]) / tokens, 1)
  percentFreqTerms <- sum(m_tdm[ , inFile] >= count) / sum(m_tdm[ , inFile] > 0)
  
  tokensFT  <- sum(m_tdm[freqTerms, inFile])
  typesFT   <- length(m_tdm[freqTerms, inFile])
  
  # Density is the count of each term over the total length (number of tokens)
  # of the document
  if (freq == FALSE)
    # See http://stackoverflow.com/questions/20596433/how-to-divide-each-row-of-a-matrix-by-elements-of-a-vector-in-r
    m_tdm <- t(t(m_tdm) / apply(m_tdm,2,sum))
  
  q <- quantile(m_tdm[ , inFile], probs = s)
  
  mnFreq    <- mean(m_tdm[ , inFile])
  varFreq   <- var(m_tdm[ , inFile])
  sdFreq    <- sd(m_tdm[ , inFile])
  seFreq    <- sdFreq/sqrt(types)
  mnFreqFT  <- mean(m_tdm[freqTerms, inFile])
  varFreqFT <- var(m_tdm[freqTerms, inFile])
  sdFreqFT  <- sd(m_tdm[freqTerms, inFile])
  seFreqFT  <- sdFreqFT/sqrt(typesFT)
  
  results <- paste0("\n", 
                    paste(toupper(inFile),"term counts statistics:\n\n"),
                    "Deciles:\n")
  
  writeLines(results)
  
  print(q)
  
  if (freq == TRUE)
    results <- paste0("\n",
                      "All Terms (frequencies):\n",
                      sprintf("  Number of Types:              %d\n", types),
                      sprintf("  Number of Tokens:             %d\n", tokens),
                      sprintf("  Tokens / Types:               %02.2f\n", tokens/types),
                      sprintf("  Frequency Mean:               %02.2f\n", mnFreq),
                      sprintf("  Frequency Variance:           %02.2f\n", varFreq),
                      sprintf("  Frequency Standard Deviation: %02.2f\n", sdFreq),
                      sprintf("  Frequency Standard Error:     %02.2f\n\n", seFreq),
                      sprintf("Frequent Terms (count >= %d):\n", count),
                      sprintf("  Number of Types:              %d\n", typesFT),
                      sprintf("  Number of Tokens:             %d\n", tokensFT),
                      sprintf("  Tokens / Types:               %02.2f\n", tokensFT/typesFT),
                      sprintf("  Frequency Mean:               %02.2f\n", mnFreqFT),
                      sprintf("  Frequency Variance:           %02.2f\n", varFreqFT),
                      sprintf("  Frequency Standard Deviation: %02.2f\n", sdFreqFT),
                      sprintf("  Frequency Standard Error:     %02.2f\n\n", seFreqFT),
                      sprintf("  Frequent Terms Percentage:    %02.2f %%\n", 
                              100*percentFreqTerms),
                      sprintf("  Frequent Terms Coverage:      %02.2f %%\n\n", 
                              100*coverageFreqTerms),
                      "--------------------\n\n")
  else
    results <- paste0("\n",
                      "All Terms (densities):\n",
                      sprintf("  Number of Types:              %d\n", types),
                      sprintf("  Number of Tokens:             %d\n", tokens),
                      sprintf("  Tokens / Types:               %02.2f\n", tokens/types),
                      sprintf("  Frequency Mean:               %e\n", mnFreq),
                      sprintf("  Frequency Variance:           %e\n", varFreq),
                      sprintf("  Frequency Standard Deviation: %e\n", sdFreq),
                      sprintf("  Frequency Standard Error:     %e\n\n", seFreq),
                      sprintf("Frequent Terms (count >= %d):\n", count),
                      sprintf("  Number of Types:              %d\n", typesFT),
                      sprintf("  Number of Tokens:             %d\n", tokensFT),
                      sprintf("  Tokens / Types:               %02.2f\n", tokensFT/typesFT),
                      sprintf("  Frequency Mean:               %e\n", mnFreqFT),
                      sprintf("  Frequency Variance:           %e\n", varFreqFT),
                      sprintf("  Frequency Standard Deviation: %e\n", sdFreqFT),
                      sprintf("  Frequency Standard Error:     %e\n\n", seFreqFT),
                      sprintf("  Frequent Terms Percentage:    %02.2f %%\n", 
                              100*percentFreqTerms),
                      sprintf("  Frequent Terms Coverage:      %02.2f %%\n\n", 
                              100*coverageFreqTerms),
                      "--------------------\n\n")
  
  writeLines(results)
}

statsCorpus <- function(tdm, v_count = c(50,50,50,50)) {
  
  for (doc in tdm$dimnames$Docs) {
    i <- which(train_tdm_1Ga$dimnames$Docs == doc)
    statsNWordGrams(tdm, doc, count = v_count[i])
  }
  # CORPUS
  statsNWordGrams(tdm, count = v_count[4])
}


#' 
#' @name calcCoverage(tdm, inFile = "TOTAL", tgtCoverage = 0.95)
#' 
#' @param tdm tm Term Document Matrix to process
#' @param inFile integer (1-4) or string; which document process in TDM
#' @param tgtCoverage numeric (0-1); target % of coverage to achieve
#' 
#' @return a list with 4 elements:
#'  - nTokens: list with:
#'                        - nTokens: total number of tokens in doc
#'                        - nVTokens: number of tokens in doc Vocabulary
#'                        - nOOVTokens: number of tokens out of doc Vocabulary
#'  - nTerms: list with:
#'                        - nTerms: total number of terms in doc
#'                        - nVTerms: number of Vocabulary terms in doc
#'                        - nOOVTerms: number of Out Of Vocabulary terms in doc
#'  - listTerms: list with:
#'                        - VTerms: vector of Vocabulary Terms
#'                        - OOVTerms: vector of OOV terms
#'  - percents: list with:
#'                        - percentFreqTerms: % of vocabulary terms over total
#'                        terms
#'                        - percentFreqTermsCoverage: % coverage of total tokens
#'                        by vocabulary most frequent terms
#' 
#' @description This function takes document 'inFile' in TDM 'tdm' and 
#' determines the most frquecy terms terms needed to cover % 'tgtCoverage' of 
#' tokens in doc.
#' 
calcCoverage <- function(tdm, inFile = "TOTAL", tgtCoverage = 0.95) {
  
  # Better work with a matrix
  m_tdm <- as.matrix(tdm)
  
  # Select doc in TDM; if "TOTAL", we need to calculate it
  if (inFile == "TOTAL" | inFile == 4) 
    m_tdm <- cbind(m_tdm, TOTAL = apply(m_tdm,1,sum))
  else 
    m_tdm <- m_tdm[m_tdm[ , inFile] > 0, ]
  
  # Order terms by decreasin frequency
  v_tdm <- m_tdm[order(m_tdm[, inFile], decreasing = TRUE), inFile]
  
  # We'll calculate the cutoff (minimum most frequent terms needed to cover 
  # %'tgtCoverage' of total tokens) adapting binary search algorithm
  
  sup <- length(v_tdm)
  inf <- 1
  
  tgt <- tgtCoverage * sum(v_tdm)
  print(tgt)
  
  while (inf <= sup) {
    
    center <- floor((sup + inf) / 2)
    writeLines(sprintf("inf: %d center: % d - sup: %d", inf, center, sup))    
    sumCount <- sum(v_tdm[1:center])
    
    if (sumCount > tgt & sumCount - v_tdm[center-1] > tgt) {
      sup <- center - 1
    }
    else {
      inf <- center + 1
    }
  }
  
  print(sprintf("sumcum: %d - percent: %f", 
                sum(v_tdm[1:center]), sum(v_tdm[1:center])/sum(v_tdm)))
  
  # Once the cutoff is decided, just calculate results to return
  
  nTotalTokens = sum(v_tdm)
  nVTokens = sum(v_tdm[1:center])
  nOOVTokens = nTotalTokens - nVTokens
  nTokens <- list(nTotalTokens = nTotalTokens, nVTokens = nVTokens, 
                  nOOVTokens = nOOVTokens) 
  
  nTotalTerms = length(v_tdm)
  nVTerms = center
  nOOVTerms = nTotalTerms - center 
  nTerms <- list(nTotalTerms = nTotalTerms, nVTerms = nVTerms, 
                 nOOVTerms = nOOVTerms)
  
  VTerms = names(v_tdm[1:center])
  OOVTerms = names(v_tdm[(center+1):nTotalTerms])
  listTerms <- list(VTerms = VTerms, OOVTerms = OOVTerms)
  
  percentFreqTermsCoverage = nVTokens / nTotalTokens
  percentFreqTerms <- nVTerms / nTotalTerms
  percents <- list (percentFreqTerms = percentFreqTerms, 
                    percentFreqTermsCoverage = percentFreqTermsCoverage)
  
  res <- list(nTokens = nTokens, nTerms = nTerms, listTerms = listTerms, 
              percents = percents)
    
  return(res)
}