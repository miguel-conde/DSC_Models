library(tm)
library(data.table)

#'
#' MODULE makeTFLs
#' 

#' 
#' FUNCTION makeTFl(tdm, N, numDoc, log = TRUE)
#' 
#' This function gets a Term-Document Matrix and buids a data table:
#'  - Terms are separated in N columns
#'  - Counts are transformed into probabilities or log-probailities
#'  
#'  @param tdm as.matrix(TermDocumentMatrix) ; TDM to be processed
#'  @param N integer; indicates de kind of N-grams contained in the terms of tdm
#'  @param numDoc integer; num column of document in tdm to process
#'  @param Counts logical; if TRUE, use counts; if FALSE, use probs or log-probs
#'  @param log logical; use or not log-probailities when Counts == FALSE
#'  
#'  @return processed tdm as data table
#'  
#'  @include library(tm), library(data.table)
#'  
#'  @usage tfl <- makeTFL(m_tdm = as.matrix(tdm)), N = 3, numDoc = 1, log = FALSE)
#'  

makeTFL <- function(m_tdm, N, numDoc, counts = FALSE, log = FALSE) {
  
  # Easier working with a dataframe - OJO lo cambio por data.table para probar
  df_tdm <- data.table(Wordgram = row.names(m_tdm), Freq = m_tdm[, numDoc])

  # Split N-wordram into single types
  aux <- sapply(as.character(df_tdm$Wordgram), strsplit, split = " ")
  
  # Checks which N-grams are complete
  is.full <- function(l) function(x) {
    
    blank <- TRUE
    length_x <- length(x)
    for (i in 1:length_x)
      blank <- blank & (x[i] != "")
    
    return((length(x) == l) & blank)
  }

  # Build TFL columns only with complete N-grams
  full <- sapply(aux, is.full(N))

  m_cols <- NULL
  
  for (i in 1:N)
    m_cols <- cbind(m_cols, sapply(aux[full], function(x) x[i]))
  m_cols <- cbind(m_cols, df_tdm[full,]$Freq)

  # Build TFL
  unified_levels <- unique(as.vector(m_cols[, 1:N]))
  
  df_NG <- as.data.frame(m_cols, stringsAsFactors = FALSE)

  for(i in 1:N) {
    df_NG[ , i] <- data.frame(factor(df_NG[ , i], levels = unified_levels))
    names(df_NG)[i] <- paste0("WG", i)
  }
  df_NG[, N+1] <- as.numeric(df_NG[, N+1])
  df_NG <- df_NG[df_NG[,N+1] > 0,]
  
  if (counts == TRUE) {
    if (log == FALSE)
      names(df_NG)[N+1] <- "Count"
    else {
      names(df_NG)[N+1] <- "log10Count"
      df_NG[, N+1] <- log10(df_NG[, N+1])
    }
  }
  else {
    sumCol <- sum(df_NG[, N+1])
    if (log == FALSE) {
      names(df_NG)[N+1] <- "Freq"
      df_NG[, N+1] <- df_NG[, N+1] / sumCol
    }
    else {
      names(df_NG)[N+1] <- "log10Freq"
      df_NG[, N+1] <- log10(df_NG[, N+1] / sumCol)
    }
  }
                  
  
  as.data.table(df_NG )
}


### TEST
# m_tdm <- matrix(c(100, 200, 400, 800, 50, 100, 200, 400, 150, 300, 600, 1200),
#                 4, 3)
# rownames(m_tdm) <- c("Fila 1 A", "Fila 2 B", "Fila 3 C", "Fila 4 D")
# 
# makeTFL(m_tdm, N = 3, numDoc = 1, log = FALSE)
# 
# rownames(m_tdm) <- c("Fila 1 A", "Fila B", "Fila 3 C", "Fila 4 D")
# 
# makeTFL(m_tdm, N = 3, numDoc = 1, log = FALSE)

#' 
#' @name tfl2CondProb(tdm)
#' 
#' @description This function transform a Table Frequency List (with terms 
#' counts) into a table with log10 conditional probabilities


tfl2CondProb4G <- function(tfl4G, tfl3G, log = TRUE) {
  
  t_fl4G <- tfl4G
  t_fl3G <- as.data.frame(tfl3G, stringsAsFactors = FALSE)
  
  setkey(t_fl4G, WG1, WG2, WG3)
  #setkey(t_fl3G, WG1, WG2, WG3)
  
  # Iterate trigrams
  n3Grams <- dim(t_fl3G)[1]
  for (ind in 1:n3Grams) {
    trigram <- t_fl3G[ind,]
    
    wg1 <- as.character(trigram$WG1)
    wg2 <- as.character(trigram$WG2)
    wg3 <- as.character(trigram$WG3)
    
    t_fl4G[.(wg1, wg2, wg3), ]$Count <- 
      t_fl4G[.(wg1, wg2, wg3), ]$Count / trigram$Count
  }
  
  # "<stx> <stx> <stx>" is not in the trigram table:
  prob_stxs <- t_fl4G[.("<stx>", "<stx>", "<stx>"), sum(Count)]
  t_fl4G[.("<stx>", "<stx>", "<stx>")]$Count <- 
    t_fl4G[.("<stx>", "<stx>", "<stx>")]$Count / prob_stxs 
  
  if (log == TRUE) {
    t_fl4G$Count <- log10(t_fl4G$Count)
    setnames(t_fl4G, "Count", "log10")
  }
  else
    setnames(t_fl4G, "Count", "Freq")
  
  return(t_fl4G)
}

#'
#' @name makeTFL1G(tfl_4G)
#' 

makeTFL1G <- function(tfl_4G) {
  tfl_1G <- tfl_4G[, sum(Count), by=WG4]
  setkey(tfl_1G, WG4)
  tfl_1G <- tfl_1G[!.("<etx>")]
  setnames(tfl_1G, c("WG4","V1"), c("WG1", "Count"))
  return(tfl_1G)
}

#'
#' @name makeTFL2G(tfl_4G)
#' 

makeTFL2G <- function(tfl_4G) {
  tfl_2G <- tfl_4G[, sum(Count), by=.(WG3, WG4)]
  setkey(tfl_2G, WG3, WG4)
  tfl_2G <- tfl_2G[!.("<etx>","<etx>")]
  setnames(tfl_2G, c("WG3","WG4","V1"), c("WG1", "WG2", "Count"))
  return(tfl_2G)
}

#'
#' @name makeTFL3G(tfl_4G)
#' 

makeTFL3G <- function(tfl_4G) {
  tfl_3G <- tfl_4G[, sum(Count), by=.(WG2, WG3, WG4)]
  setkey(tfl_3G, WG2, WG3, WG4)
  tfl_3G <- tfl_3G[!.("<etx>","<etx>", "<etx>")]
  setnames(tfl_3G, c("WG2", "WG3","WG4","V1"), c("WG1", "WG2", "WG3", "Count"))
  return(tfl_3G)
}

#' 
#' @name addProbs4G(tfl_4G)
#' 

addProbs4G <- function(tfl_4G) {
  
  t4G <- tfl_4G
  setkey(t4G, WG1, WG2, WG3)
  t4G$Freq <- t4G[t4G[,sum(Count), by=key(t4G)]][, Count/V1]
  t4G$logProb <- log2(t4G$Freq)
  
  return(t4G)
}

addProbs3G <- function(tfl_3G) {
  
  t3G <- tfl_3G
  setkey(t3G, WG1, WG2)
  t3G$Freq <- t3G[t3G[,sum(Count), by=key(t3G)]][, Count/V1]
  t3G$logProb <- log2(t3G$Freq)
  
  return(t3G)
}

addProbs2G <- function(tfl_2G) {
  
  t2G <- tfl_2G
  setkey(t2G, WG1)
  t2G$Freq <- t2G[t2G[,sum(Count), by=key(t2G)]][, Count/V1]
  t2G$logProb <- log2(t2G$Freq)
  
  return(t2G)
}

addProbs1G <- function(tfl_1G) {
  
  t1G <- tfl_1G
  setkey(t1G, WG1)
  tot <- t1G[,sum(Count)]
  t1G$Freq <- t1G$Count / tot
  t1G$logProb <- log2(t1G$Freq)
  
  return(t1G)
}

#'
#' @name getTFL3G(tfl_4G) 
#' 

getTFL3G <- function(tfl_4G){
  return(addProbs3G(makeTFL3G(tfl_4G)))
}

getTFL2G <- function(tfl_4G){
  return(addProbs2G(makeTFL2G(tfl_4G)))
}

getTFL1G <- function(tfl_4G){
  return(addProbs1G(makeTFL1G(tfl_4G)))
}


#' 
#' @name changeWords4G(tfl_4G)
#' 

changeWords4G <- function(tfl_4G, dictionary, subst = "<unk>") {
  
  t4G <- tfl_4G
  
  setkey(t4G, WG4)
  t4G[.(dictionary)]$WG4 <- subst
  setkey(t4G, WG3)
  t4G[.(dictionary)]$WG3 <- subst
  setkey(t4G, WG2)
  t4G[.(dictionary)]$WG2 <- subst
  setkey(t4G, WG1)
  t4G[.(dictionary)]$WG1 <- subst
  setkey(t4G, WG1, WG2, WG3, WG4)
  t4G <- t4G[, sum(Count), by=key(t4G)]
  setnames(t4G, "V1", "Count")
  
  return(t4G)
}

changeWords3G <- function(tfl_3G, dictionary, subst = "<unk>") {
  
  t3G <- tfl_3G
  
  setkey(t3G, WG3)
  t3G[.(dictionary)]$WG3 <- subst
  setkey(t3G, WG2)
  t3G[.(dictionary)]$WG2 <- subst
  setkey(t3G, WG1)
  t3G[.(dictionary)]$WG1 <- subst
  setkey(t3G, WG1, WG2, WG3)
  t3G <- t3G[, sum(Count), by=key(t3G)]
  setnames(t4G, "V1", "Count")
  
  return(t3G)
}

changeWords2G <- function(tfl_2G, dictionary, subst = "<unk>") {
  
  t2G <- tfl_2G
  
  setkey(t2G, WG2)
  t2G[.(dictionary)]$WG2 <- subst
  setkey(t2G, WG1)
  t2G[.(dictionary)]$WG1 <- subst
  setkey(t2G, WG1, WG2)
  t2G <- t2G[, sum(Count), by=key(t2G)]
  setnames(t2G, "V1", "Count")
  
  return(t2G)
}

changeWords1G <- function(tfl_1G, dictionary, subst = "<unk>") {
  
  t1G <- tfl_1G
  
  setkey(t1G, WG1)
  t1G[.(dictionary)]$WG1 <- subst
  setkey(t1G, WG1)
  t1G <- t1G[, sum(Count), by=key(t1G)]
  setnames(t1G, "V1", "Count")
  
  return(t1G)
}