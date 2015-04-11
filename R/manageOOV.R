library(tm)
source("./R/regExpr.R")

#' Functions
#'            manageOOV()
#'            goodOOV()
#'            
#' goodOOV is a function to unify all Out of Vocabulary words in a Corpus             

#' Function manageOOV(x, words)
#' 
#' This funciton scans the character string 'x' and replaces all the tokens 
#' matching a word in 'words' by the string "OOV"
#' 
#' @param x string; characer string to scan 
#' @param words string vector; words to replace in the string 'x'
#' 
#' @return the input strings with the matched words replaced.

manageOOV <- function(x, words) {
  
  s <- x
  for(oovWord in words) {
    print(oovWord)
    #s <- gsub(paste0(" ", oovWord, " "), " <UNK> ", s)
    #s <- gsub(oovWord, "<UNK>", s)
    s <- gsub(paste0("\\<", oovWord, "\\>"), " <UNK> ", s)
  }
  return(s)
}

#' 
#' Function goodOOV()
#' 
#' content_transformer (tm) function to replace all Out of Vocabulary tokens
#' in a Corpus
#' 
#' @return Corpus with OOV words replaced
#' 
#' @include library(tm)
#' 
#' @usage newCorpus <- tm_map(old_corpus, goodOOV, OOV_words)
#' 
goodOOV <- content_transformer(function(x, words) manageOOV(x, words))


#' 
#' Function unifyOOV(corpus, doc_number, OOV_dictionary)
#' 
#' @param corpus VCorpus; corpus to process
#' @param doc_number integer; number of doc in 'corpus' to process
#' @param OOV_dictionary strings vector; dictionary of OOV words to replace in
#' 'corpus'
#' 
#' @return a new corpus with all OOV words replaced

unifyOOV <- function (corpus, doc_number, OOV_dictionary) {
  
  if(doc_number != 4) {
    # Extract from 'corpus' the PlainTextDocument 'doc_number'
    ptd_extract <- 
      PlainTextDocument(corpus[[doc_number]])
    
    # Build a new corpus with just this document
    new_corpus <- VCorpus(VectorSource(ptd_extract))
    meta(new_corpus[[1]], tag = "id") <- meta(corpus[[doc_number]], tag = "id")
    new_corpus[[2]] <- NULL
  }
  else
    new_corpus <- corpus
  
  # Same transformations as usual
  new_corpus   <- tm_map(new_corpus, content_transformer(tolower))
  new_corpus   <- tm_map(new_corpus, stripWhitespace)
  
  # Replace OOV words - JUST 100 OOV WORDS FOR NOW !!!!!
  print(system.time(
    ## new_corpus <- tm_map(new_corpus, goodOOV, OOV_dictionary[10000:10100])
    new_corpus <- tm_map(new_corpus, goodOOV, OOV_dictionary)
  ))
  return(new_corpus)
}
