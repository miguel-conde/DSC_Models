
cleanRegExpr <- function(s) {
  
  s <- gsub("\\‘", "\\\\\\‘", s)
  s <- gsub("\\!", "\\\\\\!", s)
  s <- gsub('\\"', '\\\\\\"', s)
  s <- gsub("\\#", "\\\\\\#", s)
  s <- gsub("\\$", "\\\\\\$", s)
  s <- gsub("\\%", "\\\\\\%", s)
  s <- gsub("\\&", "\\\\\\&", s)
  s <- gsub("\\'", "\\\\\\'", s)
  s <- gsub("\\(", "\\\\\\(", s)
  s <- gsub("\\)", "\\\\\\)", s)
  s <- gsub("\\*", "\\\\\\*", s)
  s <- gsub("\\+", "\\\\\\+", s)
  s <- gsub('\\,', '\\\\\\,', s)
  s <- gsub("\\-", "\\\\\\-", s)
  s <- gsub("\\.", "\\\\\\.", s)
  s <- gsub("\\/", "\\\\\\/", s)
  s <- gsub("\\:", "\\\\\\:", s)
  s <- gsub('\\;', "\\\\\\;", s)
  s <- gsub("\\\\<", "\\\\\\<", s)
  s <- gsub("\\=", "\\\\\\=", s)
  s <- gsub("\\\\>", "\\\\\\>", s)
  s <- gsub("\\?", "\\\\\\?", s)
  s <- gsub('\\@', "\\\\\\@", s)
  
  s
}

# > s2 <- gsub("(?=ald#ldf)|(?<=ald#ldf)", "<UNK>", s, perl = T)
# > s2
# [1] "t-shirt we've haven't <UNK>ald#ldf<UNK>"
# > gsub("[\\<].*[\\>].*[\\<].*[\\>]", "",s2)
# [1] "t-shirt we've haven't "

#' 
#' Function OOV_dictionary
#' 
#' @param m_tdm numeric matrix; term document matrix
#' @param ncol numeric; column of 'm_tdm' to use for discriminating words to
#' return
#' @param cutoff numeric; number of counts under which a word is included in
#' the OOV list
#' 
#' @return string vector with all the type considered OOV words
#' 
OOV_dictionary <- function(m_tdm, ncol, cutOff) {
  OOV_list <- c(names(m_tdm[m_tdm[, ncol] < cutOff & m_tdm[, ncol] > 0, ncol]))
  #OOV_list <- cleanRegExpr(OOV_list)
  return(OOV_list)
}
