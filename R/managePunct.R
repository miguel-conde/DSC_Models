#' Function managePunct()
#' 
#'  This function preprocesses the text in order to get the punctuation as 
#'  we want.
#'  
#'  @param s text string to process
#'  @return processed text string
#'  @details
#'  See code comments

managePunct <- function(s) {
  
  # Locate emoticons - for now, just managing 10 types of emoticons
  s <- gsub("[=]?:-?\\)+|\\(-?:[=]?", "EMTKN-A", s)
  s <- gsub("[>=]?;-?\\)+|\\(-?;[<=]?", "EMTKN-B", s)
  s <- gsub("[>\\}]?:-?[\\)>]+|[\\(<]-?:[<\\{}]?", "EMTKN-C", s)
  s <- gsub("[=]?[:xX]-?[D]+", "EMTKN-D", s)
  s <- gsub("[=]?:-?\\(+|\\)-?:[=]?", "EMTKN-E", s)
  s <- gsub("[=]?:-?[/\\\\]+|[\\\\/]-?:[=]?", "EMTKN-F", s)
  s <- gsub("[=]?:-?[oO]+|[oO]-?:[=]?", "EMTKN-G", s)
  s <- gsub("[=]?:-?[\\|]+|[\\|]-?:[=]?", "EMTKN-H", s)
  s <- gsub("[=]?:[-\\*]?[xX\\)\\*]+|[xX\\(\\*][-\\*]?:[=]?", "EMTKN-I", s)
  s <- gsub("[=]?:[-]?[@]+|[@][-]?:[=]?", "EMTKN-J", s)
  
  # Substitue numbers by '-unk-' to avoid problems with '-'
  s <- gsub("[[:digit:]]+", "-kkk-", s)
  # Remove all '-' not joining two words
  s <- gsub("([[:alnum:]+])(-+)([[:alnum:]+])", "\\1TGT\\3", s)
  s <- gsub("-", "", s)
  s <- gsub("TGT", "-", s)
  # Remove '.' not joining two words
  s <- gsub("([[:alnum:]+])(\\.)([[:alnum:]+])", "\\1TGT\\3", s)
  s <- gsub("\\.", "", s)
  s <- gsub("TGT", ".", s)
  # Remove ''' not joining two words or not at the end of a word
  s <- gsub("([[:alnum:]+])(')([[:alnum:]+])", "\\1TGT\\3", s)
  s <- gsub("([[:alnum:]+])(')", "\\1TGT", s)
  s <- gsub("'", "", s)
  s <- gsub("TGT", "'", s)
  # Remove '@' not joining two words or not at the beginning of a word
  s <- gsub("([[:alnum:]*])(@)([[:alnum:]+])", "\\1TGT\\3", s)
  s <- gsub("(@)([[:alnum:]+])", "TGT\\2", s)
  s <- gsub("@", "", s)
  s <- gsub("TGT", "@", s)
  # Remove '#' not at the beginning of a word
  s <- gsub("([[:space:]]#)([[:alnum:]+])", "TGT\\2", s)
  s <- gsub("#", " ", s)
  s <- gsub("TGT", " #", s)
  # Remove '_' not joining two words
  s <- gsub("([[:alnum:]+])(_+)([[:alnum:]+])", "\\1TGT\\3", s)
  s <- gsub("_", "", s)
  s <- gsub("TGT", "_", s)
  # &, %, !, <, =, >, ?: leave just one and separated
  s <- gsub("&+", " & ", s)
  s <- gsub("%+", " % ", s)
  s <- gsub("!+", " ! ", s)
  s <- gsub("<+", " < ", s)
  s <- gsub("=+", " = ", s)
  s <- gsub(">+", " > ", s)
  s <- gsub("\\?+", " ? ", s)
  
  # Remove all other punctuation
  regx_punct <- '‘|\\"|\\$|\\(|\\)|\\*|\\+|,|/|:\\;|\\[|\\\\|\\]|\\^|`|\\{|\\||\\}|~|’'
  s <- gsub(regx_punct, "", s)
  
  # Remove non printable characters
  s <- gsub("[^[:print:]]", " ", s)
  
  # Recover emoticons
  s <- gsub("EMTKN-A", " :-) ", s)
  s <- gsub("EMTKN-B", " ;-) ", s)
  s <- gsub("EMTKN-C", " >:-) ", s)
  s <- gsub("EMTKN-D", " :-D ", s)
  s <- gsub("EMTKN-E", " :-( ", s)
  s <- gsub("EMTKN-F", " :-/ ", s)
  s <- gsub("EMTKN-G", " :-o ", s)
  s <- gsub("EMTKN-H", " :-| ", s)
  s <- gsub("EMTKN-I", " :-x ", s)
  s <- gsub("EMTKN-J", " :-@ ", s)
  
  # Clean numbers
  s <- gsub("-*kkk-*", "<num>", s)
  
  # Identify hashtags
  s <- gsub("(^| )+#\\S+", " <hashtag> ", s)
  
  # Identify twitter users
  s <- gsub("(^| )+@\\S+", " <twitter_user> ", s)
  
  # Identify email addresses
  s <- gsub("[[:alnum:]]+@[[:alnum:]]+\\.[[:alnum:]]+", "<email>", s)
  
  s
}