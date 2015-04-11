
#' Function sampleFiles()
#' 
#' This function samples files in list "inFiles" to a single file. 
#' 
#' @param inFiles string vector; names of files to sample
#' @param outFile string; name of the output sampled file
#' @param p       numeric; % (0 -1) of lines to be sampled in each file
#' @param encoding string; encoding to use reading the files
#' @param open     string; open file mode
#' 
sampleFiles <- function(inFiles, outFile, p, 
                       encoding = "UTF-8", open = "rb", ...){
  
  chunk_size <- 1000
  
  if (file.exists(outFile)) {
    unlink(outFile)
  }
  
  conW <- file(outFile, open = "at")
  
  set.seed(4675)
  
  for (infile in inFiles) {
    conR <- file(infile, open = open, encoding = encoding)
    
    while ((k <- length(lines <- readLines(con = conR, n = chunk_size,
                                           encoding = encoding,
                                           ...))) == chunk_size) {
      
      sampled_lines <- sample(c(1:k), size = round(p*length(lines)))
      
      writeLines(lines[sampled_lines], con = conW, useBytes = TRUE)
      
    }
    
    close(conR)
  }
  
  close(conW)
  
}