## TEST
model <- loadModelBlogs()

setkey(model[["m.1G"]], WG1)
setkey(model[["m.2G"]], WG1)
setkey(model[["m.3G"]], WG1,WG2)
setkey(model[["m.4G"]], WG1,WG2,WG3)

words <- c("<stx>", "<stx>", "<stx>")
phrase <- paste(words[1], words[2], words[3])

for (i in 1:1000) {
  w <- predictNG_v2(model, words, 20, finalSample = TRUE, fullRes=FALSE)[[1]]
  words[1] <- words[2]
  words[2] <- words[1]
  words[3] <- w
  phrase <- paste(phrase,w)
  if (w == "<etx>") break
}
phrase