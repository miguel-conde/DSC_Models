source("./R./perplex.R")

## TEST perplex.R
phrase <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
m.Total <- loadModelTotal()
linePerplex4G(m.Total, phrase)
model <- m.Total

setkey(model[["m.1G"]], WG1)
setkey(model[["m.2G"]], WG1,WG2)
setkey(model[["m.3G"]], WG1,WG2,WG3)
setkey(model[["m.4G"]], WG1,WG2,WG3,WG4)

## Modelo 7.5 y linePerplex4G #################################################

# 1 - 60.31584, 49.0124, 38.48577, 50.47436 - "beer" - OK
phrase <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
words <- c("a", "case", "of") 
predictNG_v2(model, words, 20, finalSample = FALSE, fullRes=TRUE)
phrase1 <- paste(phrase, "pretzels")
phrase2 <- paste(phrase, "cheese")
phrase3 <- paste(phrase, "beer")
phrase4 <- paste(phrase, "soda")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 2 - 10.21766, 12.13378, 10.39667, 8.298285 - "world" - OK
phrase <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
phrase1 <- paste(phrase, "most")
phrase2 <- paste(phrase, "universe")
phrase3 <- paste(phrase, "best")
phrase4 <- paste(phrase, "world")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 3 - 29.35205, 17.62008, 30.84187, 15.32524 - "happiest" - OK
phrase <- "Hey sunshine, can you follow me and make me the"
phrase1 <- paste(phrase, "saddest")
phrase2 <- paste(phrase, "smelliest")
phrase3 <- paste(phrase, "bluest")
phrase4 <- paste(phrase, "happiest")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 4 - 225.6184, 284.7237, 225.6184, 225.6184 1 - Triple empate ("crowd" no es)
phrase <- "Very early observations on the Bills game: Offense still struggling but the"
phrase1 <- paste(phrase, "defense")
phrase2 <- paste(phrase, "referees")
phrase3 <- paste(phrase, "crowd")
phrase4 <- paste(phrase, "players")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 5 - 27.77509, 28.26171, 29.46738, 28.00574 - "beach" - OK
phrase <- "Go on a romantic date at the"
phrase1 <- paste(phrase, "beach")
phrase2 <- paste(phrase, "grocery")
phrase3 <- paste(phrase, "movies")
phrase4 <- paste(phrase, "mall")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 6 - 99.51709, 87.71118, 115.2828, 79.99355 - "way" - OK
phrase <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
phrase1 <- paste(phrase, "horse")
phrase2 <- paste(phrase, "phone")
phrase3 <- paste(phrase, "motorcycle")
phrase4 <- paste(phrase, "way")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 7 - 58.64278, 63.59536, 63.59536, 41.0598 - "time" - OK
phrase <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
phrase1 <- paste(phrase, "years")
phrase2 <- paste(phrase, "thing")
phrase3 <- paste(phrase, "weeks")
phrase4 <- paste(phrase, "time")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 8 - 185.2319, 203.9612, 185.2319, 210.3179 - Doble empate (es "fingers")
phrase <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
phrase1 <- paste(phrase, "fingers")
phrase2 <- paste(phrase, "ears")
phrase3 <- paste(phrase, "eyes")
phrase4 <- paste(phrase, "toes")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 9 - 31.6314, 28.69299, 31.43738, 29.2224 - "bad" - OK
phrase <- "Be grateful for the good times and keep the faith during the"
phrase1 <- paste(phrase, "worse")
phrase2 <- paste(phrase, "bad")
phrase3 <- paste(phrase, "sad")
phrase4 <- paste(phrase, "hard")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

# 10 - 28.65881, 34.71156, 36.15611, 33.89345 - "insane" - OK
phrase <- "If this isn't the cutest thing you've ever seen, then you must be"
phrase1 <- paste(phrase, "insane")
phrase2 <- paste(phrase, "insensitive")
phrase3 <- paste(phrase, "callous")
phrase4 <- paste(phrase, "asleep")

linePerplex4G(m.Total, phrase1)
linePerplex4G(m.Total, phrase2)
linePerplex4G(m.Total, phrase3)
linePerplex4G(m.Total, phrase4)

## RESULTADOS Quiz 2 con este modelo 7.5 y linePerplex4G  -> 8/10 + 2 empates

## Modelo 7.5 y linePerplex4G_v2 #############################################

# 1 - 144.6735, 114.4135, 86.34005, 118.7917 - "beer" - OK
phrase <- "The guy in front of me just bought a pound of bacon, a bouquet, and a case of"
phrase1 <- paste(phrase, "pretzels")
phrase2 <- paste(phrase, "cheese")
phrase3 <- paste(phrase, "beer")
phrase4 <- paste(phrase, "soda")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 2 - 19.85894, 24.36715, 20.87688, 15.765 - "world" - OK
phrase <- "You're the reason why I smile everyday. Can you follow me please? It would mean the"
phrase1 <- paste(phrase, "most")
phrase2 <- paste(phrase, "universe")
phrase3 <- paste(phrase, "best")
phrase4 <- paste(phrase, "world")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 3 - 64.80326, 34.57974, 68.14535, 31.36014 - "happiest" - OK
phrase <- "Hey sunshine, can you follow me and make me the"
phrase1 <- paste(phrase, "saddest")
phrase2 <- paste(phrase, "smelliest")
phrase3 <- paste(phrase, "bluest")
phrase4 <- paste(phrase, "happiest")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 4 - 786.6452, 1037.274, 776.6192, 782.2831 - "crowd" - NOK (no sé cuál es) 
phrase <- "Very early observations on the Bills game: Offense still struggling but the"
phrase1 <- paste(phrase, "defense")
phrase2 <- paste(phrase, "referees")
phrase3 <- paste(phrase, "crowd")
phrase4 <- paste(phrase, "players")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 5 - 55.95437, 57.49169, 59.55724 - "beach" - OK
phrase <- "Go on a romantic date at the"
phrase1 <- paste(phrase, "beach")
phrase2 <- paste(phrase, "grocery")
phrase3 <- paste(phrase, "movies")
phrase4 <- paste(phrase, "mall")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 6 - 294.6921, 259.0168, 344.7238, 229.9926 - "way" - OK
phrase <- "Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my"
phrase1 <- paste(phrase, "horse")
phrase2 <- paste(phrase, "phone")
phrase3 <- paste(phrase, "motorcycle")
phrase4 <- paste(phrase, "way")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 7 - 134.6474, 142.4256, 145.3527, 89.14574 - "time" - OK
phrase <- "Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some"
phrase1 <- paste(phrase, "years")
phrase2 <- paste(phrase, "thing")
phrase3 <- paste(phrase, "weeks")
phrase4 <- paste(phrase, "time")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 8 - 598.929, 671.4138, 591.6251, 692.3394 - "eyes" - NOK (es "fingers") 
phrase <- "After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little"
phrase1 <- paste(phrase, "fingers")
phrase2 <- paste(phrase, "ears")
phrase3 <- paste(phrase, "eyes")
phrase4 <- paste(phrase, "toes")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 9 - 69.37197, 62.85617, 68.42299, 63.66826 - "bad" - OK
phrase <- "Be grateful for the good times and keep the faith during the"
phrase1 <- paste(phrase, "worse")
phrase2 <- paste(phrase, "bad")
phrase3 <- paste(phrase, "sad")
phrase4 <- paste(phrase, "hard")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

# 10 - 71.18635, 88.49169, 92.12842, 85.13936 - "insane" - OK
phrase <- "If this isn't the cutest thing you've ever seen, then you must be"
phrase1 <- paste(phrase, "insane")
phrase2 <- paste(phrase, "insensitive")
phrase3 <- paste(phrase, "callous")
phrase4 <- paste(phrase, "asleep")

linePerplex4G_v2(m.Total, phrase1)
linePerplex4G_v2(m.Total, phrase2)
linePerplex4G_v2(m.Total, phrase3)
linePerplex4G_v2(m.Total, phrase4)

## RESULTADOS Quiz 2 con este modelo 7.5 y linePerplex4G_v2  -> 8/10