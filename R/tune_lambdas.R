#'
#' MODULE tuneLambdas.R
#' 

# This table simulates the robabilities of the 4-grams of a 
# length N string

N <- 1000
probs <- rexp(N*4)

table_probs <- as.data.frame(matrix(probs, 4, N/4))

# This function simulates the calculation of perplexity of a of length N 
# string s 

simu_pp_s  <- function(l1, l2, l3, l4) {
  
  log_prob <- 0
  
  sapply(table_probs, function(x) {

    new_prob <- log2(l1*x[1]+ l2*x[2]+ l3*x[3] + l4*x[4])
    
    if(is.nan(new_prob)) return(-1)

    log_prob <<- log_prob + new_prob
  })

  return(2^(-(1/(N/4))*log_prob))
}

fun <- function(l) {
  sum_l <- sum(l)
  #sum_l <- 1
  #simu_pp_s(l1, l2,l3,l4) + abs(10000 * (sum(w) - 1))
  simu_pp_s(l[1]/sum_l, l[2]/sum_l,l[3]/sum_l,l[4]/sum_l)
}

simu_pp_s(0.25,0.25,0.25,0.25)

# This function applies descent gradiant algorithm to minimize perplexity

tune_lambda <- function(l1 = 0.25, l2 = 0.25, l3 = 0.25, l4 = 0.25, 
                        alpha = 0.01, max_iter = 1000, 
                        definition = 0.0001) {
  
  l1_i <- l1; l2_i <- l2; l3_i <- l3; l4_i <- l4; 
  
  list_aux <- NULL
  
  for(i in 1:max_iter) {
    
    pp1 <- fun(c(l1_i, l2_i, l3_i, l4_i))
    
    l1_i <- l1_i - alpha*grad1(l1_i, l2_i, l3_i, l4_i, alpha)
    l2_i <- l2_i - alpha*grad2(l1_i, l2_i, l3_i, l4_i, alpha)
    l3_i <- l3_i - alpha*grad3(l1_i, l2_i, l3_i, l4_i, alpha)
    l4_i <- l4_i - alpha*grad4(l1_i, l2_i, l3_i, l4_i, alpha)

    pp2 <- fun(c(l1_i, l2_i, l3_i, l4_i))
    
    list_aux <- append(list_aux, pp2-pp1)
    
    if (abs(pp2-pp1) < definition) break
  }
  
  return(list(list_aux, c(l1_i,l2_i,l3_i,l4_i)))
}

grad1 <- function(l1_i, l2_i, l3_i, l4_i, alpha) {
  num <- 
    fun(c(l1_i + alpha, l2_i, l3_i, l4_i)) - fun(c(l1_i, l2_i, l3_i, l4_i))
  return(num/alpha)
}

grad2 <- function(l1_i, l2_i, l3_i, l4_i, alpha) {
  num <- 
    fun(c(l1_i, l2_i + alpha, l3_i, l4_i)) - fun(c(l1_i, l2_i, l3_i, l4_i))
  return(num/alpha)
}

grad3 <- function(l1_i, l2_i, l3_i, l4_i, alpha) {
  num <- 
    fun(c(l1_i, l2_i, l3_i + alpha, l4_i)) - fun(c(l1_i, l2_i, l3_i, l4_i))
  return(num/alpha)
}

grad4 <- function(l1_i, l2_i, l3_i, l4_i, alpha) {
  num <- 
    fun(c(l1_i, l2_i, l3_i, l4_i + alpha)) - fun(c(l1_i, l2_i, l3_i, l4_i))
  return(num/alpha)
}

res <- tune_lambda()

tune_lambda2 <- function(l1 = 0.25, l2 = 0.25, l3 = 0.25, l4 = 0.25, 
                        alpha = 0.01, max_iter = 1000, 
                        definition = 0.0001) {
  
  l1_i <- l1; l2_i <- l2; l3_i <- l3; l4_i <- l4; 
  
  list_aux <- NULL
  
  for(i in 1:max_iter) {
    pp1 <- simu_pp_s(l1_i, l2_i, l3_i, l4_i)
    l1_i <- l1_i + alpha*grad2(l1_i, l2_i, l3_i, l4_i, alpha)
    l2_i <- l2_i + alpha*grad3(l1_i, l2_i, l3_i, l4_i, alpha)
    l3_i <- l3_i + alpha*grad4(l1_i, l2_i, l3_i, l4_i, alpha)
    #l4_i <- l4_i + alpha*grad1(l1_i, l2_i, l3_i, l4_i, alpha)
    l4_i <- 1 - l3_i - l2_i - l1_i
    pp2 <- simu_pp_s(l1_i, l2_i, l3_i, l4_i)
    
    #print(c(l1_i,l2_i,l3_i,l4_i))
    list_aux <- append(list_aux, pp2-pp1)
    if (abs(pp2-pp1) < definition) break
  }
  return(list(list_aux, c(l1_i,l2_i,l3_i,l4_i)))
}

res <- tune_lambda2(alpha = 0.001) # este funciona


############
test <- function(l1=0.25,l2=0.25,l3=0.25,l4=0.25,
                 alpha = 0.01, max_iter = 1000, 
                 definition = 0.0001) {
  
  res <- tune_lambda(l1,l2,l3,l4, alpha, max_iter, definition)
  
  l <- res[[2]]/sum(res[[2]])
  
  return(list(Evolution = res[[1]], 
              lambdas = l, sum_lambdas = sum(l), 
              simu_pp_s = simu_pp_s(l[1],l[2],l[3],l[4])))
}

res1  <- test(0.25,0.25,0.25,0.25, alpha = 0.001, definition = 0.0000001)
res2  <- test(   1,   0,   0,   0, alpha = 0.001, definition = 0.0000001)
res3  <- test(   0,   1,   0,   0, alpha = 0.001, definition = 0.0000001)
res4  <- test(   0,   0,   1,   0, alpha = 0.001, definition = 0.0000001)
res5  <- test(   0,   0,   0,   1, alpha = 0.001, definition = 0.0000001)
res6  <- test( 0.5, 0.5, 0,0, 0.0, alpha = 0.001, definition = 0.0000001)
res7  <- test( 0.5,   0, 0.5,   0, alpha = 0.001, definition = 0.0000001)
res8  <- test( 0.5,   0,   0, 0.5, alpha = 0.001, definition = 0.0000001)
res9  <- test( 0,   0.5, 0.5,   0, alpha = 0.001, definition = 0.0000001)
res10 <- test( 0,   0.5,   0, 0.5, alpha = 0.001, definition = 0.0000001)
res11 <- test( 0,     0, 0.5, 0.5, alpha = 0.001, definition = 0.0000001)

RES <- rbind(append(res1$lambdas,  res1$simu_pp_s), 
             append(res2$lambdas,  res2$simu_pp_s),
             append(res3$lambdas,  res3$simu_pp_s),
             append(res4$lambdas,  res4$simu_pp_s),
             append(res5$lambdas,  res5$simu_pp_s),
             append(res6$lambdas,  res6$simu_pp_s),
             append(res7$lambdas,  res7$simu_pp_s),
             append(res8$lambdas,  res8$simu_pp_s),
             append(res9$lambdas,  res9$simu_pp_s),
             append(res10$lambdas, res10$simu_pp_s),
             append(res11$lambdas, res11$simu_pp_s))

RES <- as.data.frame(RES)
names(RES) <- c("l1", "l2", "l3", "l4", "simu_pp")

RES


library(DEoptim)

DE <- DEoptim(fn = fun, lower = rep(0, 4), upper = rep(1, 4), 
              control = DEoptim.control())

summary(DE)

best_val <- DE$optim$bestval
best_val
best_par <- DE$optim$bestmem
best_par
best_lambdas <- best_par/sum(best_par)
best_lambdas

fun(best_lambdas)

## Play with strategies
DEoptim.control(strategy = 3)
DE2 <- DEoptim(fn = fun, lower = rep(0, 4), upper = rep(1, 4), 
              control = DEoptim.control())

summary(DE2)

best_val2 <- DE2$optim$bestval
best_val2
best_par2 <- DE2$optim$bestmem
best_par2
best_lambdas2 <- best_par2/sum(best_par2)
best_lambdas2

fun(best_lambdas2)

## Final tuning #############################################################
source("./R/perplex.R")
library(DEoptim)

sampleTotal <- function(nLines) {

  numLines <- ceiling(nLines/3)
  
  linesBlogs   <- sampleLines(blogsSampleTrainFile_1G, numLines)
  linesNews    <- sampleLines(newsSampleTrainFile_1G, numLines)
  linesTwitter <- sampleLines(twitterSampleTrainFile_1G, numLines)
  
  paste0(linesBlogs, linesNews, linesTwitter)
}

sampleLines <- function(inFile, nLines) {
  if(inFile == "TOTAL") {
    lines <- sampleTotal(nLines)
  }
  else {
    fileLines <- readLines(inFile, encoding = "UTF-8", skipNul = TRUE)
    idxLines <- sample(length(fileLines), nLines)
    lines <- NULL
    sapply(fileLines[idxLines], function(x) {
      lines <<- paste0(lines, x)
    })
  }
  lines
}

fun2 <- function(l, model, NGProbs) {
  
  sum_l <- sum(l)
  
  getPerplexity(model, NGProbs, l/sum_l, log = TRUE)
}

getLambdas <- function(loadModelFun, sampleFile, nLinesSample) {
  trainModel <- loadModelFun()
  
  setkey(trainModel$m.1G, WG1)
  setkey(trainModel$m.2G, WG1, WG2)
  setkey(trainModel$m.3G, WG1, WG2, WG3)
  setkey(trainModel$m.4G, WG1, WG2, WG3, WG4)
  
  lines <- sampleLines(sampleFile, nLinesSample)
  N4Grams <- get4Grams(lines, trainModel)
  N4Probs <- getProbs(trainModel, N4Grams)
  
  DEoptim.control(trace = 10)
  DE <- DEoptim(fn = fun2, lower = rep(0, 4), upper = rep(1, 4), 
                     control = DEoptim.control(),
                     model = trainModel, NGProbs = N4Probs)
  
  best_lambdas <- DE$optim$bestmem/sum(DE$optim$bestmem)
  best_lambdas
}

nLines <- 120

set.seed(1234)

## BLOGS
trainModel <- loadModelBlogs()

setkey(trainModel$m.1G, WG1)
setkey(trainModel$m.2G, WG1, WG2)
setkey(trainModel$m.3G, WG1, WG2, WG3)
setkey(trainModel$m.4G, WG1, WG2, WG3, WG4)

lines <- sampleLines(blogsSampleTrainFile_1G, nLines)
N4Grams <- get4Grams(lines,trainModel)
N4Probs <- getProbs(trainModel, N4Grams)

DEBlogs <- DEoptim(fn = fun2, lower = rep(0, 4), upper = rep(1, 4), 
                   control = DEoptim.control(),
                   model = trainModel, NGProbs = N4Probs)

summary(DEBlogs)

best_lambdas_Blogs <- DEBlogs$optim$bestmem/sum(DEBlogs$optim$bestmem)
best_lambdas_Blogs

## MEJOR:
init_date <- date()
nLines <- 1200

set.seed(1234)

## BLOGS
best_lambdas_Blogs <- getLambdas(loadModelBlogs, blogsSampleTrainFile_1G, 
                                 nLines)
best_lambdas_Blogs

## NEWS
best_lambdas_News <- getLambdas(loadModelNews, newsSampleTrainFile_1G, 
                                nLines)
best_lambdas_News

## TWITTER
best_lambdas_Twitter <- getLambdas(loadModelTwitter, twitterSampleTrainFile_1G, 
                                   nLines)
best_lambdas_Twitter

## TOTAL
best_lambdas_Total <- getLambdas(loadModelTotal, "TOTAL", nLines)
best_lambdas_Total

res_table <- data.frame(Par     = c("lambda1", "lambda2", "lambda3", "lambda4"),
                        Blogs   = best_lambdas_Blogs,
                        News    = best_lambdas_News,
                        Twitter = best_lambdas_Twitter,
                        Total   = best_lambdas_Total)
res_table
end_date <- date()
print(sprintf("From %s to %s", init_date, end_date))
