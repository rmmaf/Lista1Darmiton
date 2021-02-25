library(foreign)
library(caTools)

moda <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}


divideInstances <- function(df, fraction){
  smp_size <- floor(fraction * nrow(df))
  
  set.seed(123)
  train_ind <- sample(seq_len(nrow(df)), size = smp_size)
  
  train <- df[train_ind, ]
  test <- df[-train_ind, ]
  return(list(train, test))
}

k_proximos <- function(instance, examples, k, classCol){
  x <- examples[,-classCol]
  y <- examples[,classCol]
  dists <- c()
  classe <- c()
  for (i in 1:nrow(x)) {
    dists <- append(dists, dist(rbind(x[i,], instance)))
    classe <- append(classe, y[i])
  }
  df <- data.frame(classe = classe, dists = dists)
  df <- df[order(df$dists),]
  proximos <- as.vector(as.numeric(rownames(df[1:k,])))
  candidates <- y[as.numeric(proximos)]
  result <- moda(candidates)
  return(result)
}

df <- read.arff("arquivos/deffects2.arff")
df[,22] <- gsub("false", FALSE, df[,22])
df[,22] <- gsub("true", TRUE, df[,22])
df[,22] <- as.logical(df[,22])
train <- divideInstances(df, 0.75)[[1]]
test <- divideInstances(df, 0.75)[[2]]



calculateKnnAccuracy <- function(k, test, train, classCol){
  predictions <- apply(test[,-classCol], 1, function(x) k_proximos(x, train, k, classCol))
  testy <- test[,22]
  dif <- predictions == testy
  return(sum(dif)/length(predictions))
}