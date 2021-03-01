library(foreign)

moda <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
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


calculateKnnAccuracy <- function(k, test, train, classCol){
  predictions <- apply(test[,-classCol], 1, function(x) k_proximos(x, train, k, classCol))
  testy <- test[,classCol]
  dif <- predictions == testy
  return(sum(dif)/length(predictions))
}