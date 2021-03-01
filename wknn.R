library(foreign)


moda <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

get_weighted <- function(labels,weights) {
  dset = data.frame(labels=factor(labels), 
                    weights=weights)
  scores = aggregate(. ~ labels, dset, sum)
  result = as.character(scores$labels[which.max(scores$weights)])
  return(result)
}

pesosk_proximos <- function(instance, examples, k, classCol){
  x <- examples[,-classCol]
  y <- examples[,classCol]
  dists <- c()
  classe <- c()
  for (i in 1:nrow(x)) {
    dists <- append(dists, dist(rbind(x[i,], instance)))
    classe <- append(classe, y[i])
  }
  if (any(dists==0)) {
    ind <- which(dists==0)
    res <- moda(y[ind])
    return(res)
  }else {
    proximidade <- 1/dists
    df <- data.frame(classe = classe, dists = dists)
    df <- df[order(df$dists),]
    proximos <- as.vector(as.numeric(rownames(df[1:k,])))
    candidates <- y[as.numeric(proximos)]
    pesos <- proximidade[as.numeric(proximos)]
    result <- get_weighted(candidates,pesos)
    return(result)
  }
}


calculatePesoKnnAccuracy <- function(k, test, train, classCol){
  predictions <- apply(test[,-classCol], 1, function(x) pesosk_proximos(x, train, k, classCol))
  testy <- test[,classCol]
  dif <- predictions == testy
  return(sum(dif)/length(predictions))
}