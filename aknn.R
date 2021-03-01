library(foreign)


moda <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

adaptivek_proximos <- function(instance, train, k, classCol, raios){
  x <- train[,-classCol]
  y <- train[,classCol]
  dists <- c()
  classe <- c()
  for (i in 1:nrow(x)) {
    dists <- append(dists, dist(rbind(x[i,], instance))/raios[i])
    classe <- append(classe, y[i])
  }
  df <- data.frame(classe = classe, dists = dists)
  df <- df[order(df$dists),]
  proximos <- as.vector(as.numeric(rownames(df[1:k,])))
  candidates <- y[as.numeric(proximos)]
  result <- moda(candidates)
  return(result)
}


adaptiveRule <- function(train, colClass){
  radius <- c()
  for (i in 1:nrow(train)) {
    aux <- c()
    for (j in 1:nrow(train)) {
      if(train[i, colClass] != train[j, colClass]){
        distancia <- dist(rbind(train[i, -colClass], train[j, -colClass])) - 0.00000000001
        if(distancia >= 0){
          aux <- append(aux, distancia)
        }
      }
    }
    radius <- append(radius, min(aux))
  }
  return(radius)
}

adaptiveKnnAccuracy <- function(k, test, train, classCol){
  raios <- adaptiveRule(train, classCol)
  predictions <- apply(test[,-classCol], 1, function(x) adaptivek_proximos(x, train, k, classCol, raios))
  testy <- test[,classCol]
  dif <- predictions == testy
  return(sum(dif)/length(predictions))
}