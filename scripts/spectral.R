library("class")
library("mclust")
library("dendextend")
library("rARPACK")
library("igraph")


Mnn <- function(X, M){
  # calculate the distance beetween two points and save it as a matrix
  distOutput <- as.matrix(dist(X), method = "euclidean")
  
  # order the matrix 
  orderedOutput <- apply(distOutput, 2, order)
  # in first column is the same column value (1 - 1) so we want to delete it
  orderedOutput <- orderedOutput[-1, ]
  
  # choose only this rows, which are the closest
  # t function to transpose result
  S <- t(orderedOutput[1:M, ])
  
}

Mnn_graph <- function(S){
  # convert into adjacency matrix
  G <- matrix(0, nrow = nrow(S), ncol = nrow(S))
  
  for(row in 1:nrow(S)) {
    for(col in 1:ncol(S)) {
      G[row, S[row, col]] <- 1
      G[S[row, col], row] <- 1
    }
  }
  
  # creating a graph from a adjacency matrix
  ourGraph <- graph_from_adjacency_matrix(G, mode = c("undirected"), weighted = NULL, diag = FALSE)
  
  # calculating number of graph component
  comp <- components(ourGraph)
  componentsGroups <- groups(comp)
  componentsNumber <- length(componentsGroups)
  
  # if the number of component is bigger than 1 we add some edges
  while (componentsNumber != 1) {
    G[componentsGroups[[componentsNumber]][1], componentsGroups[[componentsNumber-1]][1]] <- 1
    G[componentsGroups[[componentsNumber-1]][1], componentsGroups[[componentsNumber]][1]] <- 1
    componentsNumber <- componentsNumber - 1
  }
  
  G
}


Laplacian_eigen <- function(G, k){
  stopifnot(k > 1)
  
  # creating graph to calculate a degree of a vertex (optional solution: sum of a row or a column)
  ourGraph <- graph_from_adjacency_matrix(G, mode = c("undirected"), weighted = NULL, diag = FALSE)
  
  # first solution
  # calculating a degree of a vertex
  #vertexDegree <- degree(ourGraph)
  
  # using diag function create D matrix
  #D = diag(vertexDegree, nrow(G), ncol(G))
  #L = D - G
  
  # second solution
  L <- laplacian_matrix(ourGraph)
  
  #stopifnot(isSymmetric(L))
  #eigenStructure <- eigen(L, symmetric = TRUE) # <- too slow
  
  # SA - the smallest(leftmost) values
  eigenStructure <- eigs_sym(L, 10 * k, which = "SA")
  
  #vectorNumbers <- k + 1
  
  E <-  eigenStructure$vectors[, (ncol(eigenStructure$vectors) - k + 1):ncol(eigenStructure$vectors)]
  
  # alternatives (when we use a eigen function):
  #E <- eigenStructure$vectors[, order(eigenStructure$values, decreasing = FALSE)[1:vectorNumbers]]
  
  E
}


spectral_clustering <- function(X, M, k){
  S <- Mnn(X, M)
  G <- Mnn_graph(S)
  E <- Laplacian_eigen(G, k)
  kmeans(E, k)$cluster
}

