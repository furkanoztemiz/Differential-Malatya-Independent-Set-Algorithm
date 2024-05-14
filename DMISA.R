
library(igraph)
library(threejs)
library(visNetwork)


g <- graph(edges <- c(1,2,2,3,1,4,1,5,2,5,2,6,3,6,3,7,4,5,5,6,6,7,4,8,4,9,5,9,5,10,6,10,6,11,7,11,7,12,8,9,9,10,10,11,11,12,8,13,9,13,9,14,10,14,10,15,11,15,11,16,12,16,13,14,14,15,15,16,13,17,14,17,14,18,15,18,15,19,16,19,17,18,18,19),directed = FALSE)


##### Calculate DMCA
vector = c()
DifferantialMalatyaCentrality <- function(g){
  vertexList <- c(V(g))
  for (i in vertexList) { 
    Vdegree <-degree(g,v = V(g)[i])
    KomsuDegree <- degree(g,v = neighbors(g,v = V(g)[i]))
    Value <- (Vdegree-KomsuDegree)/KomsuDegree
    vector <- c(vector, sum(Value))
  }
  return(vector)
}
######## Find Minimum Vertex
FindMinimum <- function(graph){
  data <- data.frame(DifferantialMalatyaCentrality(graph))
  return (order(data$DifferantialMalatyaCentrality.graph.,decreasing = FALSE)[1])
}

################### Maksimum Independent Set Vertex Selection ###########

IndependentSet = c()
while(vcount(g) > 0 ){
  silinecekNodes =c()
  komsu <- NULL 
  IndependentSet <- append(IndependentSet,V(g)[FindMinimum(g)])
  minNode <- V(g)[FindMinimum(g)]
  komsu <- neighbors(g,v = minNode)
  silinecekNodes <- append(silinecekNodes,minNode)
  silinecekNodes <- append(silinecekNodes,komsu)
  g <- delete_vertices(g,silinecekNodes )
}
length(IndependentSet)



############  RANDOM GRAPH GENERATORS   #############
g<- erdos.renyi.game(30000,1/10)
graph.density(g)

write.table(data.frame(as_edgelist(g)), file = Big50000Vertex.txt,row.names=TRUE, na=,col.names=TRUE, sep=,)


g <- watts.strogatz.game(1, 500, 1, 0.35, loops = FALSE, multiple = FALSE)
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = Small world model)


g <- barabasi.game(200, power = 0.2, m = NULL, out.dist = NULL, out.seq = NULL, out.pref = FALSE, zero.appeal = 1, directed = FALSE, algorithm =psumtree, start.graph = NULL)
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = Scale-free network model)


g <- static.power.law.game(500, 500, exponent.out= 2.2, exponent.in = -1, loops = FALSE, multiple = FALSE, finite.size.correction = TRUE) 
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = Scale-free network model (static))


g<- sample_bipartite(n1 = 100,n2 = 50,p = 1)

