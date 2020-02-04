library(igraph)
#node_heuristic<-c(240,186,182,163,170,150,165,139,120,130,122,104,100,77,72,65,65,0)
#names(node_heuristic)<-c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R')

#graph_1<-graph(edges=c('A','B','A','C','A','D','A','E','B','K','C','I','D','N','E','J','K','H','I','L','I','M','I','F','N','F','N','J','J','Q','J','G','H','L','L','P','M','O','Q','R','Q','G','P','R','P','O','O','R'),directed=F)
#E(graph_1)$weight<-c(73,64,89,104,83,64,89,40,35,28,20,31,84,53,80,35,36,63,50,65,113,65,41,72)

node_heuristic<-c(223,222,166,192,165,136,122,111,100,60,32,102,0)
names(node_heuristic)<-c('A','B','C','D','E','F','G','H','I','J','K','L','M')

graph_1<-graph(edges = c('A','B','A','C','B','D','C','D','C','F','C','L','D','E','F','J','F','K','L','M','E','G','J','I','J','K','K','M','G','H','I','H'),directed = F)

start_node<-'A'
goal<-'M'

V(graph_1)$color<-"white"
E(graph_1)$color<-"black"

open<-c()
open[start_node]<-node_heuristic[start_node]
closed<-c()
path<-c()
flag<-FALSE
while(TRUE){
  if(length(open)==0){
    break
  }
  n<-names(open)[1]
  if(n==goal){
    path<-c(path,n)
    flag<-TRUE
    break
  }
  closed<-c(closed,open[1])
  open<-open[-1]
  adj<-adjacent_vertices(graph_1,n)
  for(i in 1:length(adj[[1]])){
    if(adj[[1]][i]$name %in% names(open) || adj[[1]][i]$name %in% names(closed)){
      next
    }
    open[adj[[1]][i]$name]<-node_heuristic[adj[[1]][i]$name]
  }
  ordered_open<-order(as.numeric(open))
  open<-open[ordered_open]
  print(closed)
  path<-c(path,n)
}

plot(graph_1)


if(flag){
  print(path)
}else{
  print("path not found")
}