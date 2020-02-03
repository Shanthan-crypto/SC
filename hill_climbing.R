library(igraph)
node_heuristic<-c(240,186,182,163,170,150,165,139,120,130,122,104,100,77,72,65,65,0)
names(node_heuristic)<-c('A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R')

graph_1<-graph(edges=c('A','B','A','C','A','D','A','E','B','K','C','I','D','N','E','J','K','H','I','L','I','M','I','F','N','F','N','J','J','Q','J','G','H','L','L','P','M','O','Q','R','Q','G','P','R','P','O','O','R'),directed=F)
E(graph_1)$weight<-c(73,64,89,104,83,64,89,40,35,28,20,31,84,53,80,35,36,63,50,65,113,65,41,72)

start_node<-'A'
goal<-'R'
path<-c(start_node)
n<-start_node
flag<-FALSE
while(TRUE){
  if(n==goal){
    flag<-TRUE
    break
  }
  adj<-adjacent_vertices(graph_1,n)
  min_cost_child<-adj[[1]][1]$name
  for(i in 2:length(adj[[1]])){
    if(node_heuristic[min_cost_child]>node_heuristic[adj[[1]][i]$name]){
      min_cost_child<-adj[[1]][i]$name
    }
  }
  if(node_heuristic[n]<node_heuristic[min_cost_child]){
    flag<-FALSE
    break
  }
  n<-min_cost_child
  path<-c(path,n)
}
if(flag){
  print(path)
} else{
  print("stopped at local maxima")
  print(path)
}

ordered_list<-order(as.numeric(node_heuristic))
node_heuristic<-node_heuristic[ordered_list]
print(node_heuristic)
typeof(node_heuristic)

#print(node_heuristic)
#node_heuristic<-node_heuristic[names(node_heuristic)!='A']
#print(node_heuristic)
