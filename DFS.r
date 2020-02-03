library(igraph)
library(stringr)
adjList<-list(c('Zerind','Timisoara','Sibiu'),c('Odarea','Arad'),c('Zerind','Sibiu'),c('Arad','Lugoj'),c('Timisoara','Mehadia'),c('Lugoj','Dobreta'),c('Mehadia','Craiovea'),c('Dobreta','Pitesti','Rimnieu'),c('Craiovea','Rimnieu','Bucharest'),c('Sibiu','Craiovea','Pitesti'),c('Arad','Odarea','Rimnieu','Farara'),c('Sibiu','Bucharest'),c('Farara','Pitesti','Urziceni','Gurgui'),c('Bucharest','Hirsova'),c('Urziceni','Vaslui','Eforie'),'Hirsova',c('Hirsova','Iasi'),c('Neamt','Vaslui'),'Iasi','Bucharest')
names(adjList)<-c('Arad','Zerind','Odarea','Timisoara','Lugoj','Mehadia','Dobreta','Craiovea','Pitesti','Rimnieu','Sibiu','Farara','Bucharest','Urziceni','Hirsova','Eforie','Vaslui','Iasi','Neamt','Gurgui')
graph<-stack(adjList)
#plot(g,directed=FALSE)
temp<-as.data.frame(graph)
x<-paste(temp$values,temp$ind,sep=",")
temp$y = unname(sapply(x, function(x) {
  paste(sort(str_trim(strsplit(x[1], ',')[[1]])), collapse=',')} ))
temp<-temp[!duplicated(temp$y), ]
print(temp)
temp<-temp[,-3]
print(temp)
dfs<-function(start,goal){
  open<-c()
  closed<-c()
  i<-1
  j<-1
  open[1]<-start
  while(!is.null(open[1])){
    n<-open[1]
    print(n)
    if(n==goal){
      closed[j]<-n
      open=open[-1]
      break
    }
    closed[j]<-open[i]
    V(temp)[open[i]]$color<-"red"
    j<-j+1
    count<-0
    neighbor=c()
    k<-1
    len<-length(adjList[open[i]][[1]])
    for(val in 1:len){
      neighbor[k]<-adjList[open[i]][[1]][val]
      k<-k+1
    }
    cat("Neighbors of  ",open[i],"are ",neighbor,"\n")
    neighbor<-neighbor[!neighbor %in% open]
    neighbor<-neighbor[!neighbor %in% closed]
    open<-append(neighbor,open)
    open<-open[-(length(neighbor)+1)]
  }
  return(closed)
}
temp<-graph.data.frame(temp,directed=FALSE)
k1<-dfs('Arad','Bucharest')
print(k1)
for(i in 1:length(k1))
{
  V(temp)[k1[i]]$color<-"green"
}
V(temp)["Arad"]$color<-"red"
V(temp)["Bucharest"]$color<-"red"
plot(temp)