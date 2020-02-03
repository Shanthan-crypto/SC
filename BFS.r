library(igraph)
library(stringr)
bfs<-function(start,goal){
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
      print(open)
      break
    }
    closed[j]<-open[i]
    j<-j+1
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
    open<-open[-1]
    open<-append(open,neighbor)
    print(open)
    
  }
  return(closed)
}
adjList<-list(c('Timisoara','Sibiu','Zerind'),
              c('Arad', 'Odarea'),
              c('Zerind','Sibiu'),
              c('Lugoj', 'Arad'),
              c('Mehadia', 'Timisoara'),
              c('Dobreta', 'Lugoj'),
              c('Craiovea', 'Mehadia'),
              c('Pitesti','Rimnieu', 'Dobreta'),
              c('Craiovea','Bucharest','Rimnieu'),
              c('Craiovea','Pitesti', 'Sibiu'),
              c('Rimnieu','Farara','Odarea', 'Arad'),
              c('Bucharest', 'Sibiu'),
              c('Gurgui','Urziceni', 'Farara','Pitesti'),
              c('Bucharest','Hirsova'),
              c('Urziceni','Eforie', 'Vaslui'),
              'Hirsova',
              c('Hirsova','Iasi'),
              c('Vaslui', 'Neamt'),
              'Iasi',
              'Bucharest')
names(adjList)<-c('Arad',
                  'Zerind',
                  'Odarea',
                  'Timisoara',
                  'Lugoj',
                  'Mehadia',
                  'Dobreta',
                  'Craiovea',
                  'Pitesti',
                  'Rimnieu',
                  'Sibiu',
                  'Farara',
                  'Bucharest',
                  'Urziceni',
                  'Hirsova',
                  'Eforie',
                  'Vaslui',
                  'Iasi',
                  'Neamt',
                  'Gurgui')
graph<-stack(adjList)
#plot(g,directed=FALSE)
visited<-bfs('Arad','Bucharest')
print(visited)
temp<-as.data.frame(graph)
x<-paste(temp$values,temp$ind,sep=",")
temp$y = unname(sapply(x, function(x) {
  paste(sort(str_trim(strsplit(x[1], ',')[[1]])), collapse=',')} ))
temp<-temp[!duplicated(temp$y), ]
temp<-temp[,-3]
temp<-graph.data.frame(temp,directed=FALSE)
V(temp)$color<-"pink"
for(i in 1:length(visited)){
  V(temp)[visited[i]]$color<-"green"
}
V(temp)["Arad"]$color<-"red"
V(temp)["Bucharest"]$color<-"red"
plot(temp)
