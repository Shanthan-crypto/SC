library(igraph)
library(stringr)
adjList<-list(c('A', 'B'),
              c('C', 'D'),
              c('E', 'F'),
              'A',
              'A',
              c('H', 'G'),
              'B',
              'E',
              'E')
names(adjList)<-c('S',
                  'A',
                  'B',
                  'C',
                  'D',
                  'E',
                  'F',
                  'H',
                  'G')
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
k1<-dfs('S','G')
print(k1)
for(i in 1:length(k1))
{
  V(temp)[k1[i]]$color<-"green"
}
V(temp)["S"]$color<-"red"
V(temp)["G"]$color<-"red"
plot(temp)
