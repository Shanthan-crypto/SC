#install.packages("igraph")
library("igraph")
graph_1 <- graph( edges=c('A','S','A','T','A','Z','S','F','S','R','S','O','T','L','Z','O','F','B','R','P','R','C','L','M','B','G','B','U','P','B','C','P','M','D','U','H','D','C','H','E','H','V','V','I','I','N'), directed=F)


goal <- V(graph_1)['S']
start <- V(graph_1)['N']
get_adj_vertices <- function(graph, node,goal,max_depth,current_depth){
  
  if(goal==node)
    return(1)
  
  if(current_depth==max_depth)
  {
    
    return(0)
  }
  
  adjacent_vertices_list = adjacent_vertices(graph,node)
  tmp <- get('visited',.GlobalEnv)
  tmp[[length(visited)+1]] <- node
  assign("visited",tmp, .GlobalEnv)
  
  for(i in seq(1,length(adjacent_vertices_list[[1]]),by=1)){
    if(goal == adjacent_vertices_list[[1]][i]){
      return(1)
    }
    if(!(adjacent_vertices_list[[1]][i] %in% visited)){
      
      goal_reach_flag = get_adj_vertices(graph,adjacent_vertices_list[[1]][i],goal,max_depth,current_depth +1)
      if(goal_reach_flag == 1)
      {
        return(1)
      }
    }
  }
  
  return(0)
}



i=0
while(length(visited)<=gorder(graph_1)){
  #for(i in 0:3){
  visited <- list()
  goal_flag =get_adj_vertices(graph_1,start,goal,i,0)
  i=i+1
  if(goal_flag == 1)
  {
    break
  }
  
  visited[[length(visited)+1]] <- goal
  V(graph_1)[unlist(visited)]$color="pink"
  V(graph_1)[start]$color="green"
  V(graph_1)[goal]$color="yellow"
  plot(graph_1)
  Sys.sleep(2)
}
#dfs(graph_1,V(graph_1)['N'])
