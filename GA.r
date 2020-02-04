rangeL <- 0
rangeU <- 6
n = 10

fitness <- function(a,b)
{
  f <- (a^2 + b - 11)^2 + (a + b^2 - 7)^2
  return( 1/(1+f) )
}

getfitness <- function(i){
  x = as.list( strsplit(i," "))
  x1 <- getXi(BinToDec(x[[1]][1]) )
  x2 <- getXi(BinToDec(x[[1]][2]) )
  Fx <- fitness(x1,x2)
  return(Fx)
}

BinToDec <- function(x) 
  sum(2^(which(rev(unlist(strsplit(as.character(x), "")) == 1))-1))

getXi <- function(a){
  result <- rangeL + ( ((rangeU-rangeL)/((2^n)-1)) * a )
}

findchromo <-function(a,b){
  result <- c()
  for(i in b){
    result <- c(result,which(a>i)[1])
  }
  return (result)
}

Pc = 0.8
Pm = 0.05

Selection <- function(population,random_val){
  sumFx <- 0
  selection <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(selection) <- c("X1","X2","x1","x2","Fx")
  for(i in population){
    row <- c()
    x = as.list( strsplit(i," "))
    row <- c(row ,x[[1]][1])
    row <- c(row ,x[[1]][2])
    x1 <- getXi(BinToDec(x[[1]][1]) )
    x2 <- getXi(BinToDec(x[[1]][2]) )
    Fx <- fitness(x1,x2)
    sumFx <- sumFx + Fx
    row <- c(row, x1 ,x2 ,Fx)
    selection <- rbind(selection,t(as.data.frame(row)) )
  }
  colnames(selection) <- c("X1","X2","x1","x2","Fx")
  row.names(selection) <- c(1:nrow(selection))
  cm <- 0
  Picol <- c()
  CPicol <- c()
  for(i in selection$Fx){
    cm <- cm + as.numeric(i)/sumFx 
    Picol <- c(Picol,as.numeric(i)/sumFx)
    CPicol <- c(CPicol,cm)
  }
  rand <- random_val[1:nrow(selection)]
  strnew <- findchromo(CPicol,rand)
  selection <- cbind(selection,Picol,CPicol,rand, strnew)
  return(selection)
}

CrossOver <-function(pop,random_val){
  co <- data.frame(matrix(ncol = 0, nrow = length(pop)))
  co <- cbind(co,pop)
  co_site = c(9,12,5)
  randco = c(random_val[7:9])
  rval <- c()
  Co_Y_N <- c()
  Co_s <- c()
  c_overed <- c()
  for(p in seq(1,length(pop),2) ){
    rval <- c(rval,randco[(p+1)/2],randco[(p+1)/2])
    if(randco[(p+1)/2] < Pc)
    {
      Co_Y_N <- c(Co_Y_N, 1,1)
      c <- co_site[(p+1)/2]
      val <- NULL
      val1 <-NULL
      if(co_site[(p+1)/2] < n){
        val <- paste( c(substr(pop[p],1,c) , substr(pop[p+1],c+1,21 ) ),collapse = "")
        val1 <- paste( c(substr(pop[p+1],1,c) , substr(pop[p],c+1,21) ),collapse = "")
      }
      else{
        val <- paste( c(substr(pop[p],1,c+1) , substr(pop[p+1],c+2,21) ),collapse = "")
        val1 <- paste( c(substr(pop[p+1],1,c+1) , substr(pop[p],c+2,21) ),collapse = "")
      }
      c_overed <- c(c_overed,val,val1)
    }
    else{
      Co_Y_N <- c(Co_Y_N, 0,0)
      c_overed <- c(c_overed,pop[p],pop[p+1])
    }
    Co_s <- c(Co_s,co_site[(p+1)/2],co_site[(p+1)/2])
  }
  co <- cbind(co,rval,Co_Y_N,Co_s,c_overed)
  return(co)
}

Mutation <- function(pop,random_val){
  mu <- data.frame(matrix(ncol = 0, nrow = length(pop)))
  mu <- cbind(mu,pop)
  mu_site = 9
  randmu = c(random_val[10:length(random_val)])
  rval <- c()
  mu_Y_N <- c()
  mu_s <- rep(mu_site,6)
  mutated <- c()
  for(i in 1:length(pop) ){
    chromosome <- pop[i]
    newchrom <- as.character(pop[i])
    if(randmu[i] < Pm){
      mu_Y_N <- c(mu_Y_N ,1)
      b <- 0
      if(mu_s[i] > n){
        b <- 1
      }
      char <- as.character(pop[i])
      if( substring(char,mu_s[i]+b,mu_s[i]+b ) == "0" ){
        substring(char,mu_s[i]+b,mu_s[i]+b ) <- "1"
      }
      else{
        substring(char,mu_s[i]+b,mu_s[i]+b ) <- "0"
      }
      newchrom<- char
    }
    else{
      mu_Y_N <- c(mu_Y_N ,0)
    }
    mutated <- c(mutated,newchrom)
  }
  mu <- cbind(mu,mu_Y_N,mu_s,mutated)
  return(mu)
}

random_val = c(0.472,0.108,0.723,0.536,0.931,0.972,0.717,0.363,0.817,0.189,0.607,0.192,0.386,0.001,0.413)
population = c('1100010000 1110010000','0011100111 0001001101','0111001000 1010100001','1000010100 1001000110','1011100011 1100011000', '0011111000 0011100101')
inpop <- population
avgf<- c()

for( it in 1:5){
  selected <- Selection(inpop,random_val)
  pop <- as.character( inpop[selected$strnew] )
  cross_overed = CrossOver(pop,random_val)
  pop <- as.character(cross_overed$c_overed)
  mutated <- Mutation(pop,random_val)
  Totalpop = c(inpop,as.vector(as.character(cross_overed$c_overed )),as.vector(as.character(mutated$mutated)) )
  Totalpop <- unique(Totalpop)
  newpop <- c()
  
  for(i in Totalpop){
    newpop <- c(newpop,getfitness(i))
  }
  
  fit_table <- data.frame(Totalpop,newpop)
  fit_table <- fit_table[order(-newpop),]
  inpop <- as.character( t(fit_table[1:6,1]) )
  x <- mean(fit_table$newpop)
  if(length(avgf) > 0 &&  x == avgf[length(avgf)]){
    #break
  }
  
  avgf <- c(avgf,mean(fit_table$newpop))
  print(selected)
  print(cross_overed)
  print(mutated)
  #print(fit_table)
  print(avgf)
}
