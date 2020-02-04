fitness<-function(chromosome,range,chromosome_length){
  value<-strtoi(chromosome,base = 2)
  value<-range[1]+((range[2]-range[1])/(2**chromosome_length-1))*value
  return(value**2)
}
length_chromosome<-6
no_of_chromosome<-8
no_of_variable<-1
range<-c(0,20)
#random chromosome generation
X<-rbinom(no_of_chromosome*length_chromosome,1,0.5)
X<-as.character(X[1:length_of_random_number])
length_of_random_number<-no_of_chromosome*length_chromosome #length of x
chromosomes<-c()
i<-1
index<-1
while(i<no_of_chromosome*length_chromosome){
  from<-i
  to<-i+length_chromosome-1
  chromosomes[index]<-paste(X[from:to],collapse = '')
  i<-i+length_chromosome
  index<-index+1
}
co_prob<-0.65
mu_prob<-0.05
no_of_iteration<-5
chromosome_fitness<-c()
probability<-c()
cum_prob<-c()
selected_chromosomes<-c()
intermediate_chromosomes<-c()
final_offsprings<-c()
mutation_sites<-c(1,4,2,1,3,5,2,4)
crossover_sites<-c(2,4,1,3)
for(iteration in 1:no_of_iteration){
  #fitness
  for(index in 1:no_of_chromosome){
    chromosome_fitness[index]<-fitness(chromosomes[index],range,length_chromosome)
  }
  #fitness probability
  sum_fitness<-sum(chromosome_fitness)
  print("average fitness")
  print(sum_fitness/no_of_chromosome)
  for(index in 1:no_of_chromosome){
    probability[index]<-chromosome_fitness[index]/sum_fitness
  }
  #cummulative probability
  cum_prob[1]<-probability[1]
  for(index in 2:no_of_chromosome){
    cum_prob[index]<-probability[index]+cum_prob[index-1]
  }
  #selection(Roulette wheel)
  for(index in 1:no_of_chromosome){
    random_no<-runif(1)
    for(cum_index in 1:no_of_chromosome){
      if(cum_prob[cum_index]>random_no){
        selected_chromosomes[index]<-chromosomes[cum_index]
        break
      }
    }
  }
  #cross-over
  index<-1
  crossover_site_index<-1
  while(index<no_of_chromosome){
    random_no<-runif(1)
    if(random_no<co_prob){
      co_site<-crossover_sites[crossover_site_index]
      c1<-paste(substr(selected_chromosomes[index],1,co_site),
                substr(selected_chromosomes[index+1],co_site+1,length_chromosome),sep = '')
      c2<-paste(substr(selected_chromosomes[index+1],1,co_site),
                substr(selected_chromosomes[index],co_site+1,length_chromosome),sep = '')
      intermediate_chromosomes[index]<-c1
      intermediate_chromosomes[index+1]<-c2
    }else{
      intermediate_chromosomes[index]<-selected_chromosomes[index]
      intermediate_chromosomes[index+1]<-selected_chromosomes[index+1]
    }
    index<-index+2
    crossover_site_index<-crossover_site_index+1
  }
  #mutation
  for(index in 1:no_of_chromosome){
    final_offsprings[index]<-intermediate_chromosomes[index]
    random_no<-runif(1)
    if(random_no<mu_prob){
      mu_site<-mutation_sites[index]
      mu_value<-substr(final_offsprings[index],mu_site,mu_site)
      if(mu_value=="1"){
        substr(final_offsprings[index],mu_site,mu_site)<-"0"
      }else{
        substr(final_offsprings[index],mu_site,mu_site)<-"1"
      }
    }
  }
  print("iteration")
  print(iteration)
  print(chromosomes)
  chromosomes<-final_offsprings
  print(selected_chromosomes)
  print(intermediate_chromosomes)
  print(final_offsprings)
}
for(index in 1:no_of_chromosome){
  chromosome_fitness[index]<-fitness(chromosomes[index],range,length_chromosome)
}
sum<-sum(chromosome_fitness)
print(sum/no_of_chromosome)