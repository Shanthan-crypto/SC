fitness<-function(operands){
  particle_fitness<-1/(1+abs(50-sum(operands)))
  return(particle_fitness)
}

particles<-list()
no_of_particles<-20
for(index in 1:no_of_particles){
  particles[[index]]<-runif(10,min = -500,max = 500)
}
velocity<-list()
for(index in 1:no_of_particles){
  random_velocity<-runif(1,min = -1,max = 1)
  velocity[[index]]<-rep(random_velocity,10)
}


no_of_iteration<-4

w<-0.70
c1<-0.20
c2<-0.60
particle_best<-list()
particle_best_fitness<-c()
particle_best<-particles
particles_Sum<-c()
par(mfrow=c(1,2))
for(index in 1:no_of_particles){
  particles_Sum[index]<-sum(particles[[index]])
}

plot(particles_Sum,type="p")

for(iteration in 1:no_of_iteration){
  particle_fitness<-c()
  for(index in 1:no_of_particles){
    particle_fitness[index]<-fitness(particles[[index]])
    if(iteration == 1){
      particle_best[[index]]<-particles[[index]]
      particle_best_fitness[index]<-particle_fitness[index]
      next
    }
    if(particle_fitness[index]>particle_best_fitness[index]){
      particle_best[[index]]<-particles[[index]]
      particle_best_fitness[index]<-particle_fitness[index]
    }
    
  }
  gbest_position<-which.max(particle_fitness)
  new_velocity<-list()
  for(index in 1:no_of_particles){
    r1<-runif(1)
    r2<-runif(1)
    velocity[[index]]<-velocity[[index]]*w+c1*r1*(particle_best[[index]]-particles[[index]])+c2*r2*(particles[[gbest_position]]-particles[[index]])
    particles[[index]]<-particles[[index]]+velocity[[index]]
  }
}
for(index in 1:no_of_particles){
  particles_Sum[index]<-sum(particles[[index]])
}

plot(particles_Sum,type="p")