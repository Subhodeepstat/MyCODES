#####Genetic Algorithm over K Means in R#############


numofInd=20
k_max<-3
dim<-4

chromosome_length=dim*k_max

data1<-apply(iris[,-5],2,function(x){(x-min(x))/(max(x)-min(x))})



###Random Chromosome Generation Function####

initial_population <- function(numofInd) {
  ## generate a population to turn-on 15 cluster bits
  init <- t(replicate(numofInd, runif(chromosome_length, 0,1)))
  return(init)
}


######Constructing the fitness function for each chromosome##############

fitness_f<-function(i){
  #i<-initial[15,]
  c1<-i[1:4]
  c2<-i[5:8]
  c3<-i[9:12]
  c<-rbind(c1,c2,c3)
  data1<-as.data.frame(data1)
  dd<-data1 %>% rowwise() %>% mutate(cluster=which.min((Sepal.Length-c[,1])^2+(Sepal.Width-c[,2])^2+(Petal.Length-c[,3])^2+(Petal.Width-c[,4])^2)) %>% ungroup()
  #mod<-kmeans(data1,c)
  db<-index.DB(data1, cl=dd$cluster, centrotypes = "centroids")
  return(1/db$DB)
}

#####Reproduction stage####################

fitness_f_rep<-function(data){
  arr<-NULL
if(class(data)%in%"numeric"){
  arr<-fitness_f(data)
}else{ 

for(j in 1:nrow(data)){
 a<- fitness_f(data[j,])
 #cat(a,"\n")
 arr<-c(arr,a)
}
}
return(arr)
}

#######Sorting by ranks############
sort_func<-function(data){
 # data<-initial
  arr<-fitness_f_rep(data)
  data<-as.data.frame(data)
  data$fit<-arr
  data<-data %>% arrange(desc(fit))
  data$fit<-NULL
  data<-as.matrix(data)
  colnames(data)<-NULL
  return(data)
}


######Selection######################
selection<-function(data,ps=ps){
  
  for(i in 1:(as.integer(ps*numofInd)-1)){
    
    data[numofInd-i,]=data[i,]
  }
  data<-sort_func(data)
  return(data)
}


#######cross over function#########
do_crossover<-function(data,i,index){
  
  length<-ncol(data)
  cut<-sample(1:length,1)
  parent1<-data[index[i],]
  parent2<-data[index[i+1],]
  genesChild1<-c(parent1[1:cut],parent2[(cut+1):length])
  genesChild2<-c(parent1[(cut+1):length],parent2[1:cut])
  dat_new<-rbind(parent1,parent2,genesChild1,genesChild2)
  
  dat_new<-sort_func(dat_new)
  
  data[index[i],]<-dat_new[1,]
  data[index[i+1],]<-dat_new[2,]
  return(data)
}




cross_over_func<-function(data,pc){
index<-sample(1:(numofInd)-1,as.integer(pc*numofInd))

s<-seq(1,as.integer(length(index)/2),2)

for(j in s){
  data<-do_crossover(data,j,index)
  #cat(j,"\n")
}
data<-sort_func(data)
return(data)
}


#########Mutation Function###############

do_Mutation<-function(chrm_before_mut,chrm_after_mut,flag_mutation,fitness_list,i){
  length<-length(chrm_before_mut)
  dice<-NULL
  chrm_new<-NULL
  
  gene_flag<-NULL
  
  
  for(j in 1:length){
    
    dice[j]<-runif(1,0,1)
    
    if(dice[j]>pm){
      chrm_new[j]<-chrm_before_mut[j,];gene_flag[j]<-0
      
    }else{
      chrm_new[j]<-runif(1,0,1);gene_flag[j]<-1
      
    }
    
  } 
  
  
  check<-sum(gene_flag)
  
  if(check==0){
    flag_mutation[i]=0
    #data4<-as.data.frame(t(chrm_new))
    #data4$fit<-fitness_list[i]
    
  }else{
    flag_mutation[i]=1
    
  }
  chrm_after_mut<-rbind(chrm_after_mut,chrm_new)
  return(chrm_after_mut)
  
}



Mutation_func<-function(data,pm){
#data<-generation3
fitness_list<-fitness_f_rep(data)
flag_mutation<-rep(0,nrow(data))
chrm_after_mut<-NULL


for(i in 1:numofInd){
  if(i ==1){
    chrm_after_mut<-rbind(chrm_after_mut,data[1,]);flag_mutation[1]=0
  }else{
    chrm_after_mut<-do_Mutation(data[i,],chrm_after_mut,flag_mutation,fitness_list,i)
  }
}

chrm_after_mut<-sort_func(chrm_after_mut)
return(chrm_after_mut)
}


#############Function to implement genetic algorithm######
genetic_kmeans<-function(data,ps,pc,pm){
  #data<-initial
  generation1<-sort_func(data)
  generation1<-selection(generation1,ps)####selection step
  generation1<-cross_over_func(generation1,pc) ####crossover step
  generation1<-Mutation_func(generation1,pm) #####Mutation Step
  
  return(generation1)
  
    
  
}


###iter-1

generation1<-sort_func(generation)
generation2<-selection(generation1,ps)

do_crossover<-function(data,i,index){
  
  length<-ncol(data)
  cut<-sample(1:length,1)
  parent1<-data[index[i],]
  parent2<-data[index[i+1],]
  genesChild1<-c(parent1[1:cut],parent2[(cut+1):length])
  genesChild2<-c(parent1[(cut+1):length],parent2[1:cut])
  dat_new<-rbind(parent1,parent2,genesChild1,genesChild2)
  
  dat_new<-sort_func(dat_new)
  
  data[index[i],]<-dat_new[1,]
  data[index[i+1],]<-dat_new[2,]
  return(data)
}




cross_over_func<-function(data,pc){
  data<-generation2
  index<-sample(1:(numofInd)-1,as.integer(pc*numofInd))
  
  s<-seq(1,as.integer(length(index)/2),2)
  
  for(j in s){
    data<-do_crossover(data,j,index)
    #cat(j,"\n")
  }
  data<-sort_func(data)
  return(data)
}




generation3<-cross_over_func(generation2,pc)
generation4<-Mutation_func(generation3)

##iter-2

generation5<-sort_func(generation4)
generation6<-selection(generation5,ps)
generation7<-selection(generation6,pc)
generation8<-Mutation_func(generation3)


#####initialising the popilation############
initial<-initial_population(20)

####generation stage#######
generation<-sort_func(initial)

#####algorithm stage######
ps<-0.2#####Prob of Rank Selection
pc<-0.8####Prob of Crossover
pm<-0.02####Prob of Mutation
genetic_count=0

while(genetic_count<=100){
  g<-genetic_kmeans(generation,ps=ps,pc=pc,pm=pm)
  initial<-g
  genetic_count= genetic_count+1
  cat("Iteration ",genetic_count,"Done\n")
}



fitness_cl<-function(i){
  #i<-initial[15,]
  c1<-i[1:4]
  c2<-i[5:8]
  c3<-i[9:12]
  c<-rbind(c1,c2,c3)
  #data1<-as.data.frame(data1)
  #dd<-data1 %>% rowwise() %>% mutate(cluster=which.min((Sepal.Length-c[,1])^2+(Sepal.Width-c[,2])^2+(Petal.Length-c[,3])^2+(Petal.Width-c[,4])^2)) %>% ungroup()
  #mod<-kmeans(data1,c)
  #db<-index.DB(data1, cl=dd$cluster, centrotypes = "centroids")
  return(c)
}

zz<-fitness_cl(generation1[1,])
model<-kmeans(data1,zz)

table(iris$Species,model$cluster)

model1<-kmeans(data1,3)

table(iris$Species,model1$cluster)


fitness_f_rep(data)
fitness_f_rep(initial)
fitness_f_rep(sort_func(initial))

