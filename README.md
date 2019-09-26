# quanzi R program
#Compute the employee's location in supervisor's circle.
#Classifying 5 types of people by G-value (5 questions' average G-value)#

library(igraph)
print('Please confirm the preferred threshold is 0.3')
threshold = 0.3

#calculating the first question's G-value#
way <- file.choose() #use this to follow path for saving file#
Rdata <- read.csv(way)
name <- names(Rdata)[-1] 
M1<- as.matrix(Rdata[,-1])
diag(M1) <- 0
#final result will save in this file#
result <- cbind(name,'role','type') 

#Directed Effect# 
#Let variable Zji measure the strength of a connection between actors j and supervisor i:
#0 for no connection;
#1 for an asymmetric connection from j to i
#2 for an asymmetric tie from i to j
# 3 for reciprocal connections.
Deffect1 <- array(0,length(name))
Deffect1[M1[,1]==1] <- 2
Deffect1 <- Deffect1+M1[1,]
G1 <- cbind(name[-1],Deffect1[-1])

#Undirected Effect#
#The summed term measures connections from j with strong connections to colleagues k
#who have strong connections to supervisor i. #
mid1 <- array(0,length(name)) 
for(i in 2:ncol(M1)){
  mid1[i] <- sum(M1[i,]*M1[,1])
}
UDeffect1 <- (3*(mid1[-1] - min(mid1[-1])))/(max(mid1[-1]-min(mid1[-1])))
if(max(mid1[-1]) == 0){
  UDeffect1 <- 0 
}
G1 <- cbind(G1,UDeffect1)
GValue1 <- as.numeric(G1[,2])+as.numeric(G1[,3])

#calculating the second question's G-value#
way <- file.choose() 
Rdata <- read.csv(way)
name <- names(Rdata)[-1]
M2<- as.matrix(Rdata[,-1])
diag(M2) <- 0
Deffect2 <- array(0,length(name))
Deffect2[M2[,1]==1] <- 2
Deffect2 <- Deffect2+M2[1,]
G2 <- cbind(name[-1],Deffect2[-1])
mid2 <- array(0,length(name)) 
for(i in 2:ncol(M2)){
  mid2[i] <- sum(M2[i,]*M2[,1])
}
UDeffect2 <- (3*(mid2[-1] - min(mid2[-1])))/(max(mid2[-1]-min(mid2[-1])))
if(max(mid2[-1]) == 0){
  UDeffect2 <- 0 
}
G2 <- cbind(G2,UDeffect2)
GValue2 <- as.numeric(G2[,2])+as.numeric(G2[,3])

#calculating the third question's G-value#
way <- file.choose() 
Rdata <- read.csv(way)

name <- names(Rdata)[-1] 
M3<- as.matrix(Rdata[,-1])
diag(M3) <- 0
Deffect3 <- array(0,length(name))
Deffect3[M3[,1]==1] <- 2
Deffect3 <- Deffect3+M3[1,]
G3 <- cbind(name[-1],Deffect3[-1])
mid3 <- array(0,length(name)) 
for(i in 2:ncol(M3)){
  mid3[i] <- sum(M3[i,]*M3[,1])
}
UDeffect3 <- (3*(mid2[-1] - min(mid3[-1])))/(max(mid3[-1]-min(mid3[-1])))
if(max(mid3[-1]) == 0){
  UDeffect3 <- 0 
}
G3 <- cbind(G3,UDeffect3)
GValue3 <- as.numeric(G3[,2])+as.numeric(G3[,3])

#calculating the fourth question's G-value#
way <- file.choose() 
Rdata <- read.csv(way)
name <- names(Rdata)[-1] 
M4<- as.matrix(Rdata[,-1])
diag(M4) <- 0
Deffect4 <- array(0,length(name))
Deffect4[M4[,1]==1] <- 2
Deffect4 <- Deffect3+M3[1,]
G4 <- cbind(name[-1],Deffect4[-1])
mid4 <- array(0,length(name))
for(i in 2:ncol(M4)){
  mid4[i] <- sum(M4[i,]*M4[,1])
}
UDeffect4 <- (3*(mid4[-1] - min(mid4[-1])))/(max(mid4[-1]-min(mid4[-1])))
if(max(mid4[-1]) == 0){
  UDeffect4 <- 0 
}
G4 <- cbind(G4,UDeffect4)
GValue4 <- as.numeric(G4[,2])+as.numeric(G4[,3])

#calculating the fifth question's G-value#
way <- file.choose()
Rdata <- read.csv(way)
name <- names(Rdata)[-1] # save the name
M5<- as.matrix(Rdata[,-1])
diag(M5) <- 0
Deffect5 <- array(0,length(name))
Deffect5[M5[,1]==1] <- 2
Deffect5 <- Deffect5+M5[1,]
G5 <- cbind(name[-1],Deffect5[-1])

mid5 <- array(0,length(name)) #middle results#
for(i in 2:ncol(M5)){
  mid5[i] <- sum(M5[i,]*M5[,1])
}
UDeffect5 <- (3*(mid5[-1] - min(mid5[-1])))/(max(mid5[-1]-min(mid5[-1])))
if(max(mid5[-1]) == 0){
  UDeffect5 <- 0 
}
G5 <- cbind(G5,UDeffect5)
GValue5 <- as.numeric(G5[,2])+as.numeric(G5[,3])

#the final G-value by averaging the five questions 
GValue <- (GValue2+GValue1+GValue3+GValue4+GValue5)/5
names(GValue) <- name[-1] #G-value#

#calculating the arithmetic average of the five questions, let the value which bigger than 0.4 to 1, which smaller or equal to 0.2 to 0, which means the connections of the two members exist in two questions or more than two questions, the two members are considered have a connection.
M1[which(M1>=1)]<-1.0
M2[which(M2>=1)]<-1.0
M3[which(M3>=1)]<-1.0
M4[which(M4>=1)]<-1.0
M5[which(M5>=1)]<-1.0
M <- (M1+M2+M3+M4+M5)/5
M[which(M==0.2)]<-0.0
M[which(M==0.4|M==0.6|M==0.8)]<-1.0
#outputing the matrix M as the Data Set 2 #
filename1 <- strsplit(way,'\\\\')
filename1[[1]][length(filename1[[1]])] <- paste('matrix_',filename1[[1]][length(filename1[[1]])],sep='')
way1 <- paste(filename1[[1]],collapse = '\\')
write.csv(M,way1)

#sorting G-value#
Gsort <- sort(GValue,decreasing = TRUE)

#output the barplot#
filename <- strsplit(way,'\\\\')
plotname <- paste('Barplot_',filename[[1]][length(filename[[1]])],sep='')
plotname <- strsplit(plotname,'\\.')
plotname <- paste(plotname[[1]][1],'.jpeg',sep = '')
filename[[1]][length(filename[[1]])] <- plotname
plotway <- paste(filename[[1]],collapse = '\\')

barplot(Gsort)
#png(file = plotway)
#dev.off()

#finding cliffs#
mid <- array(0,length(Gsort))
mid[1:(length(Gsort)-1)]<-Gsort[-1]
Gcliff <- (Gsort-mid)[1:(length(Gsort)-1)]
Fcliff <- cbind(Gcliff,Gsort[1:(length(Gsort)-1)],Gsort[-1])
colnames(Fcliff)[2:3] <- c('Upper','Lower') 

#deciding class #
if(sum(Fcliff[,'Gcliff'] >= threshold) >=2){ #when there are more than two cliffs#
  Dis4 <- abs((Fcliff[,'Upper']-4)+(Fcliff[,'Lower']-4))
  Dis2 <- abs((Fcliff[,'Upper']-2)+(Fcliff[,'Lower']-2))
  Flag4 <- names(which.min(Dis4[Fcliff[,'Gcliff'] >= threshold]))
  Flag2 <- names(which.min(Dis2[Fcliff[,'Gcliff'] >= threshold]))
  core_members <-names(Gsort)[1:which(names(Gsort) == Flag4)]
  peripheral <- names(Gsort)[(which(names(Gsort) == Flag4)+1):which(names(Gsort) == Flag2)]
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] > threshold][which.min(Dis4[Fcliff[,'Gcliff'] >= threshold])] <= 2){
    peripheral <-names(Gsort)[1:which(names(Gsort) == Flag4)]
    core_members <- NA
  }
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] > threshold][which.min(Dis2[Fcliff[,'Gcliff'] >= threshold])] >= 4){
    core_members <- names(Gsort)[1:which(names(Gsort) == Flag2)]
    peripheral <- NA
  }
}

#if there is only one cliff we will compare it with 3# 
#over 3 is core, below 3 is peripheral#
if(sum(Fcliff[,'Gcliff'] >= threshold) ==1){ 
  if(Fcliff[,'Upper'][Fcliff[,'Gcliff'] >= threshold] >= 3){
    Flag4 <- rownames(Fcliff)[Fcliff[,'Gcliff'] >= threshold]
    core_members <-names(Gsort)[1:which(names(Gsort) == Flag4)]
    peripheral <- NA
  }else{
    Flag2 <- rownames(Fcliff)[Fcliff[,'Gcliff'] >= threshold]
    peripheral <- names(Gsort)[1:which(names(Gsort) == Flag2)]
    core_members <- NA
  }
}
#if there is no cliff we define there is no core and no peripheral#
#The program will clear memory and won't output anything#
if(sum(Fcliff[,'Gcliff'] >= threshold) ==0){
  core_members <- NA
  peripheral <- NA
  print("-------CAN NOT FIND CLIFF PLEASE USE OTHER ALGORITHM --------")
  rm(list = ls())
}

supervisor <- name[1]
result[,2][is.element(name,supervisor)] <- 'supervisor'
result[,3][is.element(name,supervisor)] <- 0
result[,2][is.element(name,core_members)] <- 'core member'
result[,3][is.element(name,core_members)] <- 1
result[,2][is.element(name,peripheral)] <- 'peripheral'
result[,3][is.element(name,peripheral)] <- 2

g <- graph.adjacency(as.matrix(M),mode = 'directed',weight = T)
#drawing the graph g #
iplot <- tkplot(g,vertex.color = 'green',vertex.label.color = 'black',canvas.width = 1000,canvas.height = 800)
tkplot.close(iplot)

#erasing ties between two people where there is no mutual recognition in ties.
# being aware of the edge.attr.comb; it must be ignored for calculating the correct distance.
g1 <- as.undirected(g,mode = 'mutual',edge.attr.comb=list("ignore"))
# drawing the graph g1 #
iplot <- tkplot(g1,vertex.color = 'green',vertex.label.color = 'black',canvas.width = 1000,canvas.height = 800)
tkplot.close(iplot)

g2 <- delete_vertices(g1,c(supervisor))
# drawing the graph g2 #
iplot <- tkplot(g2,vertex.color = 'green',vertex.label.color = 'black',canvas.width = 1000,canvas.height = 800)
tkplot.close(iplot)

#computing components with node number larger than 3. 
compo <- cohesive_blocks(g2)
compo <- blocks(compo)[parent(compo) ==1]

# locating the node with the highest degree of centrality in each component; these nodes will be coded as informal leaders.  
#core members of an informal leader were identified in Data Set 2 and labeled as informal leader's core. 
n <- length(compo)
inform <- NULL
informer <- NULL
record <- NULL
for(i in 1:(n)){
  if(length(names(compo[[i]]))>= 3){
    gg <- delete_vertices(g1,name[!is.element(name,names(compo[[i]]))])
    #find informal leaders#
    de <- degree(gg)
    inform <- names(de)[de==max(de)] 
    #saving result 3 means informal leader
    tn <- NA
    tn <- intersect(result[,1][result[,2] != 'role'],inform)
    result[,2][is.element(name,tn)]<-paste(result[,2][is.element(name,tn)],'informal leader',sep = ',')
    result[,3][is.element(name,tn)] <- paste(result[,3][is.element(name,tn)],3,sep = ',')
    result[,2][is.element(name,inform[!is.element(inform,tn)])] <- 'informal leader'
    result[,3][is.element(name,inform[!is.element(inform,tn)])] <- 3
    for(j in 1:(length(inform))){
      #find this informal leader's circle member
      DD <- distances(gg,v = inform[j])
      incore <- colnames(DD)[DD == 1]
      #saving result 4 means informal leader's circle member
      tn <- NA
      tn <- intersect(result[,1][result[,2] != 'role'],incore)
      result[,2][is.element(name,tn)] <- paste(result[,2][is.element(name,tn)],paste('informal leader ',inform[j],'\'s circle member',sep = ''),sep = ',')
      result[,3][is.element(name,tn)] <- paste(result[,3][is.element(name,tn)],4,sep = ',')
      result[,2][is.element(name,incore[!is.element(incore,tn)])] <- paste('informal leader ',inform[j],'\'s circle member',sep = '')
      result[,3][is.element(name,incore[!is.element(incore,tn)])] <- 4
      
      informer <- c(informer,list(c(inform[j],incore)))
      record <-c(record,c(inform[j],incore)) 
    }
  }
}
for(i in 1:length(informer)){
  if(length(inform)){
    names(informer[[i]])[1] <- 'inform leader'
    names(informer[[i]])[-1]<- "inform leader's circle"
  }
}

#calculating bridges. 
bridge <- c(peripheral [is.element(peripheral,record)], core_members[is.element (core_members, record)])
flagbridge <- (length(bridge)>0)

result[,2][is.element(name,bridge)] <- paste(result[,2][is.element(name,bridge)],'bridge',sep = ',')
result[,3][is.element(name,bridge)] <- paste(result[,3][is.element(name,bridge)],5,sep=',')

#in the final step, all of these four categories are taken as someone’s circle members, and others will be coded as outsiders.
record <- c(record,supervisor,core_members,bridge,peripheral)
outsider <- name[!is.element(name,record)]
result[,2][is.element(name,outsider)] <- 'outsider'
result[,3][is.element(name,outsider)] <- 6

#output and the result is saved.
print(c("supervisor is", supervisor))
print("supervisor's core member are:")
print(core_members)
print("supervisors's peripheral circle members are: ")
print(peripheral)
print('informal leader and their circle members are:')
print(informer)
if(flagbridge){
  print("there has bridge, they are:")
  print(bridge)
  result<- rbind(result,c('bridge','have',99))
}else{
  print('there is no bridge')
  result<- rbind(result,c('bridge','no',-99))
}
print('the outsider are')
print(outsider)

#output to file#
filename <- strsplit(way,'\\\\')
filename[[1]][length(filename[[1]])] <- paste('result_',filename[[1]][length(filename[[1]])],sep='')
way <- paste(filename[[1]],collapse = '\\')
print(result)
write.csv(result,way)
rm(list = ls())


