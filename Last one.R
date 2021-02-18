library(lme4)
library(lmerTest)
library(igraph)
library(fmsb)
setwd("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition")

europe1<-list()
europe2<-list()
korea1<-list()
korea2<-list()
brazil1<-list()
brazil2<-list()
oceania1<-list()
oceania2<-list()
america1<-list()
america2<-list()
for(i in 1:193){
    korea1[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/kr/1/",i,".csv"))
}
for(i in 1:195){
  brazil1[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/br/1/",i,".csv"))
}
for(i in 1:198){
  america2[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/na/2/",i,".csv"))
}
for(i in 1:197){
  brazil2[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/br/2/",i,".csv"))
  oceania1[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/oc/1/",i,".csv"))
}
for(i in setdiff(1:190,152)){
  europe2[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/eun1/2/",i,".csv"))
}
for(i in setdiff(1:197,c(145,152))){
  oceania2[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/oc/2/",i,".csv"))
}
for(i in setdiff(1:195,139)){
  korea2[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/kr/2/",i,".csv"))
}
for(i in setdiff(1:198,126)){
  europe1[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/eun1/1/",i,".csv"))
}
for(i in setdiff(1:197,c(52,126))){
  america1[[i]]<-read.csv(file=paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/na/1/",i,".csv"))
}
europe<-c(europe1,europe2)
europe[[126]]<-NULL
europe[[349]]<-NULL
europe[[292]]<-NULL
europe[[4]]<-NULL
europe[[77]]<-NULL
europe[[224]]<-NULL
europe[[275]]<-NULL
europe[[373]]<-NULL
europe[[107]]<-NULL
korea<-c(korea1,korea2)
korea[[332]]<-NULL
korea[[131]]<-NULL
korea[[293]]<-NULL
brazil<-c(brazil1,brazil2)
brazil[[43]]<-NULL
brazil[[159]]<-NULL
brazil[[184]]<-NULL
brazil[[201]]<-NULL
brazil[[254]]<-NULL
brazil[[264]]<-NULL
brazil[[119]]<-NULL
brazil[[22]]<-NULL
brazil[[95]]<-NULL
brazil[[108]]<-NULL
brazil[[119]]<-NULL
oceania<-c(oceania1,oceania2)
oceania[[342]]<-NULL
oceania[[348]]<-NULL
oceania[[35]]<-NULL
oceania[[121]]<-NULL
oceania[[124]]<-NULL
oceania[[364]]<-NULL
america<-c(america1,america2)
america[[52]]<-NULL
america[[125]]<-NULL
america[[13]]<-NULL
america[[93]]<-NULL
america[[132]]<-NULL
america[[271]]<-NULL
america[[132]]<-NULL
america[[38]]<-NULL
america[[268]]<-NULL
america[[310]]<-NULL
america[[337]]<-NULL
america[[382]]<-NULL
america[[324]]<-NULL

idk1 <- list()
idk2 <- list()
idk3 <- list()
idk4 <- list()
idk5 <- list()
idk6 <- list()
idk7 <- list()
idk8 <- list()
idk9 <- list()
idk10 <- list()

ide1 <- list()
ide2 <- list()
ide3 <- list()
ide4 <- list()
ide5 <- list()
ide6 <- list()
ide7 <- list()
ide8 <- list()
ide9 <- list()
ide10 <- list()

ido1 <- list()
ido2 <- list()
ido3 <- list()
ido4 <- list()
ido5 <- list()
ido6 <- list()
ido7 <- list()
ido8 <- list()
ido9 <- list()
ido10 <- list()

idb1 <- list()
idb2 <- list()
idb3 <- list()
idb4 <- list()
idb5 <- list()
idb6 <- list()
idb7 <- list()
idb8 <- list()
idb9 <- list()
idb10 <- list()

ida1 <- list()
ida2 <- list()
ida3 <- list()
ida4 <- list()
ida5 <- list()
ida6 <- list()
ida7 <- list()
ida8 <- list()
ida9 <- list()
ida10 <- list()
for (j in 1:385){ #Calculating the ammounts of kills for every ID
  idk1[[j]] <- length(which(korea[[j]]$PLAYERID=="1"))
  idk2[[j]] <- length(which(korea[[j]]$PLAYERID=="2"))
  idk3[[j]] <- length(which(korea[[j]]$PLAYERID=="3"))
  idk4[[j]] <- length(which(korea[[j]]$PLAYERID=="4"))
  idk5[[j]] <- length(which(korea[[j]]$PLAYERID=="5"))
  idk6[[j]] <- length(which(korea[[j]]$PLAYERID=="6"))
  idk7[[j]] <- length(which(korea[[j]]$PLAYERID=="7"))
  idk8[[j]] <- length(which(korea[[j]]$PLAYERID=="8"))
  idk9[[j]] <- length(which(korea[[j]]$PLAYERID=="9"))
  idk10[[j]] <- length(which(korea[[j]]$PLAYERID=="10"))
}
for(j in 1:379){
  ide1[[j]] <- length(which(europe[[j]]$PLAYERID=="1"))
  ide2[[j]] <- length(which(europe[[j]]$PLAYERID=="2"))
  ide3[[j]] <- length(which(europe[[j]]$PLAYERID=="3"))
  ide4[[j]] <- length(which(europe[[j]]$PLAYERID=="4"))
  ide5[[j]] <- length(which(europe[[j]]$PLAYERID=="5"))
  ide6[[j]] <- length(which(europe[[j]]$PLAYERID=="6"))
  ide7[[j]] <- length(which(europe[[j]]$PLAYERID=="7"))
  ide8[[j]] <- length(which(europe[[j]]$PLAYERID=="8"))
  ide9[[j]] <- length(which(europe[[j]]$PLAYERID=="9"))
  ide10[[j]] <- length(which(europe[[j]]$PLAYERID=="10"))
}
for(j in 1:388){
  ido1[[j]] <- length(which(oceania[[j]]$PLAYERID=="1"))
  ido2[[j]] <- length(which(oceania[[j]]$PLAYERID=="2"))
  ido3[[j]] <- length(which(oceania[[j]]$PLAYERID=="3"))
  ido4[[j]] <- length(which(oceania[[j]]$PLAYERID=="4"))
  ido5[[j]] <- length(which(oceania[[j]]$PLAYERID=="5"))
  ido6[[j]] <- length(which(oceania[[j]]$PLAYERID=="6"))
  ido7[[j]] <- length(which(oceania[[j]]$PLAYERID=="7"))
  ido8[[j]] <- length(which(oceania[[j]]$PLAYERID=="8"))
  ido9[[j]] <- length(which(oceania[[j]]$PLAYERID=="9"))
  ido10[[j]] <- length(which(oceania[[j]]$PLAYERID=="10"))
}
for(j in 1:381){
  idb1[[j]] <- length(which(brazil[[j]]$PLAYERID=="1"))
  idb2[[j]] <- length(which(brazil[[j]]$PLAYERID=="2"))
  idb3[[j]] <- length(which(brazil[[j]]$PLAYERID=="3"))
  idb4[[j]] <- length(which(brazil[[j]]$PLAYERID=="4"))
  idb5[[j]] <- length(which(brazil[[j]]$PLAYERID=="5"))
  idb6[[j]] <- length(which(brazil[[j]]$PLAYERID=="6"))
  idb7[[j]] <- length(which(brazil[[j]]$PLAYERID=="7"))
  idb8[[j]] <- length(which(brazil[[j]]$PLAYERID=="8"))
  idb9[[j]] <- length(which(brazil[[j]]$PLAYERID=="9"))
  idb10[[j]] <- length(which(brazil[[j]]$PLAYERID=="10"))
}
for (j in 1:382){
  ida1[[j]] <- length(which(america[[j]]$PLAYERID=="1"))
  ida2[[j]] <- length(which(america[[j]]$PLAYERID=="2"))
  ida3[[j]] <- length(which(america[[j]]$PLAYERID=="3"))
  ida4[[j]] <- length(which(america[[j]]$PLAYERID=="4"))
  ida5[[j]] <- length(which(america[[j]]$PLAYERID=="5"))
  ida6[[j]] <- length(which(america[[j]]$PLAYERID=="6"))
  ida7[[j]] <- length(which(america[[j]]$PLAYERID=="7"))
  ida8[[j]] <- length(which(america[[j]]$PLAYERID=="8"))
  ida9[[j]] <- length(which(america[[j]]$PLAYERID=="9"))
  ida10[[j]] <- length(which(america[[j]]$PLAYERID=="10"))
}

edt <- cbind(unlist(ide1),unlist(ide2),unlist(ide3),unlist(ide4),unlist(ide5),unlist(ide6),unlist(ide7),unlist(ide8),unlist(ide9),unlist(ide10))
edt <- as.data.frame(edt)
adt <- cbind(unlist(ida1),unlist(ida2),unlist(ida3),unlist(ida4),unlist(ida5),unlist(ida6),unlist(ida7),unlist(ida8),unlist(ida9),unlist(ida10))
adt <- as.data.frame(adt)
kdt <- cbind(unlist(idk1),unlist(idk2),unlist(idk3),unlist(idk4),unlist(idk5),unlist(idk6),unlist(idk7),unlist(idk8),unlist(idk9),unlist(idk10))
kdt <- as.data.frame(kdt)
bdt <- cbind(unlist(idb1),unlist(idb2),unlist(idb3),unlist(idb4),unlist(idb5),unlist(idb6),unlist(idb7),unlist(idb8),unlist(idb9),unlist(idb10))
bdt <- as.data.frame(bdt)
odt <- cbind(unlist(ido1),unlist(ido2),unlist(ido3),unlist(ido4),unlist(ido5),unlist(ido6),unlist(ido7),unlist(ido8),unlist(ido9),unlist(ido10))
odt <- as.data.frame(odt)

edt[,11] <- rowSums(edt[,1:5],na.rm = T)
adt[,11] <- rowSums(adt[,1:5],na.rm = T)
kdt[,11] <- rowSums(kdt[,1:5],na.rm = T)
bdt[,11] <- rowSums(bdt[,1:5],na.rm = T)
odt[,11] <- rowSums(odt[,1:5],na.rm = T)
edt[,12] <- rowSums(edt[,6:10],na.rm = T)
adt[,12] <- rowSums(adt[,6:10],na.rm = T)
kdt[,12] <- rowSums(kdt[,6:10],na.rm = T)
bdt[,12] <- rowSums(bdt[,6:10],na.rm = T)
odt[,12] <- rowSums(odt[,6:10],na.rm = T)

for(i in 1:379){
  edt[i,13]<-europe[[i]][1,8]/60
  europe[[i]][,10]<-paste0(europe[[i]][,7],europe[[i]][,9])
  if(europe[[i]][1,10]==1000){
    edt[i,14]<-0}
  if(europe[[i]][1,10]==2001){
    edt[i,14]<-0}
  if(europe[[i]][1,10]==2000){
    edt[i,14]<-1}
  if(europe[[i]][1,10]==1001){
    edt[i,14]<-1}
  
  if(europe[[i]][1,10]==1000){
    edt[i,15]<-1}
  if(europe[[i]][1,10]==2001){
    edt[i,15]<-1}
  if(europe[[i]][1,10]==2000){
    edt[i,15]<-0}
  if(europe[[i]][1,10]==1001){
    edt[i,15]<-0}
  
  edt[i,16]<-unique(europe[[i]][which(europe[[i]][,7]==100),][,6])#T1 gold
  edt[i,17]<-unique(europe[[i]][which(europe[[i]][,7]==200),][,6])#T2 gold
  edt[i,18]<-edt[i,11]/as.numeric(edt[i,13])#T1 kill per minute
  edt[i,19]<-edt[i,12]/as.numeric(edt[i,13])#T2 kill per minute
  edt[i,20]<-edt[i,16]/as.numeric(edt[i,13])#T1 gold per minute
  edt[i,21]<-edt[i,17]/as.numeric(edt[i,13])#T2 gold per minute
  edt[i,22]<-length(which(europe[[i]][c(1:nrow(europe[[i]])),c(1:4)]<6))#T1 assists
  edt[i,23]<-edt[i,22]/as.numeric(edt[i,11])#T1 assists per kill
  edt[i,24]<-edt[i,22]/as.numeric(edt[i,13])#T1 assists per minute
  edt[i,25]<-length(which(europe[[i]][c(1:nrow(europe[[i]])),c(1:4)]>5))#T2 assists
  edt[i,26]<-edt[i,24]/as.numeric(edt[i,11])#T2 assists per kill
  edt[i,27]<-edt[i,24]/as.numeric(edt[i,13])#T2 assists per minute
}
for(i in 1:385){
  kdt[i,13]<-korea[[i]][1,8]/60
  korea[[i]][,10]<-paste0(korea[[i]][,7],korea[[i]][,9])
  if(korea[[i]][1,10]==1000){
    kdt[i,14]<-0}
  if(korea[[i]][1,10]==2001){
    kdt[i,14]<-0}
  if(korea[[i]][1,10]==2000){
    kdt[i,14]<-1}
  if(korea[[i]][1,10]==1001){
    kdt[i,14]<-1}
  
  if(korea[[i]][1,10]==1000){
    kdt[i,15]<-1}
  if(korea[[i]][1,10]==2001){
    kdt[i,15]<-1}
  if(korea[[i]][1,10]==2000){
    kdt[i,15]<-0}
  if(korea[[i]][1,10]==1001){
    kdt[i,15]<-0}
  
  kdt[i,16]<-unique(korea[[i]][which(korea[[i]][,7]==100),][,6])
  kdt[i,17]<-unique(korea[[i]][which(korea[[i]][,7]==200),][,6])
  kdt[i,18]<-kdt[i,11]/as.numeric(kdt[i,13])
  kdt[i,19]<-kdt[i,12]/as.numeric(kdt[i,13])
  kdt[i,20]<-kdt[i,16]/as.numeric(kdt[i,13])
  kdt[i,21]<-kdt[i,17]/as.numeric(kdt[i,13])
  kdt[i,22]<-length(which(korea[[i]][c(1:nrow(korea[[i]])),c(1:4)]<6))#T1 assists
  kdt[i,23]<-kdt[i,22]/as.numeric(kdt[i,11])#T1 assists per kill
  kdt[i,24]<-kdt[i,22]/as.numeric(kdt[i,13])#T1 assists per minute
  kdt[i,25]<-length(which(korea[[i]][c(1:nrow(korea[[i]])),c(1:4)]>5))#T2 assists
  kdt[i,26]<-kdt[i,24]/as.numeric(kdt[i,11])#T2 assists per kill
  kdt[i,27]<-kdt[i,24]/as.numeric(kdt[i,13])#T2 assists per minute
}
for(i in 1:381){
  bdt[i,13]<-brazil[[i]][1,8]/60
  brazil[[i]][,10]<-paste0(brazil[[i]][,7],brazil[[i]][,9])
  if(brazil[[i]][1,10]==1000){
    bdt[i,14]<-0}
  if(brazil[[i]][1,10]==2001){
    bdt[i,14]<-0}
  if(brazil[[i]][1,10]==2000){
    bdt[i,14]<-1}
  if(brazil[[i]][1,10]==1001){
    bdt[i,14]<-1}
  
  if(brazil[[i]][1,10]==1000){
    bdt[i,15]<-1}
  if(brazil[[i]][1,10]==2001){
    bdt[i,15]<-1}
  if(brazil[[i]][1,10]==2000){
    bdt[i,15]<-0}
  if(brazil[[i]][1,10]==1001){
    bdt[i,15]<-0}
  
  bdt[i,16]<-unique(brazil[[i]][which(brazil[[i]][,7]==100),][,6])
  bdt[i,17]<-unique(brazil[[i]][which(brazil[[i]][,7]==200),][,6])
  bdt[i,18]<-bdt[i,11]/as.numeric(bdt[i,13])
  bdt[i,19]<-bdt[i,12]/as.numeric(bdt[i,13])
  bdt[i,20]<-bdt[i,16]/as.numeric(bdt[i,13])
  bdt[i,21]<-bdt[i,17]/as.numeric(bdt[i,13])
  bdt[i,22]<-length(which(brazil[[i]][c(1:nrow(brazil[[i]])),c(1:4)]<6))#T1 assists
  bdt[i,23]<-bdt[i,22]/as.numeric(bdt[i,11])#T1 assists per kill
  bdt[i,24]<-bdt[i,22]/as.numeric(bdt[i,13])#T1 assists per minute
  bdt[i,25]<-length(which(brazil[[i]][c(1:nrow(brazil[[i]])),c(1:4)]>5))#T2 assists
  bdt[i,26]<-bdt[i,24]/as.numeric(bdt[i,11])#T2 assists per kill
  bdt[i,27]<-bdt[i,24]/as.numeric(bdt[i,13])#T2 assists per minute
}
for(i in 1:388){
  odt[i,13]<-oceania[[i]][1,8]/60
  oceania[[i]][,10]<-paste0(oceania[[i]][,7],oceania[[i]][,9])
  if(oceania[[i]][1,10]==1000){
    odt[i,14]<-0}
  if(oceania[[i]][1,10]==2001){
    odt[i,14]<-0}
  if(oceania[[i]][1,10]==2000){
    odt[i,14]<-1}
  if(oceania[[i]][1,10]==1001){
    odt[i,14]<-1}
  
  if(oceania[[i]][1,10]==1000){
    odt[i,15]<-1}
  if(oceania[[i]][1,10]==2001){
    odt[i,15]<-1}
  if(oceania[[i]][1,10]==2000){
    odt[i,15]<-0}
  if(oceania[[i]][1,10]==1001){
    odt[i,15]<-0}
  
  odt[i,16]<-unique(oceania[[i]][which(oceania[[i]][,7]==100),][,6])
  odt[i,17]<-unique(oceania[[i]][which(oceania[[i]][,7]==200),][,6])
  odt[i,18]<-odt[i,11]/as.numeric(odt[i,13])
  odt[i,19]<-odt[i,12]/as.numeric(odt[i,13])
  odt[i,20]<-odt[i,16]/as.numeric(odt[i,13])
  odt[i,21]<-odt[i,17]/as.numeric(odt[i,13])
  odt[i,22]<-length(which(oceania[[i]][c(1:nrow(oceania[[i]])),c(1:4)]<6))#T1 assists
  odt[i,23]<-odt[i,22]/as.numeric(odt[i,11])#T1 assists per kill
  odt[i,24]<-odt[i,22]/as.numeric(odt[i,13])#T1 assists per minute
  odt[i,25]<-length(which(oceania[[i]][c(1:nrow(oceania[[i]])),c(1:4)]>5))#T2 assists
  odt[i,26]<-odt[i,24]/as.numeric(odt[i,11])#T2 assists per kill
  odt[i,27]<-odt[i,24]/as.numeric(odt[i,13])#T2 assists per minute
}
for(i in 1:382){
  adt[i,13]<-america[[i]][1,8]/60
  america[[i]][,10]<-paste0(america[[i]][,7],america[[i]][,9])
  if(america[[i]][1,10]==1000){
    adt[i,14]<-0}
  if(america[[i]][1,10]==2001){
    adt[i,14]<-0}
  if(america[[i]][1,10]==2000){
    adt[i,14]<-1}
  if(america[[i]][1,10]==1001){
    adt[i,14]<-1}
  
  if(america[[i]][1,10]==1000){
    adt[i,15]<-1}
  if(america[[i]][1,10]==2001){
    adt[i,15]<-1}
  if(america[[i]][1,10]==2000){
    adt[i,15]<-0}
  if(america[[i]][1,10]==1001){
    adt[i,15]<-0}
  
  adt[i,16]<-unique(america[[i]][which(america[[i]][,7]==100),][,6])
  adt[i,17]<-unique(america[[i]][which(america[[i]][,7]==200),][,6])
  adt[i,18]<-adt[i,11]/as.numeric(adt[i,13])
  adt[i,19]<-adt[i,12]/as.numeric(adt[i,13])
  adt[i,20]<-adt[i,16]/as.numeric(adt[i,13])
  adt[i,21]<-adt[i,17]/as.numeric(adt[i,13])
  adt[i,22]<-length(which(america[[i]][c(1:nrow(america[[i]])),c(1:4)]<6))#T1 assists
  adt[i,23]<-adt[i,22]/as.numeric(adt[i,11])#T1 assists per kill
  adt[i,24]<-adt[i,22]/as.numeric(adt[i,13])#T1 assists per minute
  adt[i,25]<-length(which(america[[i]][c(1:nrow(america[[i]])),c(1:4)]>5))#T2 assists
  adt[i,26]<-adt[i,24]/as.numeric(adt[i,11])#T2 assists per kill
  adt[i,27]<-adt[i,24]/as.numeric(adt[i,13])#T2 assists per minute
}

unicol<-c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10","T1Kills","T2Kills","Time","T1Win","T2Win","T1Gold","T2Gold","T1Killspermin","T2Killspermin",
          "T1Goldpermin","T2Goldpermin","T1Assists","T1Assistsperkill","T1Assistspermin","T2Assists","T2Assistsperkill","T2Assistspermin")
colnames(edt)<-unicol
colnames(kdt)<-unicol
colnames(bdt)<-unicol
colnames(odt)<-unicol
colnames(adt)<-unicol

feeu<-list()
fekr<-list()
febr<-list()
feoc<-list()
feam<-list()

eeu <- list()
ekr <- list()
eam <- list()
ebr <- list()
eoc <- list()
eeu1 <- list()
eeu2 <- list()
ekr1 <- list()
ekr2 <- list()
ebr1 <- list()
ebr2 <- list()
eoc1 <- list()
eoc2 <- list()
eam1 <- list()
eam2 <- list()

weeu1<-list()
weeu2 <- list()
wekr1 <- list()
wekr2 <- list()
webr1 <- list()
webr2 <- list()
weoc1 <- list()
weoc2 <- list()
weam1 <- list()
weam2 <- list()

coneu1<-list()
coneu2<-list()
conkr1<-list()
conkr2<-list()
conbr1<-list()
conbr2<-list()
conoc1<-list()
conoc2<-list()
conam1<-list()
conam2<-list()

geu<-list()
gkr<-list()
gbr<-list()
goc<-list()
gam<-list()
geu1<-list()
geu2<-list()
gkr1<-list()
gkr2<-list()
gbr1<-list()
gbr2<-list()
goc1<-list()
goc2<-list()
gam1<-list()
gam2<-list()

for (i in 1:379){ #Modifying the data sets so that they can be turned into edge lists
  feeu[[i]] <- europe[[i]][,c(1:5)]
  eeu[[i]] <- reshape2::melt(feeu[[i]], id.vars = "PLAYERID")
  eeu[[i]] <- eeu[[i]][!is.na(eeu[[i]][, 3]), c(1, 3)]
  eeu[[i]] <- as.matrix(eeu[[i]])
  eeu1[[i]] <- eeu[[i]][eeu[[i]][,1]<6,]
  eeu2[[i]] <- eeu[[i]][eeu[[i]][,1]>5,]
  
  weeu1[[i]]<-as.data.frame(paste(eeu1[[i]][,1],eeu1[[i]][,2], sep=""))
  weeu2[[i]]<-as.data.frame(paste(eeu2[[i]][,1],eeu2[[i]][,2], sep=""))
  coneu1[[i]]<-nrow(weeu1[[i]])
  coneu2[[i]]<-nrow(weeu2[[i]])
  
  geu[[i]] <- graph_from_edgelist(eeu[[i]])
  geu1[[i]] <- graph_from_edgelist(eeu1[[i]])
  geu2[[i]] <- graph_from_edgelist(eeu2[[i]])
}

for(i in 1:381){
  febr[[i]] <- brazil[[i]][,c(1:5)]
  ebr[[i]] <- reshape2::melt(febr[[i]], id.vars = "PLAYERID")
  ebr[[i]] <- ebr[[i]][!is.na(ebr[[i]][, 3]), c(1, 3)]
  ebr[[i]] <- as.matrix(ebr[[i]])
  ebr1[[i]] <- ebr[[i]][ebr[[i]][,1]<6,]
  ebr2[[i]] <- ebr[[i]][ebr[[i]][,1]>5,]
  
  webr1[[i]]<-as.data.frame(paste(ebr1[[i]][,1],ebr1[[i]][,2], sep=""))
  webr2[[i]]<-as.data.frame(paste(ebr2[[i]][,1],ebr2[[i]][,2], sep=""))
  conbr1[[i]]<-nrow(webr1[[i]])
  conbr2[[i]]<-nrow(webr2[[i]])
  
  gbr[[i]] <- graph_from_edgelist(ebr[[i]])
  gbr1[[i]] <- graph_from_edgelist(ebr1[[i]])
  gbr2[[i]] <- graph_from_edgelist(ebr2[[i]])
}

for(i in 1:385){
  fekr[[i]]<-korea[[i]][,c(1:5)]
  ekr[[i]] <- reshape2::melt(fekr[[i]], id.vars = "PLAYERID")
  ekr[[i]] <- ekr[[i]][!is.na(ekr[[i]][, 3]), c(1, 3)]
  ekr[[i]] <- as.matrix(ekr[[i]])
  ekr1[[i]] <- ekr[[i]][ekr[[i]][,1]<6,]
  ekr2[[i]] <- ekr[[i]][ekr[[i]][,1]>5,]
  
  wekr1[[i]]<-as.data.frame(paste(ekr1[[i]][,1],ekr1[[i]][,2], sep=""))
  wekr2[[i]]<-as.data.frame(paste(ekr2[[i]][,1],ekr2[[i]][,2], sep=""))
  conkr1[[i]]<-nrow(wekr1[[i]])
  conkr2[[i]]<-nrow(wekr2[[i]])
  
  gkr[[i]] <- graph_from_edgelist(ekr[[i]])
  gkr1[[i]] <- graph_from_edgelist(ekr1[[i]])
  gkr2[[i]] <- graph_from_edgelist(ekr2[[i]])
}

for(i in 1:388){ 
  feoc[[i]]<-oceania[[i]][,c(1:5)]
  eoc[[i]] <- reshape2::melt(feoc[[i]], id.vars = "PLAYERID")
  eoc[[i]] <- eoc[[i]][!is.na(eoc[[i]][, 3]), c(1, 3)]
  eoc[[i]] <- as.matrix(eoc[[i]])
  eoc1[[i]] <- eoc[[i]][eoc[[i]][,1]<6,]
  eoc2[[i]] <- eoc[[i]][eoc[[i]][,1]>5,]
  
  weoc1[[i]]<-as.data.frame(paste(eoc1[[i]][,1],eoc1[[i]][,2], sep=""))
  weoc2[[i]]<-as.data.frame(paste(eoc2[[i]][,1],eoc2[[i]][,2], sep=""))
  conoc1[[i]]<-nrow(weoc1[[i]])
  conoc2[[i]]<-nrow(weoc2[[i]])
  
  goc[[i]] <- graph_from_edgelist(eoc[[i]])
  goc1[[i]] <- graph_from_edgelist(eoc1[[i]])
  goc2[[i]] <- graph_from_edgelist(eoc2[[i]])
}
for (i in 1:382){
  feam[[i]] <- america[[i]][,c(1:5)]
  eam[[i]] <- reshape2::melt(feam[[i]], id.vars = "PLAYERID")
  eam[[i]] <- eam[[i]][!is.na(eam[[i]][, 3]), c(1, 3)]
  eam[[i]] <- as.matrix(eam[[i]])
  eam1[[i]] <- eam[[i]][eam[[i]][,1]<6,]
  eam2[[i]] <- eam[[i]][eam[[i]][,1]>5,]
  
  weam1[[i]]<-as.data.frame(paste(eam1[[i]][,1],eam1[[i]][,2], sep=""))
  weam2[[i]]<-as.data.frame(paste(eam2[[i]][,1],eam2[[i]][,2], sep=""))
  conam1[[i]]<-nrow(weam1[[i]])
  conam2[[i]]<-nrow(weam2[[i]])
  
  gam[[i]] <- graph_from_edgelist(eam[[i]])
  gam1[[i]] <- graph_from_edgelist(eam1[[i]])
  gam2[[i]] <- graph_from_edgelist(eam2[[i]])
}

t1con<-setdiff(c(12:54),c(16:20,22,26:30,33,36:40,44,46:50,55))
t2con<-setdiff(c(67:98,106:109,610,710,810,910),c(70:75,77,80:85,88,90:95,99))

for(h in 1:379){#EU1
  for(i in 1:coneu1[[h]]){
    for(j in t1con){
      if(weeu1[[h]][i,1]==j){
        weeu1[[h]][i,2]<-length(which(weeu1[[h]]==j))/nrow(weeu1[[h]])
      }
    }
  }
}

for(h in 1:379){#EU2
  for(i in 1:coneu2[[h]]){
    for(j in t2con){
      if(weeu2[[h]][i,1]==j){
        weeu2[[h]][i,2]<-length(which(weeu2[[h]]==j))/nrow(weeu2[[h]])
      }
    }
  }
}
for(h in 1:385){#KR1
  for(i in 1:conkr1[[h]]){
    for(j in t1con){
      if(wekr1[[h]][i,1]==j){
        wekr1[[h]][i,2]<-length(which(wekr1[[h]]==j))/nrow(wekr1[[h]])
      }
    }
  }
}
for(h in 1:385){#KR2
  for(i in 1:conkr2[[h]]){
    for(j in t2con){
      if(wekr2[[h]][i,1]==j){
        wekr2[[h]][i,2]<-length(which(wekr2[[h]]==j))/nrow(wekr2[[h]])
      }
    }
  }
}
for(h in 1:381){#BR1
  for(i in 1:conbr1[[h]]){
    for(j in t1con){
      if(webr1[[h]][i,1]==j){
        webr1[[h]][i,2]<-length(which(webr1[[h]]==j))/nrow(webr1[[h]])
      }
    }
  }
}
for(h in 1:381){#BR2
  for(i in 1:conbr2[[h]]){
    for(j in t2con){
      if(webr2[[h]][i,1]==j){
        webr2[[h]][i,2]<-length(which(webr2[[h]]==j))/nrow(webr2[[h]])
      }
    }
  }
}
for(h in 1:388){#OC1
  for(i in 1:conoc1[[h]]){
    for(j in t1con){
      if(weoc1[[h]][i,1]==j){
        weoc1[[h]][i,2]<-length(which(weoc1[[h]]==j))/nrow(weoc1[[h]])
      }
    }
  }
}
for(h in 1:388){#OC2
  for(i in 1:conoc2[[h]]){
    for(j in t2con){
      if(weoc2[[h]][i,1]==j){
        weoc2[[h]][i,2]<-length(which(weoc2[[h]]==j))/nrow(weoc2[[h]])
      }
    }
  }
}
for(h in 1:382){#NA1
  for(i in 1:conam1[[h]]){
    for(j in t1con){
      if(weam1[[h]][i,1]==j){
        weam1[[h]][i,2]<-length(which(weam1[[h]]==j))/nrow(weam1[[h]])
      }
    }
  }
}
for(h in 1:382){#NA2
  for(i in 1:conam2[[h]]){
    for(j in t2con){
      if(weam2[[h]][i,1]==j){
        weam2[[h]][i,2]<-length(which(weam2[[h]]==j))/nrow(weam2[[h]])
      }
    }
  }
}
edt1 <- edt[,c(1:5,11,13,14,16,18,20,22,23,24)]
edt2 <- edt[,c(6:10,12,13,15,17,19,21,25,26,27)]
adt1 <- adt[,c(1:5,11,13,14,16,18,20,22,23,24)]
adt2 <- adt[,c(6:10,12,13,15,17,19,21,25,26,27)]
kdt1 <- kdt[,c(1:5,11,13,14,16,18,20,22,23,24)]
kdt2 <- kdt[,c(6:10,12,13,15,17,19,21,25,26,27)]
bdt1 <- bdt[,c(1:5,11,13,14,16,18,20,22,23,24)]
bdt2 <- bdt[,c(6:10,12,13,15,17,19,21,25,26,27)]
odt1 <- odt[,c(1:5,11,13,14,16,18,20,22,23,24)]
odt2 <- odt[,c(6:10,12,13,15,17,19,21,25,26,27)]
edt1[,15]<-rep("Europe",379)
edt2[,15]<-rep("Europe",379)
adt1[,15]<-rep("America",382)
adt2[,15]<-rep("America",382)
kdt1[,15]<-rep("Korea",385)
kdt2[,15]<-rep("Korea",385)
bdt1[,15]<-rep("Brazil",381)
bdt2[,15]<-rep("Brazil",381)
odt1[,15]<-rep("Oceania",388)
odt2[,15]<-rep("Oceania",388)

for(i in 1:379){
  geu1[[i]]<-set.edge.attribute(graph = geu1[[i]],name = "weight",value = weeu1[[i]][,2])
  geu2[[i]]<-set.edge.attribute(graph = geu2[[i]],name = "weight",value = weeu2[[i]][,2])
  geu1[[i]]<-simplify(geu1[[i]],remove.multiple = T)
  geu2[[i]]<-simplify(geu2[[i]],remove.multiple = T)
  edt1[i,16]<-centr_degree(geu1[[i]], mode = "in",normalized = T)[[2]]
  edt2[i,16]<-centr_degree(geu2[[i]], mode = "in",normalized = T)[[2]]
  edt1[i,17]<-centr_degree(geu1[[i]], mode = "out",normalized = T)[[2]]
  edt2[i,17]<-centr_degree(geu2[[i]], mode = "out",normalized = T)[[2]]
}
for(i in 1:385){
  gkr1[[i]]<-set.edge.attribute(graph = gkr1[[i]],name = "weight",value = wekr1[[i]][,2])
  gkr2[[i]]<-set.edge.attribute(graph = gkr2[[i]],name = "weight",value = wekr2[[i]][,2])
  gkr1[[i]]<-simplify(gkr1[[i]],remove.multiple = T)
  gkr2[[i]]<-simplify(gkr2[[i]],remove.multiple = T)
  kdt1[i,16]<-centr_degree(gkr1[[i]], mode = "in",normalized = T)[[2]]
  kdt2[i,16]<-centr_degree(gkr2[[i]], mode = "in",normalized = T)[[2]]
  kdt1[i,17]<-centr_degree(gkr1[[i]], mode = "out",normalized = T)[[2]]
  kdt2[i,17]<-centr_degree(gkr2[[i]], mode = "out",normalized = T)[[2]]

}
for(i in 1:381){
  gbr1[[i]]<-set.edge.attribute(graph = gbr1[[i]],name = "weight",value = webr1[[i]][,2])
  gbr2[[i]]<-set.edge.attribute(graph = gbr2[[i]],name = "weight",value = webr2[[i]][,2])
  gbr1[[i]]<-simplify(gbr1[[i]],remove.multiple = T)
  gbr2[[i]]<-simplify(gbr2[[i]],remove.multiple = T)
  bdt1[i,16]<-centr_degree(gbr1[[i]], mode = "in",normalized = T)[[2]]
  bdt2[i,16]<-centr_degree(gbr2[[i]], mode = "in",normalized = T)[[2]]
  bdt1[i,17]<-centr_degree(gbr1[[i]], mode = "out",normalized = T)[[2]]
  bdt2[i,17]<-centr_degree(gbr2[[i]], mode = "out",normalized = T)[[2]]
}
for(i in 1:388){
  goc1[[i]]<-set.edge.attribute(graph = goc1[[i]],name = "weight",value = weoc1[[i]][,2])
  goc2[[i]]<-set.edge.attribute(graph = goc2[[i]],name = "weight",value = weoc2[[i]][,2])
  goc1[[i]]<-simplify(goc1[[i]],remove.multiple = T)
  goc2[[i]]<-simplify(goc2[[i]],remove.multiple = T)
  odt1[i,16]<-centr_degree(goc1[[i]], mode = "in",normalized = T)[[2]]
  odt2[i,16]<-centr_degree(goc2[[i]], mode = "in",normalized = T)[[2]]
  odt1[i,17]<-centr_degree(goc1[[i]], mode = "out",normalized = T)[[2]]
  odt2[i,17]<-centr_degree(goc2[[i]], mode = "out",normalized = T)[[2]]
}
for(i in 1:382){
  gam1[[i]]<-set.edge.attribute(graph = gam1[[i]],name = "weight",value = weam1[[i]][,2])
  gam2[[i]]<-set.edge.attribute(graph = gam2[[i]],name = "weight",value = weam2[[i]][,2])
  gam1[[i]]<-simplify(gam1[[i]],remove.multiple = T)
  gam2[[i]]<-simplify(gam2[[i]],remove.multiple = T)
  adt1[i,16]<-centr_degree(gam1[[i]], mode = "in",normalized = T)[[2]]
  adt2[i,16]<-centr_degree(gam2[[i]], mode = "in",normalized = T)[[2]]
  adt1[i,17]<-centr_degree(gam1[[i]], mode = "out",normalized = T)[[2]]
  adt2[i,17]<-centr_degree(gam2[[i]], mode = "out",normalized = T)[[2]]
}
unicol <- c("ID1","ID2","ID3","ID4","ID5","Kills","Time","Win","Gold","Killspermin","Goldpermin","Assists","Intensity","Assistspermin","Region","IndegCent","OutdegCent")
colnames(edt1)<-unicol
colnames(edt2)<-unicol
colnames(adt1)<-unicol
colnames(adt2)<-unicol
colnames(kdt1)<-unicol
colnames(kdt2)<-unicol
colnames(bdt1)<-unicol
colnames(bdt2)<-unicol
colnames(odt1)<-unicol
colnames(odt2)<-unicol
unidt <- rbind(edt1,edt2,adt1,adt2,kdt1,kdt2,bdt1,bdt2,odt1,odt2)

summary(unidt$Assistspermin)
summary(unidt$Killspermin)

summary(unidt$Intensity)

summary(unidt$IndegCent)
summary(unidt$OutdegCent)
summary(unidt$Goldpermin)

NagelkerkeR2(glm(Win~Intensity+OutdegCent+IndegCent+Intensity:Region, unidt, family="binomial"))

plot(density(unidt$Goldpermin), main = "Distribution of gold per minute", lwd=2, col="dark green")
summary(glm(Win~Intensity+OutdegCent+IndegCent+Intensity:Region, unidt, family="binomial"))
summary(lm(Goldpermin~Intensity+IndegCent+OutdegCent+Intensity:Region, unidt))
linear<-summary(lm(Goldpermin~Killspermin+Intensity+IndegCent+OutdegCent, unidt))
lineari<-summary(lm(Goldpermin~Killspermin+Intensity+IndegCent+OutdegCent+Intensity:Region, unidt))
mixed<-summary(lmer(Goldpermin~Killspermin+Intensity+IndegCent+OutdegCent+(1|Region),unidt))
lineark<-summary(lm(Killspermin~Intensity+IndegCent+OutdegCent, unidt))
