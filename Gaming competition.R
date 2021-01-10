library(rjson)
library(lme4)
library(lmerTest)
library(gsubfn)
library(igraph)
setwd("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition")
korea <- list()
europe <- list()
usa <- list()
oceania <- list()
brazil <- list()
chippysmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Chippys/matcheschippys.json")
chippysmatches <- data.frame(matrix(unlist(chippysmatches[[1]]),nrow = length(chippysmatches[[1]]), byrow = T))
idchippys <- as.vector(chippysmatches[,2])
capsmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Caps/matchescaps.json")
capsmatches <- data.frame(matrix(unlist(capsmatches[[1]]),nrow = length(capsmatches[[1]]), byrow = T))
idcaps <- as.vector(capsmatches[,2])
dlmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Doublelift/matchesdl.json")
dlmatches <- data.frame(matrix(unlist(dlmatches[[1]]),nrow = length(dlmatches[[1]]), byrow = T))
iddl <- as.vector(dlmatches[,2])
fakermatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Faker/matchesfaker.json")
fakermatches <- data.frame(matrix(unlist(fakermatches[[1]]),nrow = length(fakermatches[[1]]), byrow = T))
idfaker <- as.vector(fakermatches[,2])
brttmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/BrTT/matchesbrtt.json")
brttmatches <- data.frame(matrix(unlist(brttmatches[[1]]),nrow = length(brttmatches[[1]]), byrow = T))
idbrtt <- as.vector(brttmatches[,2])

babipmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Babip/matchesbabip.json")
babipmatches <- data.frame(matrix(unlist(babipmatches[[1]]),nrow = length(babipmatches[[1]]), byrow = T))
idbabip <- as.vector(babipmatches[,2])
rekklesmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Rekkles/matchesrekkles.json")
rekklesmatches <- data.frame(matrix(unlist(rekklesmatches[[1]]),nrow = length(rekklesmatches[[1]]), byrow = T))
idrekkles <- as.vector(rekklesmatches[,2])
licoricematches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Licorice/matcheslicorice.json")
licoricematches <- data.frame(matrix(unlist(licoricematches[[1]]),nrow = length(licoricematches[[1]]), byrow = T))
idlicorice <- as.vector(licoricematches[,2])
doinbmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Doinb/matchesdoinb.json")
doinbmatches <- data.frame(matrix(unlist(doinbmatches[[1]]),nrow = length(doinbmatches[[1]]), byrow = T))
iddoinb <- as.vector(doinbmatches[,2])
yangmatches <- fromJSON(file="C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Yang/matchesyang.json")
yangmatches <- data.frame(matrix(unlist(yangmatches[[1]]),nrow = length(yangmatches[[1]]), byrow = T))
idyang <- as.vector(yangmatches[,2])
babipcsv <- list()
brttcsv <- list()
capscsv <- list()
chippyscsv <- list()
dlcsv <- list()
doinbcsv <- list()
fakercsv <- list()
licoricecsv <- list()
rekklescsv <- list()
yangcsv <- list()
iddl <- setdiff(iddl,iddl[88])
for (i in 1:100){
  babipcsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Babip/",idbabip[i],".csv",sep="")
  brttcsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/BrTT/",idbrtt[i],".csv",sep="")
  capscsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Caps/",idcaps[i],".csv",sep="")
  chippyscsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Chippys/",idchippys[i],".csv",sep="")
  dlcsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Doublelift/",iddl[i],".csv",sep="")
  doinbcsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Doinb/",iddoinb[i],".csv",sep="")
  fakercsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Faker/",idfaker[i],".csv",sep="")
  licoricecsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Licorice/",idlicorice[i],".csv",sep="")
  rekklescsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Rekkles/",idrekkles[i],".csv",sep="")
  yangcsv[[i]] <- paste0("C:/Users/Mateusz/Documents/Studia/Thesis/Gaming competition/Yang/",idyang[i],".csv",sep="")
}
babip <- list()
brtt <- list()
caps <- list()
chippys <- list()
dl <- list()
doinb <- list()
faker <- list()
licorice <- list()
rekkles <- list()
yang <- list()
for (i in 1:100){
  babip[[i]] <- read.csv(file=babipcsv[[i]]) 
  brtt[[i]] <- read.csv(file=brttcsv[[i]]) 
  caps[[i]] <- read.csv(file=capscsv[[i]]) 
  chippys[[i]] <- read.csv(file=chippyscsv[[i]]) 
  doinb[[i]] <- read.csv(file=doinbcsv[[i]]) 
  faker[[i]] <- read.csv(file=fakercsv[[i]]) 
  licorice[[i]] <- read.csv(file=licoricecsv[[i]]) 
  rekkles[[i]] <- read.csv(file=rekklescsv[[i]]) 
  yang[[i]] <- read.csv(file=yangcsv[[i]]) 
  dl[[i]] <- read.csv(file=dlcsv[[i]]) 
}
korea <- c(faker,doinb)
usa <- c(dl,licorice)
oceania <- c(chippys,babip)
brazil <- c(brtt,yang)
europe <- c(caps,rekkles)
regions <- c("korea", "usa", "oceania", "brazil", "europe")

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
for (j in 1:200){
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

idu1 <- list()
idu2 <- list()
idu3 <- list()
idu4 <- list()
idu5 <- list()
idu6 <- list()
idu7 <- list()
idu8 <- list()
idu9 <- list()
idu10 <- list()
for (j in 1:199){
  idu1[[j]] <- length(which(usa[[j]]$PLAYERID=="1"))
  idu2[[j]] <- length(which(usa[[j]]$PLAYERID=="2"))
  idu3[[j]] <- length(which(usa[[j]]$PLAYERID=="3"))
  idu4[[j]] <- length(which(usa[[j]]$PLAYERID=="4"))
  idu5[[j]] <- length(which(usa[[j]]$PLAYERID=="5"))
  idu6[[j]] <- length(which(usa[[j]]$PLAYERID=="6"))
  idu7[[j]] <- length(which(usa[[j]]$PLAYERID=="7"))
  idu8[[j]] <- length(which(usa[[j]]$PLAYERID=="8"))
  idu9[[j]] <- length(which(usa[[j]]$PLAYERID=="9"))
  idu10[[j]] <- length(which(usa[[j]]$PLAYERID=="10"))
}
edt <- cbind(unlist(ide1),unlist(ide2),unlist(ide3),unlist(ide4),unlist(ide5),unlist(ide6),unlist(ide7),unlist(ide8),unlist(ide9),unlist(ide10))
edt <- as.data.frame(edt)
udt <- cbind(unlist(idu1),unlist(idu2),unlist(idu3),unlist(idu4),unlist(idu5),unlist(idu6),unlist(idu7),unlist(idu8),unlist(idu9),unlist(idu10))
udt <- as.data.frame(udt)
kdt <- cbind(unlist(idk1),unlist(idk2),unlist(idk3),unlist(idk4),unlist(idk5),unlist(idk6),unlist(idk7),unlist(idk8),unlist(idk9),unlist(idk10))
kdt <- as.data.frame(kdt)
bdt <- cbind(unlist(idb1),unlist(idb2),unlist(idb3),unlist(idb4),unlist(idb5),unlist(idb6),unlist(idb7),unlist(idb8),unlist(idb9),unlist(idb10))
bdt <- as.data.frame(bdt)
odt <- cbind(unlist(ido1),unlist(ido2),unlist(ido3),unlist(ido4),unlist(ido5),unlist(ido6),unlist(ido7),unlist(ido8),unlist(ido9),unlist(ido10))
odt <- as.data.frame(odt)

edt[,11] <- rowSums(edt[,1:5],na.rm = T)
udt[,11] <- rowSums(udt[,1:5],na.rm = T)
kdt[,11] <- rowSums(kdt[,1:5],na.rm = T)
bdt[,11] <- rowSums(bdt[,1:5],na.rm = T)
odt[,11] <- rowSums(odt[,1:5],na.rm = T)
edt[,12] <- rowSums(edt[,6:10],na.rm = T)
udt[,12] <- rowSums(udt[,6:10],na.rm = T)
kdt[,12] <- rowSums(kdt[,6:10],na.rm = T)
bdt[,12] <- rowSums(bdt[,6:10],na.rm = T)
odt[,12] <- rowSums(odt[,6:10],na.rm = T)

ep1 <- list()
ep2 <- list()
ep3 <- list()
ep4 <- list()
ep5 <- list()
ep6 <- list()
ep7 <- list()
ep8 <- list()
ep9 <- list()
ep10 <- list()

up1 <- list()
up2 <- list()
up3 <- list()
up4 <- list()
up5 <- list()
up6 <- list()
up7 <- list()
up8 <- list()
up9 <- list()
up10 <- list()

bp1 <- list()
bp2 <- list()
bp3 <- list()
bp4 <- list()
bp5 <- list()
bp6 <- list()
bp7 <- list()
bp8 <- list()
bp9 <- list()
bp10 <- list()

kp1 <- list()
kp2 <- list()
kp3 <- list()
kp4 <- list()
kp5 <- list()
kp6 <- list()
kp7 <- list()
kp8 <- list()
kp9 <- list()
kp10 <- list()

op1 <- list()
op2 <- list()
op3 <- list()
op4 <- list()
op5 <- list()
op6 <- list()
op7 <- list()
op8 <- list()
op9 <- list()
op10 <- list()
for (i in 1:200){
  ep1[[i]] <- edt[i,1]/edt[i,11]
  ep2[[i]] <- edt[i,2]/edt[i,11]
  ep3[[i]] <- edt[i,3]/edt[i,11]
  ep4[[i]] <- edt[i,4]/edt[i,11]
  ep5[[i]] <- edt[i,5]/edt[i,11]
  ep6[[i]] <- edt[i,6]/edt[i,12]
  ep7[[i]] <- edt[i,7]/edt[i,12]
  ep8[[i]] <- edt[i,8]/edt[i,12]
  ep9[[i]] <- edt[i,9]/edt[i,12]
  ep10[[i]] <- edt[i,10]/edt[i,12]
  
  bp1[[i]] <- bdt[i,1]/bdt[i,11]
  bp2[[i]] <- bdt[i,2]/bdt[i,11]
  bp3[[i]] <- bdt[i,3]/bdt[i,11]
  bp4[[i]] <- bdt[i,4]/bdt[i,11]
  bp5[[i]] <- bdt[i,5]/bdt[i,11]
  bp6[[i]] <- bdt[i,6]/bdt[i,12]
  bp7[[i]] <- bdt[i,7]/bdt[i,12]
  bp8[[i]] <- bdt[i,8]/bdt[i,12]
  bp9[[i]] <- bdt[i,9]/bdt[i,12]
  bp10[[i]] <- bdt[i,10]/bdt[i,12]
  
  kp1[[i]] <- kdt[i,1]/kdt[i,11]
  kp2[[i]] <- kdt[i,2]/kdt[i,11]
  kp3[[i]] <- kdt[i,3]/kdt[i,11]
  kp4[[i]] <- kdt[i,4]/kdt[i,11]
  kp5[[i]] <- kdt[i,5]/kdt[i,11]
  kp6[[i]] <- kdt[i,6]/kdt[i,12]
  kp7[[i]] <- kdt[i,7]/kdt[i,12]
  kp8[[i]] <- kdt[i,8]/kdt[i,12]
  kp9[[i]] <- kdt[i,9]/kdt[i,12]
  kp10[[i]] <- kdt[i,10]/kdt[i,12]
  
  op1[[i]] <- odt[i,1]/odt[i,11]
  op2[[i]] <- odt[i,2]/odt[i,11]
  op3[[i]] <- odt[i,3]/odt[i,11]
  op4[[i]] <- odt[i,4]/odt[i,11]
  op5[[i]] <- odt[i,5]/odt[i,11]
  op6[[i]] <- odt[i,6]/odt[i,12]
  op7[[i]] <- odt[i,7]/odt[i,12]
  op8[[i]] <- odt[i,8]/odt[i,12]
  op9[[i]] <- odt[i,9]/odt[i,12]
  op10[[i]] <- odt[i,10]/odt[i,12]
}
for (i in 1:199){
  up1[[i]] <- udt[i,1]/udt[i,11]
  up2[[i]] <- udt[i,2]/udt[i,11]
  up3[[i]] <- udt[i,3]/udt[i,11]
  up4[[i]] <- udt[i,4]/udt[i,11]
  up5[[i]] <- udt[i,5]/udt[i,11]
  up6[[i]] <- udt[i,6]/udt[i,12]
  up7[[i]] <- udt[i,7]/udt[i,12]
  up8[[i]] <- udt[i,8]/udt[i,12]
  up9[[i]] <- udt[i,9]/udt[i,12]
  up10[[i]] <- udt[i,10]/udt[i,12]
}

edtp <- cbind(unlist(ep1),unlist(ep2),unlist(ep3),unlist(ep4),unlist(ep5),unlist(ep6),unlist(ep7),unlist(ep8),unlist(ep9),unlist(ep10))
edtp <- as.data.frame(edtp)
udtp <- cbind(unlist(up1),unlist(up2),unlist(up3),unlist(up4),unlist(up5),unlist(up6),unlist(up7),unlist(up8),unlist(up9),unlist(up10))
udtp <- as.data.frame(udtp)
kdtp <- cbind(unlist(kp1),unlist(kp2),unlist(kp3),unlist(kp4),unlist(kp5),unlist(kp6),unlist(kp7),unlist(kp8),unlist(kp9),unlist(kp10))
kdtp <- as.data.frame(kdtp)
bdtp <- cbind(unlist(bp1),unlist(bp2),unlist(bp3),unlist(bp4),unlist(bp5),unlist(bp6),unlist(bp7),unlist(bp8),unlist(bp9),unlist(bp10))
bdtp <- as.data.frame(bdtp)
odtp <- cbind(unlist(op1),unlist(op2),unlist(op3),unlist(op4),unlist(op5),unlist(op6),unlist(op7),unlist(op8),unlist(op9),unlist(op10))
odtp <- as.data.frame(odtp)
columnsp <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10")
colnames(edtp) <- columnsp
colnames(udtp) <- columnsp
colnames(kdtp) <- columnsp
colnames(bdtp) <- columnsp
colnames(odtp) <- columnsp

solocarrye1 <- list()
solocarryu1 <- list()
solocarryk1 <- list()
solocarryb1 <- list()
solocarryo1 <- list()
solocarrye2 <- list()
solocarryu2 <- list()
solocarryk2 <- list()
solocarryb2 <- list()
solocarryo2 <- list()

duocarrye1 <- list()
duocarryu1 <- list()
duocarryk1 <- list()
duocarryb1 <- list()
duocarryo1 <- list()
duocarrye2 <- list()
duocarryu2 <- list()
duocarryk2 <- list()
duocarryb2 <- list()
duocarryo2 <- list()

wine1 <- list()
winu1 <- list()
wink1 <- list()
winb1 <- list()
wino1 <- list()
wine2 <- list()
winu2 <- list()
wink2 <- list()
winb2 <- list()
wino2 <- list()

for (i in 1:200){
  solocarrye1[[i]] <- any(edtp[i,1:5] >= 0.45)
  solocarryk1[[i]] <- any(kdtp[i,1:5] >= 0.45)
  solocarryb1[[i]] <- any(bdtp[i,1:5] >= 0.45)
  solocarryo1[[i]] <- any(odtp[i,1:5] >= 0.45)
  solocarrye2[[i]] <- any(edtp[i,6:10] >= 0.45)
  solocarryk2[[i]] <- any(kdtp[i,6:10] >= 0.45)
  solocarryb2[[i]] <- any(bdtp[i,6:10] >= 0.45)
  solocarryo2[[i]] <- any(odtp[i,6:10] >= 0.45)
  
  duocarrye1[[i]] <- sum(edtp[i,1:5]>=0.3)==2
  duocarryk1[[i]] <- sum(edtp[i,1:5]>=0.3)==2
  duocarryb1[[i]] <- sum(edtp[i,1:5]>=0.3)==2
  duocarryo1[[i]] <- sum(edtp[i,1:5]>=0.3)==2
  duocarrye2[[i]] <- sum(edtp[i,6:10]>=0.3)==2
  duocarryk2[[i]] <- sum(edtp[i,6:10]>=0.3)==2
  duocarryb2[[i]] <- sum(edtp[i,6:10]>=0.3)==2
  duocarryo2[[i]] <- sum(edtp[i,6:10]>=0.3)==2
  
  wine1[[i]] <- edt[i,11]>edt[i,12]
  wink1[[i]] <- kdt[i,11]>kdt[i,12]
  winb1[[i]] <- bdt[i,11]>bdt[i,12]
  wino1[[i]] <- odt[i,11]>odt[i,12]
  wine2[[i]] <- edt[i,11]<edt[i,12]
  wink2[[i]] <- kdt[i,11]<kdt[i,12]
  winb2[[i]] <- bdt[i,11]<bdt[i,12]
  wino2[[i]] <- odt[i,11]<odt[i,12]
}

for (i in 1:199){
  solocarryu1[[i]] <- any(udtp[i,1:5] >= 0.45)
  solocarryu2[[i]] <- any(udtp[i,6:10] >= 0.45)
  
  duocarryu1[[i]] <- sum(edtp[i,1:5]>=0.3)==2
  duocarryu2[[i]] <- sum(edtp[i,6:10]>=0.3)==2
  
  winu1[[i]] <- udt[i,11]>udt[i,12]
  winu2[[i]] <- udt[i,11]<udt[i,12]
}

edt[,13] <- unlist(solocarrye1)
udt[,13] <- unlist(solocarryu1)
kdt[,13] <- unlist(solocarryk1)
bdt[,13] <- unlist(solocarryb1)
odt[,13] <- unlist(solocarryo1)
edt[,14] <- unlist(solocarrye2)
udt[,14] <- unlist(solocarryu2)
kdt[,14] <- unlist(solocarryk2)
bdt[,14] <- unlist(solocarryb2)
odt[,14] <- unlist(solocarryo2)

edt[,15] <- unlist(duocarrye1)
udt[,15] <- unlist(duocarryu1)
kdt[,15] <- unlist(duocarryk1)
bdt[,15] <- unlist(duocarryb1)
odt[,15] <- unlist(duocarryo1)
edt[,16] <- unlist(duocarrye2)
udt[,16] <- unlist(duocarryu2)
kdt[,16] <- unlist(duocarryk2)
bdt[,16] <- unlist(duocarryb2)
odt[,16] <- unlist(duocarryo2)

edt[,17] <- unlist(wine1)
udt[,17] <- unlist(winu1)
kdt[,17] <- unlist(wink1)
bdt[,17] <- unlist(winb1)
odt[,17] <- unlist(wino1)
edt[,18] <- unlist(wine2)
udt[,18] <- unlist(winu2)
kdt[,18] <- unlist(wink2)
bdt[,18] <- unlist(winb2)
odt[,18] <- unlist(wino2)

columns <- c("ID1","ID2","ID3","ID4","ID5","ID6","ID7","ID8","ID9","ID10","T1","T2","Solo1","Solo2","Duo1","Duo2","T1W","T2W")
colnames(edt) <- columns
colnames(udt) <- columns
colnames(kdt) <- columns
colnames(bdt) <- columns
colnames(odt) <- columns

lms1 <- list()
lms2 <- list()

lms1[[1]] <- lm(T1~Solo1+Duo1, edt)
lms2[[1]] <- lm(T2~Solo2+Duo2, edt)
lms1[[2]] <- lm(T1~Solo1+Duo1, udt)
lms2[[2]] <- lm(T2~Solo2+Duo2, udt)
lms1[[3]] <- lm(T1~Solo1+Duo1, kdt)
lms2[[3]] <- lm(T2~Solo2+Duo2, kdt)
lms1[[4]] <- lm(T1~Solo1+Duo1, bdt)
lms2[[4]] <- lm(T2~Solo2+Duo2, bdt)
lms1[[5]] <- lm(T1~Solo1+Duo1, odt)
lms2[[5]] <- lm(T2~Solo2+Duo2, odt)

rese1 <- coef(lms1[[1]])
rese2 <- coef(lms2[[1]])
resu1 <- coef(lms1[[2]])
resu2 <- coef(lms2[[2]])
resk1 <- coef(lms1[[3]])
resk2 <- coef(lms2[[3]])
resb1 <- coef(lms1[[4]])
resb2 <- coef(lms2[[4]])
reso1 <- coef(lms1[[5]])
reso2 <- coef(lms2[[5]])

lres <- cbind(rese1,rese2,resu1,resu2,resk1,resk2,resb1,resb2,reso1,reso2)
rescol <- c("Europe1","Europe2","NAmerica1","NAmerica2","Korea1","Korea2","Brazil1","Brazil2","Oceania1","Oceania2")
colnames(lres) <- rescol

edt1 <- edt[,c(1:5,11,13,15,17)]
edt2 <- setdiff(edt,edt1)
udt1 <- udt[,c(1:5,11,13,15,17)]
udt2 <- udt[,c(6:10,12,14,16,18)]
kdt1 <- kdt[,c(1:5,11,13,15,17)]
kdt2 <- setdiff(kdt,kdt1)
bdt1 <- bdt[,c(1:5,11,13,15,17)]
bdt2 <- setdiff(bdt,bdt1)
odt1 <- odt[,c(1:5,11,13,15,17)]
odt2 <- odt[,c(6:10,12,14,16,18)]

unicol <- colnames(edt1)
colnames(edt2) <- unicol
colnames(udt1) <- unicol
colnames(udt2) <- unicol
colnames(kdt1) <- unicol
colnames(kdt2) <- unicol
colnames(bdt1) <- unicol
colnames(bdt2) <- unicol
colnames(odt1) <- unicol
colnames(odt2) <- unicol

edt1[,10] <- rep("Europe", times=200)
edt2[,10] <- rep("Europe", times=200)
udt1[,10] <- rep("NAmerica", times=199)
udt2[,10] <- rep("NAmerica", times=199)
kdt1[,10] <- rep("Korea", times=200)
kdt2[,10] <- rep("Korea", times=200)
bdt1[,10] <- rep("Brazil", times=200)
bdt2[,10] <- rep("Brazil", times=200)
odt1[,10] <- rep("Oceania", times=200)
odt2[,10] <- rep("Oceania", times=200)

eeu <- list()
ekr <- list()
ena <- list()
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
ena1 <- list()
ena2 <- list()

for (i in 1:200){
  eeu[[i]] <- reshape2::melt(europe[[i]], id.vars = "PLAYERID")
  eeu[[i]] <- eeu[[i]][!is.na(eeu[[i]][, 3]), c(1, 3)]
  eeu[[i]] <- as.matrix(eeu[[i]])
  
  ekr[[i]] <- reshape2::melt(korea[[i]], id.vars = "PLAYERID")
  ekr[[i]] <- ekr[[i]][!is.na(ekr[[i]][, 3]), c(1, 3)]
  ekr[[i]] <- as.matrix(ekr[[i]])
  
  ebr[[i]] <- reshape2::melt(brazil[[i]], id.vars = "PLAYERID")
  ebr[[i]] <- ebr[[i]][!is.na(ebr[[i]][, 3]), c(1, 3)]
  ebr[[i]] <- as.matrix(ebr[[i]])
  
  eoc[[i]] <- reshape2::melt(oceania[[i]], id.vars = "PLAYERID")
  eoc[[i]] <- eoc[[i]][!is.na(eoc[[i]][, 3]), c(1, 3)]
  eoc[[i]] <- as.matrix(eoc[[i]])
  
  eeu1[[i]] <- eeu[[i]][eeu[[i]][,1]<6,]
  eeu2[[i]] <- eeu[[i]][eeu[[i]][,1]>5,]
  
  ekr1[[i]] <- ekr[[i]][ekr[[i]][,1]<6,]
  ekr2[[i]] <- ekr[[i]][ekr[[i]][,1]>5,]
  
  ebr1[[i]] <- ebr[[i]][ebr[[i]][,1]<6,]
  ebr2[[i]] <- ebr[[i]][ebr[[i]][,1]>5,]
  
  eoc1[[i]] <- eoc[[i]][eoc[[i]][,1]<6,]
  eoc2[[i]] <- eoc[[i]][eoc[[i]][,1]>5,]
}
for (i in 1:199){
  ena[[i]] <- reshape2::melt(usa[[i]], id.vars = "PLAYERID")
  ena[[i]] <- ena[[i]][!is.na(ena[[i]][, 3]), c(1, 3)]
  ena[[i]] <- as.matrix(ena[[i]])
  
  ena1[[i]] <- ena[[i]][ena[[i]][,1]<6,]
  ena2[[i]] <- ena[[i]][ena[[i]][,1]>5,]
}
geu <- list()
gkr <- list()
gbr <- list()
goc <- list()
gna <- list()
geu1 <- list()
geu2 <- list()
gkr1 <- list()
gkr2 <- list()
gbr1 <- list()
gbr2 <- list()
goc1 <- list()
goc2 <- list()
gna1 <- list()
gna2 <- list()

eeu1[[61]] <- eeu1[[6]]
eeu1[[131]] <- eeu1[[6]]
eeu1[[145]] <- eeu1[[6]]
eeu2[[61]] <- eeu1[[6]]
eeu2[[131]] <- eeu1[[6]]
eeu2[[145]] <- eeu1[[6]]
ena1[[128]] <- eeu1[[6]]
ena2[[128]] <- eeu1[[6]]

for (i in 1:200){
  geu[[i]] <- graph_from_edgelist(eeu[[i]])
  gkr[[i]] <- graph_from_edgelist(ekr[[i]])
  gbr[[i]] <- graph_from_edgelist(ebr[[i]])
  goc[[i]] <- graph_from_edgelist(eoc[[i]])
  geu1[[i]] <- graph_from_edgelist(eeu1[[i]])
  geu2[[i]] <- graph_from_edgelist(eeu2[[i]])
  gkr1[[i]] <- graph_from_edgelist(ekr1[[i]])
  gkr2[[i]] <- graph_from_edgelist(ekr2[[i]])
  gbr1[[i]] <- graph_from_edgelist(ebr1[[i]])
  gbr2[[i]] <- graph_from_edgelist(ebr2[[i]])
  goc1[[i]] <- graph_from_edgelist(eoc1[[i]])
  goc2[[i]] <- graph_from_edgelist(eoc2[[i]])
}

for (i in 1:199){
gna[[i]] <- graph_from_edgelist(ena[[i]])
gna1[[i]] <- graph_from_edgelist(ena1[[i]])
gna2[[i]] <- graph_from_edgelist(ena2[[i]])
}

outeu <- list()
outeu1 <- list()
outeu2 <- list()
outkr <- list()
outkr1 <- list()
outkr2 <- list()
outna <- list()
outna1 <- list()
outna2 <- list()
outbr <- list()
outbr1 <- list()
outbr2 <- list()
outoc <- list()
outoc1 <- list()
outoc2 <- list()

mouteu1 <- list()
mouteu2 <- list()
moutkr1 <- list()
moutkr2 <- list()
moutbr1 <- list()
moutbr2 <- list()
moutoc1 <- list()
moutoc2 <- list()
moutna1 <- list()
moutna2 <- list()

for (i in 1:200){
  outeu[[i]] <- degree(geu[[i]], mode = "out")
  outkr[[i]] <- degree(gkr[[i]], mode = "out")
  outbr[[i]] <- degree(gbr[[i]], mode = "out")
  outoc[[i]] <- degree(goc[[i]], mode = "out")
  outeu1[[i]] <- mean(outeu[[i]][1:5])
  outeu2[[i]] <- mean(outeu[[i]][6:10])
  outkr1[[i]] <- mean(outkr[[i]][1:5])
  outkr2[[i]] <- mean(outkr[[i]][6:10])
  outbr1[[i]] <- mean(outbr[[i]][1:5])
  outbr2[[i]] <- mean(outbr[[i]][6:10])
  outoc1[[i]] <- mean(outoc[[i]][1:5])
  outoc2[[i]] <- mean(outoc[[i]][6:10])
  mouteu1[[i]] <- outeu1[[i]]/edt1[i,6]
  mouteu2[[i]] <- outeu2[[i]]/edt2[i,6]
  moutkr1[[i]] <- outkr1[[i]]/kdt1[i,6]
  moutkr2[[i]] <- outkr2[[i]]/kdt2[i,6]
  moutbr1[[i]] <- outbr1[[i]]/bdt1[i,6]
  moutbr2[[i]] <- outbr2[[i]]/bdt2[i,6]
  moutoc1[[i]] <- outoc1[[i]]/odt1[i,6]
  moutoc2[[i]] <- outoc2[[i]]/odt2[i,6]
}

for (i in 1:199){
  outna[[i]] <- degree(gna[[i]], mode = "out")
  outna1[[i]] <- mean(outna[[i]][1:5])
  outna2[[i]] <- mean(outna[[i]][6:10])
  moutna1[[i]] <- outna1[[i]]/udt1[i,6]
  moutna2[[i]] <- outna2[[i]]/udt2[i,6]
}

centeu <- list()
centkr <- list()
centbr <- list()
centoc <- list()
centna <- list()

dcenteu <- list()
dcentkr <- list()
dcentbr <- list()
dcentoc <- list()
dcentna <- list()

centeu1 <- list()
centkr1 <- list()
centbr1 <- list()
centoc1 <- list()
centna1 <- list()
centeu2 <- list()
centkr2 <- list()
centbr2 <- list()
centoc2 <- list()
centna2 <- list()

mcenteu1 <- list()
mcentkr1 <- list()
mcentbr1 <- list()
mcentoc1 <- list()
mcentna1 <- list()
mcenteu2 <- list()
mcentkr2 <- list()
mcentbr2 <- list()
mcentoc2 <- list()
mcentna2 <- list()

kmcenteu1 <- list()
kmcentkr1 <- list()
kmcentbr1 <- list()
kmcentoc1 <- list()
kmcentna1 <- list()
kmcenteu2 <- list()
kmcentkr2 <- list()
kmcentbr2 <- list()
kmcentoc2 <- list()
kmcentna2 <- list()

outcenteu1 <- list()
outcenteu2 <- list()
outcentkr1 <- list()
outcentkr2 <- list()
outcentbr1 <- list()
outcentbr2 <- list()
outcentoc1 <- list()
outcentoc2 <- list()
outcentna1 <- list()
outcentna2 <- list()

incenteu1 <- list()
incenteu2 <- list()
incentkr1 <- list()
incentkr2 <- list()
incentbr1 <- list()
incentbr2 <- list()
incentoc1 <- list()
incentoc2 <- list()
incentna1 <- list()
incentna2 <- list()

denseu1 <- list()
denseu2 <- list()
denskr1 <- list()
denskr2 <- list()
densbr1 <- list()
densbr2 <- list()
densoc1 <- list()
densoc2 <- list()
densna1 <- list()
densna2 <- list()

for (i in 1:200){
  centeu[[i]] <- centr_degree(geu[[i]], mode="in")[[1]]
  centkr[[i]] <- centr_degree(gkr[[i]], mode="in")[[1]]
  centbr[[i]] <- centr_degree(gbr[[i]], mode="in")[[1]]
  centoc[[i]] <- centr_degree(goc[[i]], mode="in")[[1]]
  
  incenteu1[[i]] <- centr_degree(geu1[[i]], mode="in")[[2]]
  incenteu2[[i]] <- centr_degree(geu2[[i]], mode="in")[[2]]
  
  incentkr1[[i]] <- centr_degree(gkr1[[i]], mode="in")[[2]]
  incentkr2[[i]] <- centr_degree(gkr2[[i]], mode="in")[[2]]
  
  incentbr1[[i]] <- centr_degree(gbr1[[i]], mode="in")[[2]]
  incentbr2[[i]] <- centr_degree(gbr2[[i]], mode="in")[[2]]
  
  incentoc1[[i]] <- centr_degree(goc1[[i]], mode="in")[[2]]
  incentoc2[[i]] <- centr_degree(goc2[[i]], mode="in")[[2]]
  
  outcenteu1[[i]] <- centr_degree(geu1[[i]], mode="out")[[2]]
  outcenteu2[[i]] <- centr_degree(geu2[[i]], mode="out")[[2]]
  
  outcentkr1[[i]] <- centr_degree(gkr1[[i]], mode="out")[[2]]
  outcentkr2[[i]] <- centr_degree(gkr2[[i]], mode="out")[[2]]
  
  outcentbr1[[i]] <- centr_degree(gbr1[[i]], mode="out")[[2]]
  outcentbr2[[i]] <- centr_degree(gbr2[[i]], mode="out")[[2]]
  
  outcentoc1[[i]] <- centr_degree(goc1[[i]], mode="out")[[2]]
  outcentoc2[[i]] <- centr_degree(goc2[[i]], mode="out")[[2]]
  
  mcenteu1[[i]] <- mean(centeu[[i]][1:5])
  mcenteu2[[i]] <- mean(centeu[[i]][6:10])
  mcentkr1[[i]] <- mean(centkr[[i]][1:5])
  mcentkr2[[i]] <- mean(centkr[[i]][6:10])
  mcentbr1[[i]] <- mean(centbr[[i]][1:5])
  mcentbr2[[i]] <- mean(centbr[[i]][6:10])
  mcentoc1[[i]] <- mean(centoc[[i]][1:5])
  mcentoc2[[i]] <- mean(centoc[[i]][6:10])
  
  kmcenteu1[[i]] <- mcenteu1[[i]]/edt1[i,6]
  kmcenteu2[[i]] <- mcenteu2[[i]]/edt2[i,6]
  kmcentkr1[[i]] <- mcentkr1[[i]]/kdt1[i,6]
  kmcentkr2[[i]] <- mcentkr2[[i]]/kdt2[i,6]
  kmcentbr1[[i]] <- mcentbr1[[i]]/bdt1[i,6]
  kmcentbr2[[i]] <- mcentbr2[[i]]/bdt2[i,6]
  kmcentoc1[[i]] <- mcentoc1[[i]]/odt1[i,6]
  kmcentoc2[[i]] <- mcentoc2[[i]]/odt2[i,6]
  
  denseu1[[i]] <- edge_density(geu1[[i]])
  denseu2[[i]] <- edge_density(geu2[[i]])
  denskr1[[i]] <- edge_density(gkr1[[i]])
  denskr2[[i]] <- edge_density(gkr2[[i]])
  densbr1[[i]] <- edge_density(gbr1[[i]])
  densbr2[[i]] <- edge_density(gbr2[[i]])
  densoc1[[i]] <- edge_density(goc1[[i]])
  densoc2[[i]] <- edge_density(goc2[[i]])
}

for (i in 1:199){
  centna[[i]] <- centr_degree(gna[[i]], mode="in")[[1]]
  
  mcentna1[[i]] <- mean(centna[[i]][1:5])
  mcentna2[[i]] <- mean(centna[[i]][6:10])
  
  kmcentna1[[i]] <- mcentna1[[i]]/udt1[i,6]
  kmcentna2[[i]] <- mcentna2[[i]]/udt2[i,6]
  
  incentna1[[i]] <- centr_degree(gna1[[i]], mode="in")[[2]]
  incentna2[[i]] <- centr_degree(gna2[[i]], mode="in")[[2]]
  
  outcentna1[[i]] <- centr_degree(gna1[[i]], mode="out")[[2]]
  outcentna2[[i]] <- centr_degree(gna2[[i]], mode="out")[[2]]
  
  densna1[[i]] <- edge_density(gna1[[i]])
  densna2[[i]] <- edge_density(gna2[[i]])
}

edt1[,11] <- unlist(outeu1)
edt2[,11] <- unlist(outeu2)
kdt1[,11] <- unlist(outkr1)
kdt2[,11] <- unlist(outkr2)
udt1[,11] <- unlist(outna1)
udt2[,11] <- unlist(outna2)
bdt1[,11] <- unlist(outbr1)
bdt2[,11] <- unlist(outbr2)
odt1[,11] <- unlist(outoc1)
odt2[,11] <- unlist(outoc2)

edt1[,12] <- unlist(mcenteu1)
edt2[,12] <- unlist(mcenteu2)
kdt1[,12] <- unlist(mcentkr1)
kdt2[,12] <- unlist(mcentkr2)
udt1[,12] <- unlist(mcentna1)
udt2[,12] <- unlist(mcentna2)
bdt1[,12] <- unlist(mcentbr1)
bdt2[,12] <- unlist(mcentbr2)
odt1[,12] <- unlist(mcentoc1)
odt2[,12] <- unlist(mcentoc2)

edt1[,13] <- unlist(mouteu1)
edt2[,13] <- unlist(mouteu2)
kdt1[,13] <- unlist(moutkr1)
kdt2[,13] <- unlist(moutkr2)
udt1[,13] <- unlist(moutna1)
udt2[,13] <- unlist(moutna2)
bdt1[,13] <- unlist(moutbr1)
bdt2[,13] <- unlist(moutbr2)
odt1[,13] <- unlist(moutoc1)
odt2[,13] <- unlist(moutoc2)

edt1[,14] <- unlist(kmcenteu1)
edt2[,14] <- unlist(kmcenteu2)
kdt1[,14] <- unlist(kmcentkr1)
kdt2[,14] <- unlist(kmcentkr2)
udt1[,14] <- unlist(kmcentna1)
udt2[,14] <- unlist(kmcentna2)
bdt1[,14] <- unlist(kmcentbr1)
bdt2[,14] <- unlist(kmcentbr2)
odt1[,14] <- unlist(kmcentoc1)
odt2[,14] <- unlist(kmcentoc2)

edt1[,15] <- unlist(incenteu1)
edt2[,15] <- unlist(incenteu2)
kdt1[,15] <- unlist(incentkr1)
kdt2[,15] <- unlist(incentkr2)
udt1[,15] <- unlist(incentna1)
udt2[,15] <- unlist(incentna2)
bdt1[,15] <- unlist(incentbr1)
bdt2[,15] <- unlist(incentbr2)
odt1[,15] <- unlist(incentoc1)
odt2[,15] <- unlist(incentoc2)

edt1[,16] <- unlist(outcenteu1)
edt2[,16] <- unlist(outcenteu2)
kdt1[,16] <- unlist(outcentkr1)
kdt2[,16] <- unlist(outcentkr2)
udt1[,16] <- unlist(outcentna1)
udt2[,16] <- unlist(outcentna2)
bdt1[,16] <- unlist(outcentbr1)
bdt2[,16] <- unlist(outcentbr2)
odt1[,16] <- unlist(outcentoc1)
odt2[,16] <- unlist(outcentoc2)

edt1[,17] <- unlist(denseu1)
edt2[,17] <- unlist(denseu2)
kdt1[,17] <- unlist(denskr1)
kdt2[,17] <- unlist(denskr2)
udt1[,17] <- unlist(densna1)
udt2[,17] <- unlist(densna2)
bdt1[,17] <- unlist(densbr1)
bdt2[,17] <- unlist(densbr2)
odt1[,17] <- unlist(densoc1)
odt2[,17] <- unlist(densoc2)

edt1[61,15] <- NA
edt1[131,15] <- NA
edt1[145,15] <- NA
edt2[61,15] <- NA
edt2[131,15] <- NA
edt2[145,15] <- NA
udt1[128,15] <- NA
udt2[128,15] <- NA

unidt <- rbind(edt1,edt2,udt1,udt2,kdt1,kdt2,bdt1,bdt2,odt1,odt2)
unicol <- c("ID1","ID2","ID3","ID4","ID5","Kills","Solocarry","Duocarry","Win","Region","Avg.Outdegree","Avg.Centralisation",
            "Avg.OutKill","Avg.CentKill","InCent","OutCent","Density")
colnames(unidt) <- unicol

unie <- rbind(edt1,edt2)
unik <- rbind(kdt1,kdt2)
unib <- rbind(bdt1,bdt2)
unio <- rbind(odt1,odt2)
uniu <- rbind(udt1,udt2)

colnames(unie) <- unicol
colnames(unik) <- unicol
colnames(unib) <- unicol
colnames(unio) <- unicol
colnames(uniu) <- unicol

unis <- list(unie,unik,unib,unio,uniu)
lmssolo <- list()
lmsduo <- list()
lmsboth <- list()
lmsdeg <- list()
lmscent <- list()
lmsmcent <- list()
lmskmcent <- list()
lmsinoutcent <- list()
lmsdens <- list()
lmsdensw <- list()
lmscentw <- list()
lmsdenscent <- list()
lmsinter <- list()

for (i in 1:5){
  lmssolo[[i]] <- summary(lm(Kills~Solocarry, unis[[i]]))
  lmsduo[[i]] <- summary(lm(Kills~Duocarry, unis[[i]]))
  lmsboth[[i]] <- summary(lm(Kills~Solocarry+Duocarry, unis[[i]]))
  lmsdeg[[i]] <- summary(lm(Kills~Avg.Outdegree, unis[[i]]))
  lmscent[[i]] <- summary(lm(Kills~Avg.Centralisation, unis[[i]]))
  lmsmcent[[i]] <- summary(lm(Kills~Avg.OutKill, unis[[i]]))
  lmskmcent[[i]] <- summary(lm(Kills~Avg.CentKill, unis[[i]]))
  lmsinoutcent[[i]] <- summary(lm(Kills~InCent + OutCent, unis[[i]]))
  lmsdens[[i]] <- summary(lm(Kills~Density, unis[[i]]))
  lmsdensw[[i]] <- summary(lm(Win~Density, unis[[i]]))
  #lmscentw[[i]] <- summary(lm(Win~TeamCent, unis[[i]]))
  lmsdenscent[[i]] <- summary(lm(Kills~Density + InCent + OutCent, unis[[i]]))
  lmsinter[[i]] <- summary(lm(Kills~Density + InCent + OutCent + Density:InCent + Density:OutCent, unis[[i]]))
}

#summary(lm(Kills~Solocarry, unidt))
#summary(lm(Kills~Duocarry, unidt))
#summary(lm(Kills~Duocarry + Region:Duocarry, unidt))
#summary(lm(Kills~Solocarry+Duocarry, unidt))
#summary(lm(Kills~Solocarry + Region:Solocarry, unidt))
summary(lm(Kills~OutCent, unidt))
summary(lm(Kills~ Density + OutCent + OutCent:Region, unidt))
summary(lm(Kills~Density, unidt))
summary(lm(Kills~OutCent + Density + Density:Region, unidt))
#summary(lm(Kills~Avg.Outdegree + Avg.Outdegree:Region,unidt))
#summary(lm(Win~TeamCent, unidt))
#summary(lm(Win~TeamCent + TeamCent:Region, unidt))
#summary(lm(Win~Density, unidt))
#summary(lm(Win~Density + Density:Region, unidt))

#summary(lmer(Kills~1+Solocarry+(1|Region),unidt))
#summary(lmer(Kills~1+Duocarry+(1|Region),unidt))
#summary(lmer(Kills~1+Solocarry+Duocarry+(1|Region),unidt))
#summary(lmer(Kills~1+Solocarry+(1|Region)+Solocarry:Region,unidt))
#summary(lmer(Kills~1+Duocarry+(1|Region)+Duocarry:Region,unidt))
#summary(lmer(Kills~1+Avg.Outdegree + (1|Region) + Avg.Outdegree:Region ,unidt))
summary(lmer(Kills~1+OutCent + InCent + (1|Region),unidt))
summary(lmer(Kills~1+Density + (1|Region),unidt))
summary(lmer(Kills~1+Density + InCent + OutCent + (1|Region), unidt))
summary(lmer(Kills~1+Density + InCent + OutCent + Density:InCent + Density:OutCent + (1|Region), unidt))
x <- goc1[[152]]
E(x)$weight <- length(E(x))

killden <- density(unidt[,6], na.rm = T)
killdistp <- plot(killden, type = "l", main = "Distribution of kills", xlab = "Kills", ylab = "Distribution", lwd = 2, col = "light blue")

denden <- density(unidt[,17], na.rm = T)
dendistp <- plot(denden, type = "l", main = "Distribution of density", xlab = "Density", ylab = "Distribution", lwd = 2, col = "light blue")

inden <- density(unidt[,15], na.rm = T)
indistp <- plot(inden, type = "l", main = "Distribution of indegree centrality", xlab = "Indegree centrality", ylab = "Distribution", lwd = 2, col = "light blue")

outden <- density(unidt[,16], na.rm = T)
outdistp <- plot(outden, type = "l", main = "Distribution of outdegree centrality", xlab = "Outdegree centrality", ylab = "Distribution", lwd = 2, col = "light blue")
