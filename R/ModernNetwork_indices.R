###Recreating Modern web network indices

filewebs = list.files(path="all", pattern="*.csv", full.names=T)
filewebs2 = list.files(path="all", pattern="*.csv", full.names=F)

networklev=NULL
higherlev=NULL
plantlev=NULL

for (i in 37:length(filewebs)){
  nam=paste(filewebs[i])
  netcurr=read.csv(nam, header=F)
  colnames(netcurr)=c("from", "to", "val")
  net=spread(netcurr, to, val)
  net=net[,-1]
  nam2=strsplit(filewebs2[i],".csv")[[1]]
  #plotweb(net)
  networklev=rbind(networklev,c(nam2,networklevel(net)))
  X=specieslevel(net)
  X1=X$`higher level`
  X2=X$`lower level`
  
  X1$web=nam2
  X2$web=nam2
  
  higherlev=rbind(higherlev,X1)
  plantlev=rbind(plantlev,X2)
  print(i)
}
  
  plantnam=NULL
  highernam=NULL
  for (i in 1:length(filewebs)){
    nam=paste(filewebs[i])
    netcurr=read.csv(nam, header=F)
    colnames(netcurr)=c("from", "to", "val")
    net=spread(netcurr, to, val)
    net=net[,-1]
    nam2=strsplit(filewebs2[i],".csv")[[1]]
    plantnam=c(plantnam,rownames(net))
    highernam=c(highernam,colnames(net))
  }
  
  plantlev$speciesID=plantnam
  higherlev$speciesID=highernam
  
  
  #write.csv(networklev, "networklevelmodern.csv")
  #write.csv(higherlev,"higherlevel.csv")
  #write.csv(plantlev, "plantlevel.csv")
  