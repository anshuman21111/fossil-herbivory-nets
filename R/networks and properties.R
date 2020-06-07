
#making the networks
library("bipartite")

WD=read.csv(file = "~/Documents/Research/Manuscripts/Fossil herbivory/data/extended/WD.csv", header = TRUE)
#WD=read.csv(file = "~/Downloads/Williamson Drive_GMedit.csv", header = TRUE)
WDmat2=WD %>% group_by(Species) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
WDmat = data.matrix(WDmat2[,-c(1)], rownames.force = NA)
rownames(WDmat)= WDmat2$Species

colnam=rgb(1,0.5,0.5, 0.75)
colnam2=rgb(75/256,155/256,79/256, 1)
colnam3=rgb(85/256,85/256,85/256, 1)

plotweb(WDmat, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = NULL, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Species)/sum(table(WD$Species))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

WDmat4=WD %>% group_by(Clade) %>%  summarise_if(is.numeric,  sum, na.rm = TRUE)
WDmat3 = data.matrix(WDmat4[,-c(1)], rownames.force = NA)
rownames(WDmat3)= WDmat4$Clade

colnam=rgb(1,0.5,0.5, 0.75)
colnam2=rgb(75/256,155/256,79/256, 1)
colnam3=rgb(85/256,85/256,85/256, 1)

plotweb(WDmat3, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = 0.105, high.spacing = 0.013, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Clade)/sum(table(WD$Clade))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.05, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

SAPmat2=SAP %>% group_by(Species) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
SAPmat = data.matrix(SAPmat2[,-c(1)], rownames.force = NA)
rownames(SAPmat)= SAPmat2$Species

SAPmat4=SAP %>% group_by(Clade) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
SAPmat3 = data.matrix(SAPmat4[,-c(1)], rownames.force = NA)
rownames(SAPmat3)= SAPmat4$Clade

colnam=rgb(246/256,205/256,68/256, 0.75) #change this
colnam2=rgb(75/256,155/256,79/256, 1)
colnam3=rgb(85/256,85/256,85/256, 1)

plotweb(SAPmat, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.01, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Species)/sum(table(WD$Species))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.07, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

plotweb(SAPmat3, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.01, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Clade)/sum(table(WD$Clade))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.05, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

MCFmat2=MCF %>% group_by(Species) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
MCFmat = data.matrix(MCFmat2[,-c(1:3)], rownames.force = NA)
rownames(MCFmat)= MCFmat2$Species

MCFmat4=MCF %>% group_by(Clade) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
MCFmat3 = data.matrix(MCFmat4[,-c(1)], rownames.force = NA)
rownames(MCFmat3)= MCFmat4$Clade

colnam=rgb(210/256,158/256,210/256, 0.70) #change this
colnam2=rgb(75/256,155/256,79/256, 1)
colnam3=rgb(85/256,85/256,85/256, 1)

plotweb(MCFmat, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.01, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Species)/sum(table(WD$Species))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.035, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

plotweb(MCFmat3, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.01, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Clade)/sum(table(WD$Clade))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.80), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.07, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

CCPmat2=CCP %>% group_by(Species) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
CCPmat = data.matrix(CCPmat2[,-c(1)], rownames.force = NA)
rownames(CCPmat)= CCPmat2$Species

CCPmat4= CCP %>% group_by(Clade) %>%  summarise_if(is.numeric, sum, na.rm = TRUE)
CCPmat3 = data.matrix(CCPmat4[,-c(1)], rownames.force = NA)
rownames(CCPmat3)= CCPmat4$Clade

colnam=rgb(152/256,219/256,232/256, 0.70) #change this
colnam2=rgb(75/256,155/256,79/256, 1)
colnam3=rgb(85/256,85/256,85/256, 1)

plotweb(CCPmat, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.017, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Species)/sum(table(WD$Species))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.85), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.25, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

plotweb(CCPmat3, method = "normal", empty = TRUE, labsize = 1.7, ybig = 1, y.width.low = 0.1,y.width.high = 0.1, low.spacing = NULL, high.spacing = 0.015, arrow="no", col.interaction= colnam, bor.col.interaction =colnam, bor.col.high=colnam3, bor.col.low=colnam2, high.lablength = NULL, low.lablength = NULL, sequence= NULL, low.abun=(table(WD$Clade)/sum(table(WD$Clade))), low.abun.col=colnam2, bor.low.abun.col =colnam2, high.abun=((colSums(WD[,4:74]))/sum(colSums(WD[,4:74]))), high.abun.col=colnam3, bor.high.abun.col=colnam3, text.rot=90, text.high.col=colnam3, text.low.col=colnam2, adj.high=NULL, adj.low=NULL, plot.axes = FALSE, low.y=0.5, high.y=1.5, add=FALSE, y.lim=c(-0.95,1.85), x.lim=NULL, low.plot=TRUE, high.plot=TRUE, high.xoff = 0, low.xoff = 0.27, high.lab.dis = NULL, low.lab.dis = NULL, abuns.type="none", col.low=colnam2, col.high = colnam3)

#specieslevel net properties
SAPall=networklevel(SAPmat, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

MCFall=networklevel(MCFmat, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

CCPall=networklevel(CCPmat, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

WDall=networklevel(WDmat, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)


netall=rbind(SAPall, MCFall, CCPall, WDall)
rownames(netall)=c("SAP", "MCF", "CCP", "WD")

#cladelevelnet properties
SAPall3=networklevel(SAPmat3, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

MCFall3=networklevel(MCFmat3, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

CCPall3=networklevel(CCPmat3, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)

WDall3=networklevel(WDmat3, index="ALLBUTDD", level="both", weighted=TRUE,ISAmethod="Bluethgen", SAmethod = "Bluethgen", extinctmethod = "r", nrep = 100, CCfun=median, dist="horn", normalise=TRUE, empty.web=TRUE,logbase="e", intereven="prod", H2_integer=FALSE, fcweighted=TRUE,fcdist="euclidean", legacy=FALSE)


netall_clade=rbind(SAPall3, MCFall3, CCPall3, WDall3)
rownames(netall_clade)=c("SAP", "MCF", "CCP", "WD")


#specieslevel
SAPsp=specieslevel(SAPmat)
MCFsp=specieslevel(MCFmat)
CCPsp=specieslevel(CCPmat)
WDsp=specieslevel(WDmat)

#cladelevel
SAPsp3=specieslevel(SAPmat3)
MCFsp3=specieslevel(MCFmat3)
CCPsp3=specieslevel(CCPmat3)
WDsp3=specieslevel(WDmat3)

#specieslevel
A=rownames(SAPsp$`higher level`)
A2=cbind(A, SAPsp$`higher level`)
A=rownames(MCFsp$`higher level`)
B2=cbind(A, MCFsp$`higher level`)
A=rownames(CCPsp$`higher level`)
C2=cbind(A, CCPsp$`higher level`)
A=rownames(WDsp$`higher level`)
D2=cbind(A, WDsp$`higher level`)

HLdata=rbind(A2, B2, C2, D2)
rownames(HLdata)=NULL
sitenam1= rep(c("SAP", "MCF", "CCP", "WD"), times=c(nrow(SAPsp$`higher level`), nrow(MCFsp$`higher level`), nrow(CCPsp$`higher level`), nrow(WDsp$`higher level`)))
HLdata=cbind(sitenam1, HLdata)

#cladelevel
A=rownames(SAPsp3$`higher level`)
A2=cbind(A, SAPsp3$`higher level`)
A=rownames(MCFsp3$`higher level`)
B2=cbind(A, MCFsp3$`higher level`)
A=rownames(CCPsp3$`higher level`)
C2=cbind(A, CCPsp3$`higher level`)
A=rownames(WDsp3$`higher level`)
D2=cbind(A, WDsp3$`higher level`)

HLdata_clade=rbind(A2, B2, C2, D2)
rownames(HLdata_clade)=NULL
sitenam1a= rep(c("SAP", "MCF", "CCP", "WD"), times=c(nrow(SAPsp3$`higher level`), nrow(MCFsp3$`higher level`), nrow(CCPsp3$`higher level`), nrow(WDsp3$`higher level`)))
HLdata_clade=cbind(sitenam1a, HLdata_clade)

#specieslevel
A=rownames(SAPsp$`lower level`)
A2=cbind(A, SAPsp$`lower level`)
A=rownames(MCFsp$`lower level`)
B2=cbind(A, MCFsp$`lower level`)
A=rownames(CCPsp$`lower level`)
C2=cbind(A, CCPsp$`lower level`)
A=rownames(WDsp$`lower level`)
D2=cbind(A, WDsp$`lower level`)

LLdata=rbind(A2, B2, C2, D2)
rownames(LLdata)=NULL
sitenam2= rep(c("SAP", "MCF", "CCP", "WD"), times=c(nrow(SAPsp$`lower level`), nrow(MCFsp$`lower level`), nrow(CCPsp$`lower level`), nrow(WDsp$`lower level`)))
LLdata=cbind(sitenam2, LLdata)

boxplot(LLdata$proportional.generality~LLdata$sitenam2)

#cladelevel
A=rownames(SAPsp3$`lower level`)
A2=cbind(A, SAPsp3$`lower level`)
A=rownames(MCFsp3$`lower level`)
B2=cbind(A, MCFsp3$`lower level`)
A=rownames(CCPsp3$`lower level`)
C2=cbind(A, CCPsp3$`lower level`)
A=rownames(WDsp3$`lower level`)
D2=cbind(A, WDsp3$`lower level`)

LLdata_clade=rbind(A2, B2, C2, D2)
rownames(LLdata_clade)=NULL
sitenam2a= rep(c("SAP", "MCF", "CCP", "WD"), times=c(nrow(SAPsp3$`lower level`), nrow(MCFsp3$`lower level`), nrow(CCPsp3$`lower level`), nrow(WDsp3$`lower level`)))
LLdata_clade=cbind(sitenam2a, LLdata_clade)

boxplot(LLdata_clade$proportional.generality~LLdata_clade$sitenam2a)