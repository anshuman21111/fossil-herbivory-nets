##data input and processing into bipartite adjacency matrix
library(dplyr)

SAP=read.csv(file = "~/Documents/Research/Manuscripts/Fossil herbivory/data/extended/SAP.csv", header = TRUE)

SAPmat2=SAP %>% group_by(Species) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
SAPmat = data.matrix(SAPmat2[,-c(1)], rownames.force = NA)
rownames(SAPmat)= SAPmat2$Species

SAPmat4=SAP %>% group_by(Clade) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
SAPmat3 = data.matrix(SAPmat4[,-c(1)], rownames.force = NA)
rownames(SAPmat3)= SAPmat4$Clade


MCF=read.csv(file = "~/Documents/Research/Manuscripts/Fossil herbivory/data/extended/MCF.csv", header = TRUE)

MCFmat2=MCF %>% group_by(Species) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
MCFmat = data.matrix(MCFmat2[,-c(1:3)], rownames.force = NA)
rownames(MCFmat)= MCFmat2$Species

MCFmat4=MCF %>% group_by(Clade) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
MCFmat3 = data.matrix(MCFmat4[,-c(1)], rownames.force = NA)
rownames(MCFmat3)= MCFmat4$Clade

CCP=read.csv(file = "~/Documents/Research/Manuscripts/Fossil herbivory/data/extended/CCP.csv", header = TRUE)

CCPmat2=CCP %>% group_by(Species) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
CCPmat = data.matrix(CCPmat2[,-c(1)], rownames.force = NA)
rownames(CCPmat)= CCPmat2$Species

CCPmat4= CCP %>% group_by(Clade) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
CCPmat3 = data.matrix(CCPmat4[,-c(1)], rownames.force = NA)
rownames(CCPmat3)= CCPmat4$Clade

WD=read.csv(file = "~/Documents/Research/Manuscripts/Fossil herbivory/data/extended/WD.csv", header = TRUE)
WDmat2=WD %>% group_by(Species) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
WDmat = data.matrix(WDmat2[,-c(1)], rownames.force = NA)
rownames(WDmat)= WDmat2$Species

WDmat4=WD %>% group_by(Clade) %>%  summarise_if(is.numeric, mean, na.rm = TRUE)
WDmat3 = data.matrix(WDmat4[,-c(1)], rownames.force = NA)
rownames(WDmat3)= WDmat4$Clade