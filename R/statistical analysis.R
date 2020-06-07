#PCA
library(factoextra)
res.pca <- prcomp(HLdata[,c(3,5,15,18,19,20,21,22)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(HLdata$sitenam1)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080")),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Sites",
             repel = TRUE
)

res.pca <- prcomp(HLdata_clade[,c(3,5,15,18,19,20,21,22)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(HLdata_clade$sitenam1a)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080")),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Sites",
             repel = TRUE
)

res.pca <- prcomp(LLdata[,c(3,5,15,18,19,20,21,22)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(LLdata$sitenam2)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080")),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Sites",
             repel = TRUE
)

res.pca <- prcomp(LLdata_clade[,c(3,5,15,18,19,20,21,22)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(LLdata_clade$sitenam2a)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             palette = as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080")),
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Sites",
             repel = TRUE
)

newHLdata=read.csv("~/Downloads/HLdata.csv", header = T)
newHLdata=newHLdata[,-1]
#library(factoextra)
res.pca <- prcomp(newHLdata[,c(4,6,16,19,20,21,22,23)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(newHLdata$type)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Type",
             repel = TRUE
)

LLnew=read.csv("~/Downloads/LLnew.csv")
res.pca <- prcomp(LLdata[,c(3,5,15,18,19,20,21,22)], scale = TRUE)
fviz_eig(res.pca)

fviz_pca_var(res.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(res.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

groups <- as.factor(LLnew$clade)
fviz_pca_ind(res.pca,
             col.ind = groups, # color by groups
             addEllipses = TRUE, # Concentration ellipses
             ellipse.type = "confidence",
             legend.title = "Sites",
             repel = TRUE
)


#NMDS - extra
library(vegan)
nmds <- metaMDS(HLdata[,-c(1,2,3,17,4,6)])
HLdata1=HLdata


library(ggplot2)

#scale_color_manual(name="sitenam1", values= c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))

boolColors <- as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))
boolScale <- scale_colour_manual(name="sitenam1", values=boolColors)

scores(nmds) %>%
  cbind(HLdata1) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) + boolScale+
  geom_point(aes(size = degree, color = sitenam1)) +
  stat_ellipse(geom = "polygon", aes(group = sitenam1, color = sitenam1), alpha = 0.04) +
  annotate("text", x = -1.2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0) + 
  theme_bw()

nmds <- metaMDS(HLdata_clade[,c(3,5,15,16,18,19,20,21,22)])
HLdata1=HLdata_clade


#nmds <- metaMDS(HLdata_clade[,-c(1,2,3,17)])
#HLdata1=HLdata_clade


library(ggplot2)

#scale_color_manual(name="sitenam1", values= c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))

boolColors <- as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))
boolScale <- scale_colour_manual(name="sitenam1a", values=boolColors)

scores(nmds) %>%
  cbind(HLdata1) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) + boolScale +
  geom_point(aes(size = degree, color = sitenam1a)) +
  stat_ellipse(geom = "polygon", aes(group = sitenam1a, color = sitenam1a), alpha = 0.04) +
  annotate("text", x = -1.2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0) + 
  theme_bw()

nmds <- metaMDS(HLdata[,c(3,5,15,16,18,19,20,21,22)])
HLdata1=HLdata

library(ggplot2)
scores(nmds) %>%
  cbind(HLdata1) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) +
  geom_point(aes(size = degree, color = sitenam1)) +
  stat_ellipse(geom = "polygon", aes(group = sitenam1, color = sitenam1, fill = sitenam1), alpha = 0.3) +
  annotate("text", x = -2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0) +
  theme_bw()

nmds <- metaMDS(LLdata[,c(3,5,15,16, 17,18,19,20,21,22)])
LLdata1=LLdata


#nmds <- metaMDS(HLdata[,-c(1,2,3,17)])
#HLdata1=HLdata


#library(ggplot2)

#scale_color_manual(name="sitenam1", values= c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))

boolColors <- as.character(c("CCP" = "#98DBE8", "MCF" = "#D29ED2", "SAP"="#F6CD44", "WD"= "#FF8080"))
boolScale <- scale_colour_manual(name="sitenam2", values=boolColors)

scores(nmds) %>%
  cbind(LLdata1) %>%
  ggplot(aes(x = NMDS1, y = NMDS2)) + boolScale+
  geom_point(aes(size = degree, color = sitenam2)) +
  annotate("text", x = -1.2, y = 0.95, label = paste0("stress: ", format(nmds$stress, digits = 4)), hjust = 0) + 
  theme_bw()

