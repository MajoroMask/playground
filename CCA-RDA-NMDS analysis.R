library(vegan)
data(varespec)
data(varechem)
## Common but bad way: use all variables you happen to have in your
## environmental data matrix
decorana(varespec) #DCA(Detrended correspondence analysis)

## CCA
vare.cca <- cca(varespec, varechem)
vare.cca
plot(vare.cca,xlab=paste("CCA1 (",round(vare.cca$CCA$eig[1]*100,2),"%)",sep=""),
          ylab=paste("CCA2 (",round(vare.cca$CCA$eig[2]*100,2),"%)",sep=""))
permutest(vare.cca,permu=999)
ef=envfit(vare.cca,varechem,permu=999)
ef
## Formula interface and a better model
vare.cca <- cca(varespec ~ Al + P*(K + Baresoil), data=varechem)
vare.cca
plot(vare.cca) # display="si"
## `Partialling out' and `negative components of variance'
cca(varespec ~ Ca, varechem)
cca(varespec ~ Ca + Condition(pH), varechem)


## RDA
data(dune)
data(dune.env)
dune.Manure <- rda(dune ~ Manure, dune.env)
plot(dune.Manure,xlab=paste("RDA1 (",round(dune.Manure$CCA$eig[1],2),"%)",sep=""),
     ylab=paste("RDA2 (",round(dune.Manure$CCA$eig[2],2),"%)",sep="")) 
# display=c("sp","si","bp")
plot(dune.Manure,xlab=paste("RDA1 (",round(dune.Manure$CCA$eig[1],2),"%)",sep=""),
     ylab=paste("RDA2 (",round(dune.Manure$CCA$eig[2],2),"%)",sep=""),display = c("sp","bp")) 

plot(dune.Manure,xlab=paste("RDA1 (",round(dune.Manure$CCA$eig[1],2),"%)",sep=""),
     ylab=paste("RDA2 (",round(dune.Manure$CCA$eig[2],2),"%)",sep=""),display = "sp") 

plot(dune.Manure,xlab=paste("RDA1 (",round(dune.Manure$CCA$eig[1],2),"%)",sep=""),
     ylab=paste("RDA2 (",round(dune.Manure$CCA$eig[2],2),"%)",sep=""),display = "si") 

## PCA
data(dune)
data(dune.env)
dune.pca <- rda(dune,scale = F)
biplot(dune.pca,xlab=paste("PC1 (",round(dune.pca$CA$eig[1],2),"%)",sep=""),
       ylab=paste("PC2 (",round(dune.pca$CA$eig[2],2),"%)",sep=""))
plot(dune.pca,display = "sp")
plot(dune.pca,display = "si")

## NMDS
data(varespec)
# monoMDS
vare.dis <- vegdist(varespec)
vare.mds0 <- monoMDS(vare.dis)
stressplot(vare.mds0, vare.dis)
ordiplot(vare.mds0, type = "t")
# metaMDS
vare.mds<-metaMDS(varespec, trace=F)
plot(vare.mds, type="t")

data(dune)
# Global NMDS using monoMDS
sol <- metaMDS(dune)
sol
label<-colnames(dune)
plot(sol$points, type="b")
text(sol$points, labels=label)
#plot(sol, display = "sites", choices = c(1, 2), type = "t") 
## Start from previous best solution
sol <- metaMDS(dune, previous.best = sol)
## Local NMDS and stress 2 of monoMDS
sol2 <- metaMDS(dune, model = "local", stress=2)
sol2
## Use Arrhenius exponent 'z' as a binary dissimilarity measure
sol <- metaMDS(dune, distfun = betadiver, distance = "z")
sol
