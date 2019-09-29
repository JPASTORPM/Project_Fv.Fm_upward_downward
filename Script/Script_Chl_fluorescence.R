#------------------------------------------------
# Script: Fv/Fm upward and downward
# Autor: Junior Pastor Pérez-Molina
# Date: 09-29-2019
#------------------------------------------------



#-----------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
#------------------------------------------------



#------------------------------------------------
# Packages
#------------------------------------------------
library("readxl")
library(openxlsx)
library(Rmisc)
library(fields) #https://www.rdocumentation.org/packages/fields/versions/9.8-6/topics/interp.surface
library(plot3D) #https://rpubs.com/yoshio/95844
library(yarrr)
#------------------------------------------------



#------------------------------------------------
# Loading database
#------------------------------------------------
abs<-read.delim("Data/Data_absorvancia.txt",header=T,sep="\t",dec=".")
abs_470<-abs[abs$wavelength==470.4,]
abs_663.2<-abs[abs$wavelength==662.9,]
abs_646.8<-abs[abs$wavelength==646.6,]
ab<-merge(abs_470, abs_663.2, all=TRUE)
abs<-merge(ab, abs_646.8, all=TRUE)
abs<-data.frame(t(abs))
names(abs)<-c(abs[1,])
abs<-abs[-1,]
abs$sp<-row.names(abs)

FvFm<-read_excel("Data/Bioesta.xlsx", sheet = "Database")
FvFm<-data.frame(FvFm)
str(FvFm)
masa<-data.frame(FvFm$nombre_cientifico, FvFm$n_serie_absorvancia, FvFm$masa_hoja_chl_g)
masa<-na.omit(masa)
masa<-masa[c(-2,-4,-6,-8,-10,-12,-14,-16,-18,-20,-22),]
names(masa)<-c("cod_sp","n_serie_absorvancia", "masa_g")

pigments<-data.frame(masa, abs)
pigments$Chl_a_ug_g<-(12.25*pigments$X662.9-2.79*pigments$X646.6)*(5/pigments$masa_g)
pigments$Chl_b_ug_g<-(21.5*pigments$X646.6-5.10*pigments$X662.9)*(5/pigments$masa_g)
pigments$Chl_xc_ug_g<-((1000*pigments$X470.4-1.82*pigments$Chl_a_ug_g-85.02*pigments$Chl_b_ug_g)/198)*(5/pigments$masa_g)
pigments$Chl_xc_ug_g[pigments$Chl_xc_ug_g<0]<-0

write.xlsx(pigments, "Data/Pigments.xlsx",
           sheetName="pigments",col.names=TRUE,
           row.names=FALSE, append=FALSE,
           showNA=TRUE, password=NULL)

FvFm$FvFm_index<-(FvFm$Fv.Fm_haz-FvFm$Fv.Fm_enves)
FvFm2<-FvFm[FvFm$Fv.Fm_haz>=0.72, ] # Mayor a 90% de Fv/Fmm, donde 0.8 es 100%, 0.72/0.8= 90%
plot(FvFm2[,c(-1:-7, -16:-21)])
#------------------------------------------------



#------------------------------------------------
# Fig. Spearman - Correlations.
#------------------------------------------------
str(FvFm2)
d <- FvFm2[, c(22,8,9,10,11,12,13,14,15)]
d <- na.omit(d)
hc <- hclust(as.dist(1-cor(d, method='spearman', use='pairwise.complete.obs')))
#hc.order <- order.dendrogram(as.dendrogram(hc))
#d <- d[ ,hc]#d[ ,hc.order]
gr <- as.factor(FvFm2$tipo_hoja)

cols.key <- scales::muted(c('black', 'black', 'black'))
cols.key <- adjustcolor(cols.key, alpha.f=1)
pchs.key <- c(19,15,17)

panel.hist <- function(x, ...) {
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(usr[1:2], 0, 1.5))
  h <- hist(x, plot=FALSE)
  breaks <- h$breaks
  nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col='gray', ...)
}
panel.cor <- function(x, y, ...){
  usr <- par('usr'); on.exit(par(usr))
  par(usr=c(0,1,0,1))
  r <- cor(x, y, method='spearman', use='pairwise.complete.obs')
  zcol <- lattice::level.colors(r, at=seq(-1, 1, length=81), col.regions=colorRampPalette(c(scales::muted('red'),'white',scales::muted('blue')), space='rgb')(81))
  ell <- ellipse::ellipse(r, level=0.95, type='l', npoints=50, scale=c(.2, .2), centre=c(.5, .5))
  polygon(ell, col=zcol, border=zcol, ...)
  text(x=.5, y=.5, lab=100*round(r, 2), cex=2, col='black')
  # pval <- cor.test(x, y, method='spearman', use='pairwise.complete.obs')$p.value
  # sig <- symnum(pval, corr=FALSE, na=FALSE, cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1), symbols = c('***', '**', '*', '.', ' '))
  # text(.6, .8, sig, cex=2, col='gray20')
}
panel.scatter <- function(x, y){
  points(x, y, col=cols.key[gr], pch=pchs.key[gr], cex=1.15)
  lines(lowess(x, y))
}

#pdf(file = "Results/Fig. Spearman correlations.pdf", width = 4.5*0.95, height = 4.5*0.95)
pairs(d,
      diag.panel=panel.hist,
      lower.panel=panel.scatter,
      upper.panel=panel.cor,
      gap=0.5,
      labels=gsub('\\.', '\n', colnames(d)),
      label.pos=0.7,
      cex.labels=1.4
)
#dev.off()
#------------------------------------------------











