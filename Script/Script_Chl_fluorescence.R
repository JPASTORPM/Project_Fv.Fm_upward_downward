#------------------------------------------------
# Script: Fv/Fm upward and downward
# Autor: Junior Pastor Pérez-Molina
# Date: 04-13-2019
#------------------------------------------------



#-----------------------------------------------
# Initial steps
#------------------------------------------------
rm(list = ls()) # Remove all objects
graphics.off()  # Remove all graphics
cat("\014")     # Remove script in console
#------------------------------------------------


#------------------------------------------------
# Loading database
#------------------------------------------------
abs<-read.delim("Data/Data_absorvancia.txt",header=T,sep="\t",dec=".")
str(abs)
abs_470<-abs[abs$wavelength==470.4,]
abs_663.2<-abs[abs$wavelength==662.9,]
abs_646.8<-abs[abs$wavelength==646.6,]
ab<-merge(abs_470, abs_663.2, all=TRUE)
abs<-merge(ab, abs_646.8, all=TRUE)
abs
FvFm<-read.delim("Data/.txt",header=T,sep="\t",dec=".")
str(abs)

#------------------------------------------------















