#Aim : to merge 2 files with inconsistent time series. File 1 = Reference (contains the time series desired at the end, and a virtual data set (1 to n),
#file 2 = Imported, where the key data will be interpolated, the extracted for the time specified in file 1
#The problem revolves two sets of data with different time steps. Imported is less frequent than Reference
#Create data set in excel : NB : the end of column Import_datetime should be filled with incremental semih (virtual), not with NA. Avoid using # in column title.
#Column 'Ref_Value' is just a line number, but it is necessary for the process

#'data.frame':   6349 obs. of  15 variables:
# $ Reference_datetime     : Factor w/ 6349 levels "01/09/2013 00:00",..: 3841 3842 3843 3844 3845 3846 3847 3848 3849 3850 ...
#$ Ref_Value              : int  1 2 3 4 5 6 7 8 9 10 ...
#$ Import_datetime        : Factor w/ 886 levels "04/09/2013 13:53",..: 359 360 361 362 363 364 365 366 367 368 ...
#$ Tsoil_surf             : num  21.2 21 20.9 20.9 20.9 ...
# $ CO2_amb                : num  401 425 416 419 428 ...
# $ Tsoil_10cm             : num  21.3 21.3 21.3 21.4 21.4 ...
# $ Rsoil_quadratic        : num  4.18 4.46 3.42 4.42 3.77 ...
# $ Rsoil_quadratic_IC95inf: num  4.02 4.31 3.3 4.26 3.63 ...
# $ Rsoil_quadratic_IC95sup: num  4.34 4.62 3.53 4.58 3.9 ...
# $ Rsoil_linear           : num  3.45 3.54 3.28 3.51 3.4 ...
# $ Rsoil_linear_IC95inf   : num  3.3 3.36 3.21 3.34 3.31 ...
# $ Rsoil_linear_IC95sup   : num  3.6 3.72 3.35 3.69 3.5 ...                             
# $ STD_slope              : num  2.1 2.23 1.62 2.04 1.89 ...
# $ delta.T                : num  -0.08 -0.27 -0.36 -0.48 -0.56 -0.64 -0.73 -0.81 -1.02 -1.18 ...


#save as .csv file (DOS.csv file) with coma sep
#Import .csv file with header, Replace "\""/" by "/" in pathway

#Address old: 
#data<-read.table("D:/Mes donnees/1MainData/Modèles/Formation R novembre 2012/R interpolation/datainterpolRsoil.csv", header=T, sep=",")

#Address new:
data<-read.table("C:/Users/JuniorPastor/Dropbox/Master Junior, Karel, Olivier/Datos/Articulo 1_Rsoil/Rsoil Chamber Robot/Datos/Interpolated Rsoil Semih data/Interpolated_Modificado_JP/datainterpolRsoil.txt", header=T, sep="\t")

# To allow calling the columns afterwards :
attach(data)

#characteristics : to diaply the imported data : check they are OK !
str(data)

#Reference is a double vector extracted from 'data'
Reference <- data.frame(Reference_datetime,Ref_Value)

#'Reference$Reference_datetime' is the 'Reference_datetime' column, extracted from 'Reference'
#function 'as.POSIXct' is to convert date and time from string at the right format, to actual data, 
Reference$Reference_datetime <- as.POSIXct(Reference$Reference_datetime, format="%d/%m/%Y %H:%M")

Import<-data.frame(Import_datetime,Tsoil_surf,CO2_amb,Tsoil_10cm,Rsoil_quadratic,Rsoil_quadratic_IC95inf,Rsoil_quadratic_IC95sup,
Rsoil_linear,Rsoil_linear_IC95inf,Rsoil_linear_IC95sup,STD_slope,delta.T)

Import$Import_datetime <- as.POSIXct(Import$Import_datetime, format="%d/%m/%Y %H:%M")

library(zoo)
# Create zoo objects
zR <- zoo(Reference$Ref_Value, Reference$Reference_datetime)  # high freq
#zR

zI_Tsoil_surf <- zoo(Import$Tsoil_surf, Import$Import_datetime)    # low freq
#zI_Tsoil_surf
zI_CO2_amb <- zoo(Import$CO2_amb, Import$Import_datetime)    # low freq
#zI_CO2_amb
zI_Tsoil_10cm <- zoo(Import$Tsoil_10cm, Import$Import_datetime)    # low freq
#zI_Tsoil_10cm
zI_Rsoil_quadratic <- zoo(Import$Rsoil_quadratic, Import$Import_datetime)    # low freq
#zI_Rsoil_quadratic
zI_Rsoil_quadratic_IC95inf <- zoo(Import$Rsoil_quadratic_IC95inf, Import$Import_datetime)    # low freq
#zI_Rsoil_quadratic_IC95inf
zI_Rsoil_quadratic_IC95sup <- zoo(Import$Rsoil_quadratic_IC95sup, Import$Import_datetime)    # low freq
#zI_Rsoil_quadratic_IC95sup
zI_Rsoil_linear <- zoo(Import$Rsoil_linear, Import$Import_datetime)    # low freq
#zI_Rsoil_linear
zI_Rsoil_linear_IC95inf <- zoo(Import$Rsoil_linear_IC95inf, Import$Import_datetime)    # low freq
#zI_Rsoil_linear_IC95inf
zI_Rsoil_linear_IC95sup <- zoo(Import$Rsoil_linear_IC95sup, Import$Import_datetime)    # low freq
#zI_Tsoil_surf
zI_STD_slope <- zoo(Import$STD_slope, Import$Import_datetime)    # low freq
#zI_STD_slope
zI_delta.T<- zoo(Import$delta.T, Import$Import_datetime)    # low freq
#zI_delta.T


# Merge series into one object : all time lines from Reference and Import will coexist in the same vector
z <- merge(zR,zI_Tsoil_surf,zI_CO2_amb,zI_Tsoil_10cm,zI_Rsoil_quadratic,zI_Rsoil_quadratic_IC95inf,zI_Rsoil_quadratic_IC95sup,
zI_Rsoil_linear,zI_Rsoil_linear_IC95inf,zI_Rsoil_linear_IC95sup,zI_STD_slope,zI_delta.T)
#z

# Interpolate calibration data along all time times from z (na.spline could also be used)
z$zI_Tsoil_surf <- na.approx(z$zI_Tsoil_surf,rule=2)
#z$zI_Tsoil_surf
z$zI_CO2_amb<- na.approx(z$zI_CO2_amb,rule=2)
#z$zI_CO2_amb
z$zI_Tsoil_10cm <- na.approx(z$zI_Tsoil_10cm,rule=2)
#z$zI_Tsoil_10cm
z$zI_Rsoil_quadratic <- na.approx(z$zI_Rsoil_quadratic,rule=2)
#z$zI_Rsoil_quadratic
z$zI_Rsoil_quadratic_IC95inf <- na.approx(z$zI_Rsoil_quadratic_IC95inf,rule=2)
#z$zI_Rsoil_quadratic_IC95inf
z$zI_Rsoil_quadratic_IC95sup <- na.approx(z$zI_Rsoil_quadratic_IC95sup,rule=2)
#z$zI_Rsoil_quadratic_IC95sup
z$zI_Rsoil_linear<- na.approx(z$zI_Rsoil_linear,rule=2)
#z$zI_Rsoil_linear
z$zI_Rsoil_linear_IC95inf <- na.approx(z$zI_Rsoil_linear_IC95inf,rule=2)
#z$zI_Rsoil_linear_IC95inf
z$zI_Rsoil_linear_IC95sup <- na.approx(z$zI_Rsoil_linear_IC95sup,rule=2)
#z$zI_Rsoil_linear_IC95sup
z$zI_STD_slope <- na.approx(z$zI_STD_slope,rule=2)
#z$zI_STD_slope
z$zI_delta.T <- na.approx(z$zI_delta.T,rule=2)
#z$zI_delta.T

# Only keep index time values from 'Reference' data
Z <- z[index(zR),]
#Z

#create data.frame from vectors:
data1<-data.frame(Z)

#data1

#Export output data to file: 
write.table(data1,"C:/Users/JuniorPastor/Dropbox/Master Junior, Karel, Olivier/Datos/Articulo 1_Rsoil/Rsoil Chamber Robot/Datos/Interpolated Rsoil Semih data/Interpolated_Modificado_JP/Out_datainterpolRsoil.csv", sep=",")



