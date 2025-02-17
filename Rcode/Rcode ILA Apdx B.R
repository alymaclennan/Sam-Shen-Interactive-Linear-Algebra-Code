################################################################
#
# Appendix B: Visualization of Matrices
#
################################################################


# R plot Fig. 1.1: A simple line graph of data
# go to your working directory
setwd("/Users/sshen/climstats") 
# read the data file from the folder named "data"
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE) #Read from the data folder
# check the data matrix dimension
dim(NOAAtemp)
#[1] 140   6 
#140 years from 1880 to 2019
#2019 will be excluded since data only up to July 2019
#col1 is year, col2 is anomalies, col3-6 are data errors
# set the plot margins and the positions of labels
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type ="o", col="brown", lwd=3,
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(
       paste("Temperature Anomaly [", degree,"C]"))
)

#R plot Fig. 1.2: Staircase chart of data
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="s", #staircase curve for data
     col="black", lwd=2,
     main="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="year",
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)

# R plot Fig. 1.3: A color bar chart of data
x <- NOAAtemp[,1]
y <- NOAAtemp[,2]
z <- rep(-99, length(x))
# compute 5-point moving average
for (i in 3:length(x)-2) z[i] = 
  mean(c(y[i-2],y[i-1],y[i],y[i+1],y[i+2]))
n1 <- which(y>=0); x1 <- x[n1]; y1 <- y[n1]
n2 <- which(y<0); x2 <- x[n2]; y2 <- y[n2]
x3 <- x[2:length(x)-2]
y3 <- z[2:length(x)-2]
plot(x1, y1, type="h", #bars for data
     xlim = c(1880,2016), lwd=3, 
     tck = 0.02,  #tck>0 makes ticks inside the plot
     ylim = c(-0.7,0.7), xlab="Year", col="red",
     ylab = expression(paste(
       "Temperature Anomaly [", degree,"C]")),
     main ="Global Land-Ocean Average Annual Mean 
     Surface Temperature Anomalies: 1880-2018",
     cex.lab = 1.2, cex.axis = 1.2)
lines(x2, y2, type="h",
      lwd = 3, tck = -0.02, col = "blue")
lines(x3, y3, lwd = 2)

#
#R code for computing statistical indices
setwd("/Users/sshen/climstats")
NOAAtemp = read.table(
  "data/aravg.ann.land_ocean.90S.90N.v4.0.1.201907.txt",
  header=FALSE)
temp2018=NOAAtemp[1:139,2] #use the temp data up to 2018
head(temp2018) #show the first six values
#[1] -0.370221 -0.319993 -0.320088 -0.396044 -0.458355 -0.470374
mean(temp2018) #mean
#[1] -0.1858632
sd(temp2018) #standard deviation
#[1] 0.324757
var(temp2018) #variance
#[1] 0.1054671
library(e1071) 
#This R library is needed to compute the following parameters
#install.packages("e1071") #if it is not in your computer
skewness(temp2018) 
#[1] 0.7742704
kurtosis(temp2018)
#[1] -0.2619131
median(temp2018)
#[1] -0.274434
quantile(temp2018,probs= c(0.05, 0.25, 0.75, 0.95))
#       5%        25%        75%        95% 
#  -0.5764861 -0.4119770  0.0155245  0.4132383 

#
#R plot Fig. 1.4. Histogram nad its fit
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
h <- hist(NOAAtemp[1:139, 2], 
          main="Histogram of 1880-2018 Temperature Anomalies",
          xlab=expression(paste(
            "Temperature anomalies [", degree, "C]")), 
          xlim=c(-1,1), ylim=c(0,30),
          breaks=10, cex.lab=1.2, cex.axis=1.2) 
xfit <- seq(-1, 1, length=100)
areat <- sum((h$counts)*diff(h$breaks[1:2]))#Normalization area
yfit <- areat*dnorm(xfit, 
                    mean=mean(NOAAtemp[1:139,2]), 
                    sd=sd(NOAAtemp[1:139,2]))
#Plot the normal fit on the histogram
lines(xfit, yfit, col="blue", lwd=3) 

#
# R plot Fig. 1.5: Box plot
boxplot(NOAAtemp[1:139, 2], ylim = c(-0.8, 0.8), 
        ylab=expression(paste(
          "Temperature anomalies [", degree, "C]")),
        width=NULL, cex.lab=1.2, cex.axis=1.2)

#
# R plot Fig. 1.6: Q-Q plot for the standardized 
# global average annual mean temperature anomalies
temp2018 <- NOAAtemp[1:139,2]
tstand <- (temp2018 - mean(temp2018))/sd(temp2018)
set.seed(101)
qn <- rnorm(139) #simulate 139 points by N(0,1)
qns <- sort(qn) # sort the points
qq2 <- qqnorm(qns,col="blue",lwd = 2)

setEPS() #Automatically saves the .eps file
postscript("fig0106.eps", height=7, width=7)
par(mar = c(4.5,5,2.5,1), xaxs = "i", yaxs = "i")
qt = qqnorm(tstand, 
            main = "Q-Q plot for the Standardized Global Average 
  Annual Mean Temperature Anomalies vs N(0,1)", 
            ylab="Quantile of Temperature Anomalies", 
            xlab="Quantile of N(0,1)", 
            xlim=c(-3,3), ylim = c(-3,3),
            cex.lab = 1.3, cex.axis = 1.3)
qqline(tstand, col = "red", lwd=3)
points(qq2$x, qq2$y, pch = 19, 
       col ="purple")
dev.off()

# R plot Fig. 1.7: Data line graph with a linear trend line
par(mar=c(3.5,3.5,2.5,1), mgp=c(2,0.8,0))
plot(NOAAtemp[1:139,1], NOAAtemp[1:139,2],
     type="l", col="brown", lwd=3,
     main=" Global Land-Ocean Average Annual Mean 
Surface Temperature Anomalies: 1880-2018",
     cex.lab=1.2,cex.axis=1.2,
     xlab="Year", 
     ylab=expression(paste(
       "Temperature Anomaly [", degree,"C]"))
)
abline(lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1]),
       lwd=3, col="blue")
lm(NOAAtemp[1:139,2] ~ NOAAtemp[1:139,1])
#       (Intercept)  NOAAtemp[1:139, 1]  
#-13.872921            0.007023 
#Trend 0.7023 degC/100a
text(1930, 0.5, 
     expression(paste("Linear trend: 0.7023",
                      degree,"C/100a")),
     cex = 1.5, col="blue")


#
# Rea read the netCDF data: NOAAGlobalTemp 
setwd("/Users/sshen/climstats")  
#install.packages("ncdf4")
library(ncdf4)
nc = ncdf4::nc_open("data/air.mon.anom.nc")
nc # describes details of the dataset
Lat <- ncvar_get(nc, "lat")
Lat # latitude data
#[1] -87.5 -82.5 -77.5 -72.5 -67.5 -62.5
Lon <- ncvar_get(nc, "lon")
Lon # longitude data
#[1]   2.5   7.5  12.5  17.5  22.5  27.5 
Time<- ncvar_get(nc, "time")
head(Time) # time data in Julian days
#[1] 29219 29250 29279 29310 29340 29371
library(chron) # convert Julian date to calendar date
nc$dim$time$units # .nc base time for conversion
#[1] "days since 1800-1-1 00:00:0.0"
month.day.year(29219,c(month = 1, day = 1, year = 1800))
#1880-01-01 # the beginning time of the dataset
tail(Time)
#[1] 79988 80019 80047 80078 80108 80139
month.day.year(80139,c(month = 1, day = 1, year = 1800))
#2019-06-01 # the end time of the dataset

# extract anomaly data in (lon, lat, time) coordinates
NOAAgridT <- ncvar_get(nc, "air") 
dim(NOAAgridT) # dimensions of the data array
#[1]  72   36 1674 #5-by-5, 1674 months from Jan 1880-Jun 2019


#
#Plot Fig. 1.8: Dec 2015 global surface temp anomalies map
library(maps)# requires maps package 
mapmat=NOAAgridT[,,1632]
# Julian date time 1632 corresponds to Dec 2015
mapmat=pmax(pmin(mapmat,6),-6) # put values in [-6, 6]
int=seq(-6, 6, length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
                               'darkgreen', 'green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5, 4, 2.5, 1), mgp=c(2.3, 0.8, 0))
filled.contour(Lon, Lat, mapmat, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main="NOAAGlobalTemp Anomalies: Dec 2015",
                                xlab="Latitude", ylab="Longitude", cex.lab=1.2),
               plot.axes={axis(1, cex.axis=1.2, las=1); 
                 axis(2, cex.axis=1.2, las=2);
                 map('world2', add=TRUE); grid()},
               key.title=title(main=expression(paste("[", degree, "C]"))),
               key.axes={axis(4, cex.axis=1.2)})


#Fig. 1.9 is plotted by Panoply
#
#R plot Fig. 1.10: Hovmoller diagram
library(maps)
mapmat=NOAAgridT[30,12:24,1309:1668]
#Longitude= 240 deg, Lat =[-30 30] deg
#Time=Jan 1989-Dec 2018: 30 years
mapmat=pmax(pmin(mapmat,2),-2) # put values in [-2,2]
par(mar=c(4,5,3,0))
int=seq(-2,2,length.out=81)
rgb.palette=colorRampPalette(c('black','blue', 
                               'darkgreen','green', 'yellow','pink','red','maroon'),
                             interpolate='spline')
par(mar=c(3.5,3.5,2.5,1), mgp=c(2.4, 0.8, 0))
x = seq(1989, 2018, len=360)
y = seq(-30, 30, by=5)
filled.contour(x, y, t(mapmat), 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
                                  "Hovmoller diagram of the NOAAGlobalTemp Anomalies",
                                xlab="Time",ylab="Latitude", cex.lab=1.2),
               plot.axes={axis(1, cex.axis=1.2); 
                 axis(2, cex.axis=1.2); 
                 map('world2', add=TRUE);grid()},
               key.title=title(main = 
                                 expression(paste("[", degree, "C]"))),
               key.axes={axis(4, cex.axis=1.2)})


#
#R read a 4-Dimensional netCDF file
setwd("/Users/sshen/mathmodel")
library(ncdf4)
# read GODAS data 1-by-1 deg, 40 levels, Jan-Dec 2015
nc=ncdf4::nc_open("data/godas2015.nc")
nc
Lat <- ncvar_get(nc, "lat")
Lat
Lon <- ncvar_get(nc, "lon")
Lon
Level <- ncvar_get(nc, "level")
Level
Time <- ncvar_get(nc, "time")
head(Time)
#[1] 78527 78558 78586 78617 78647 78678
library(chron)
month.day.year(78527,c(month = 1, day = 1, year = 1800))
# 2015-01-01
# potential temperature pottmp[lon, lat, level, time] 
godasT <- ncvar_get(nc, "pottmp")
dim(godasT)
#[1] 360 418  40  12, 
#i.e., 360 lon, 418 lat, 40 levels, 12 months=2015
t(godasT[246:250, 209:210, 2, 12]) 
#Dec level 2 (15-meter depth) water temperature [K] of
#a few grid boxes over the eastern tropical Pacific
#        [,1]     [,2]     [,3]     [,4]     [,5]
#[1,] 300.0655 299.9831 299.8793 299.7771 299.6641
#[2,] 300.1845 300.1006 299.9998 299.9007 299.8045

int=seq(273,298,length.out=81)
rgb.palette=colorRampPalette(c('black','blue',
                               'darkgreen','green', 'white','yellow',
                               'pink','red','maroon'), interpolate='spline')
tem = godasT[, , 1, 1]
par(mar=c(3.5, 3.5, 2.5, 0), mgp=c(2, 0.8, 0))
filled.contour(Lon, Lat, tem, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
                                  "GODAS 2015 Annual Mean Temperature at 195 [m] Depth Level",
                                xlab="Longitude",ylab="Latitude",
                                cex.lab=1.3, cex.axis=1.3),
               plot.axes={axis(1); axis(2); map('world2', add=TRUE);grid()},
               key.title=title(main="[K]"))
dev.off()
#
# R plot Fig. 1.11: The ocean potential temperature
# the 20th layer from surface: 195 meters depth
# compute 2015 annual mean temperature at 20th layer
library(maps)
climmat=matrix(0,nrow=360,ncol=418)
sdmat=matrix(0,nrow=360,ncol=418)
Jmon<-1:12
for (i in 1:360){
  for (j in 1:418){
    climmat[i,j] = mean(godasT[i,j,20,Jmon]); 
    sdmat[i,j]=sd(godasT[i,j,20,Jmon]) 
  }
}
int=seq(273,298,length.out=81)
rgb.palette=colorRampPalette(c('black','blue',
                               'darkgreen','green', 'white','yellow',
                               'pink','red','maroon'), interpolate='spline')
setwd("/Users/sshen/climstats")
setEPS() # save the .eps figure file 
postscript("fig0111.eps", height = 5, width = 10)
par(mar=c(3.5, 3.5, 2.5, 0), mgp=c(2, 0.8, 0))
filled.contour(Lon, Lat, climmat, 
               color.palette=rgb.palette, levels=int,
               plot.title=title(main=
                                  "GODAS 2015 Annual Mean Temperature at 195 [m] Depth Level",
                                xlab="Longitude",ylab="Latitude",
                                cex.lab=1.3, cex.axis=1.3),
               plot.axes={axis(1); axis(2); map('world2', add=TRUE);grid()},
               key.title=title(main="[K]"))
dev.off()


#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#
#
#Jingle Bells Music in R
#by Keith McNulty in 2018
#https://paulvanderlaken.com/2017/12/18/jingle-bells-in-r/


#Original version
if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)

#Revised version

if(!"dplyr" %in% installed.packages()) install.packages("dplyr")
if(!"audio" %in% installed.packages()) install.packages("audio")

library("dplyr")
library("audio")

notes <- c(A = 0, B = 2, C = 3, D = 5, E = 7, F = 8, G = 10)

pitch <- paste("F C F",
               "G D G",
               "G G C D",
               "E",
               "F F F F",
               "G B B B",
               "E D D E",
               "D G",
               "E E E",
               "E E E",
               "E G C D",
               "E",
               "F F F F",
               "F E E E E",
               "G G F D",
               "C",
               "G3 E D C",
               "G3",
               "G3 G3 G3 E D C",
               "A3",
               "A3 F E D",
               "B3",
               "G G F D",
               "E",
               "G3 E D C",
               "G3",
               "G3 E D C",
               "A3 A3",
               "A3 F E D",
               "G G G G A G F D",
               "C C5 B A G F G",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "E D D D D E",
               "D D E F G F E D",
               "E E E G C D",
               "E E E G C D",
               "E F G A C E D F",
               "E C D E F G A G",
               "F F F F F F",
               "F E E E E E",
               "G C5 B A G F E D",
               "C C E G C5")

duration <- c(1, 3, 4,
              3, 1, 2,
              1, 2, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 3, 1,
              2, 2,
              1, 1, 2,
              1, 1, 2,
              1, 1, 1.5, 0.5,
              4,
              1, 1, 1, 1,
              1, 1, 1, 0.5, 0.5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, .5, .5,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              4,
              1, 1, 1, 1,
              3, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 1, 1, 1,
              1, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 0.5, 0.5, 1,
              1, 0.33, 0.33, 0.33, 1, 0.33, 0.33, 0.33,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              1, 1, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              1, 0.5, 0.5, 1, 0.5, 0.5,
              0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5, 0.5,
              1, 0.33, 0.33, 0.33, 2)

jbells <- data_frame(pitch = strsplit(pitch, " ")[[1]],
                     duration = duration)

jbells <- jbells %>%
  mutate(octave = substring(pitch, nchar(pitch)) %>%
           {suppressWarnings(as.numeric(.))} %>%
           ifelse(is.na(.), 4, .),
         note = notes[substr(pitch, 1, 1)],
         note = note + grepl("#", pitch) -
           grepl("b", pitch) + octave * 12 +
           12 * (note < 3),
         freq = 2 ^ ((note - 60) / 12) * 440)

tempo <- 250

sample_rate <- 44100

make_sine <- function(freq, duration) {
  wave <- sin(seq(0, duration / tempo * 60, 1 / sample_rate) *
                freq * 2 * pi)
  fade <- seq(0, 1, 50 / sample_rate)
  wave * c(fade, rep(1, length(wave) - 2 * length(fade)), rev(fade))
}

jbells_wave <- mapply(make_sine, jbells$freq, jbells$duration) %>%
  do.call("c", .)

play(jbells_wave)



### SVD for A into vectors
A = matrix(c(1,3,2,4,-2,5), nrow = 2)
A
#     [,1] [,2] [,3]
#[1,]    1    2   -2
#[2,]    3    4    5
res = svd(A)
res
U = res$u
U
V = res$v
V
D = diag(res$d)
D
U%*%D%*%t(V)

D[1,1]*U[,1]%*%t(V[,1]) + D[2,2]*U[,2]%*%t(V[,2])

#First mode recovery
D[1,1]*U[,1]%*%t(V[,1])

#Second mode recovery
D[2,2]*U[,2]%*%t(V[,2])
