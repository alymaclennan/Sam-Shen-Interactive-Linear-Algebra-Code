################################################################
#
# Chapter 4: Matrix Applications to Regression Models
#
################################################################

#Plot Fig. 4.1: Colorado temperature lapse rate: R code
x= c(
  1671.5, 1635.6, 2097.0, 1295.4, 1822.7, 2396.9, 2763.0, 1284.7,
  1525.2, 1328.6, 1378.9, 2323.8, 2757.8, 1033.3, 1105.5, 1185.7,
  2343.9, 1764.5, 1271.0, 2347.3, 2094.0, 2643.2, 1837.9, 1121.7)
y= c(
  22.064, 23.591, 18.464, 23.995, 20.645, 17.175, 13.582, 24.635,
  22.178, 24.002, 23.952, 16.613, 13.588, 25.645, 25.625, 25.828,
  17.626, 22.433, 24.539, 17.364, 17.327, 15.413, 22.174, 24.549)
setEPS() # save the .eps figure 
postscript("fig0401.eps",  width = 8)
par(mar=c(4.5,4.5,2.5,0.5))
plot(x,y, 
     xlab="Elevation [m]",
     ylab=expression("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5, cex.main =1.2)
reg=lm(y~x)
reg
#(Intercept)            x  
# 33.476216    -0.006982   #-7.0 degC/1km
summary(reg)
#R-squared:  0.9631
abline(reg,lwd=3)
text(2100, 25.5, 
     expression("Temperature lapse rate: 7.0"~degree~"C/1.0km"), 
     cex=1.5)
text(2350, 24, "y = 33.48 - 0.0070 x", cex=1.5)
text(2350, 22.5,"R-squared = 0.96", cex=1.5)
dev.off()

#R code for the Colorado TLR regression analysis
lm(y ~ x)
#(Intercept)            x  
#33.476216    -0.006982 
reg = lm(y ~ x)
round(reg$residuals, digits = 5)
#       1        2        3        4        5        6 
#0.25792  1.53427 -0.37129 -0.43697 -0.10542  0.43358 
#7        8        9       10       11       12 
#-0.60335  0.12833 -0.64953 -0.19817  0.10302 -0.63880 
#13       14       15       16       17       18 
#-0.63366 -0.61692 -0.13283  0.63012  0.51454  1.27623 
#19       20       21       22       23       24 
#-0.06333  0.27628 -1.52923  0.39122  1.52971 -1.09572 

mean(reg$residuals)
#[1] 1.62043e-17

xa = x - mean(x)
sum(xa*reg$residuals)
#[1] -2.83773e-13

sum((reg$residuals)^2)/(length(y) -2)
#[1] 0.6096193


#
#Plot Fig. 4.2: R code
setEPS() 
postscript("fig0402.eps",  height = 5, width = 8)
par(mar=c(0.0,0.5,0.0,0.5))
plot(0,0, xlim=c(0,5.2), ylim=c(0,2.2),
     axes = FALSE, xlab="", ylab="")
arrows(0,0,4,0, angle=5, code=2, lwd=3, length=0.5)
arrows(4,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(0,0,4,2, angle=5, code=2, lwd=3, length=0.5)
arrows(5,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,5,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(3,0,4,2, angle=7, code=2, lwd=2, lty=3, length=0.5)
arrows(0,0,3,0, angle=7, code=2, lwd=2, lty=3, length=0.5)
segments(3.9,0, 3.9, 0.1)
segments(3.9, 0.1, 4.0, 0.1)
text(2,0.2, expression(hat(b)~bold(x)[a]), cex=2)
text(2,1.2, expression(bold(y)[a]), cex=2)
text(4.1,1, expression(bold(e)), cex=2)
text(3.8,0.6, expression(paste("Shortest ",bold(e))), 
     cex=1.5, srt=90)
text(3.4,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=71)
text(4.6,1.1, expression(paste("Longer ",bold(e))), 
     cex=1.5, srt=-71)
dev.off()

#
#
#R code for estimating regression slope b
#
#Method 1: Using vector projection
xa = x - mean(x)  #Anomaly the x data vector
nxa = sqrt(sum(xa^2)) #Norm of the anomaly data vector
ya = y - mean(y)
nya=sqrt(sum(ya^2))
sum(ya*(xa/nxa))/nxa #Compute b
#[1] -0.006981885  #This is an estimate for b

#Method 2:  Using correlation: R code
corxy=cor(xa, ya) #Compute the correlation between xa and ya
corxy
#[1] -0.9813858 #Very high correlation
corxy*nya/nxa #Compute b
#[1] -0.006981885 #This is an estimate for b

#
#R code for computing MV 
var(reg$fitted.values)
#[1] 15.22721 
#Or another way
yhat = reg$fitted.values
var(yhat)
#[1] 15.22721 
#Or still another way
n = 24
sum((yhat - mean(yhat))^2)/(n-1)
#[1] 15.22721 

#R code for computing YV 
sum((y - mean(y))^2)/(n-1)
# [1] 15.81033  
#Or another way
var(y)
#[1] 15.81033

#R code for computing R-squared value 
var(reg$fitted.values)/var(y)
#[1] 0.9631181  #This is the R-squared value

cor(x,y)
#[1] -0.9813858
(cor(x,y))^2
#[1] 0.9631181 #This is the R-squared value


#
#
qt(c(.025, .975), df=22)
#[1] -2.073873  2.073873

summary(reg)

-0.0069818 - 2.073873 * 0.0002913
#[1] -0.007585919
-0.0069818 + 2.073873 * 0.0002913
#[1] -0.006377681

33.4762157 - 2.073873*0.5460279
#[1] 32.34382
33.4762157 + 2.073873*0.5460279
#[1] 34.60861

(-0.0069818-(-0.0073))/0.0002913
#[1] 1.092345


#R plot Fig. 4.3: Confidence interval of a regression model
setwd("/Users/sshen/climstats")
#Confidence interval of the linear model
x1 = seq(max(x), min(x),len=100)
n = 24
xbar = mean(x)
reg = lm(y ~ x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modTLR = 33.476216 + -0.006982*x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modTLR + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modTLR - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modTLR + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modTLR - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0403.eps", height = 8, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x,y, 
     ylim=c(10,30), xlim=c(1000,3000),
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
abline(reg,lwd=3)
text(2280, 26, 
     bquote("Temperature lapse rate: 7.0"~degree~"C/km"), 
     cex=1.5)
text(2350, 27.5,"R-squared = 0.96", cex=1.5)
text(2350, 29, "y= 33.48 - 0.0070 x", cex=1.5)
text(1600, 15,"Blue lines: CI of July Tmean RV", 
     col="blue", cex=1.5)
text(1600, 13.5,"Red lines: CI of the fitted model", 
     col="red", cex=1.5)
dev.off()


#
#Plot Fig. 4.4: R code
reg = lm(y~x)
setEPS() #Plot the figure and save the file
postscript("fig0404.eps", height = 4.5, width = 8)
par(mar=c(4.5,4.5,2.0,0.5))
plot(x, reg$residuals, pch=5,
     ylim=c(-2,2), xlim=c(1000,2800),
     xlab="Elevation [m]",
     ylab=bquote("Residual Temp ["~degree~"C]"),
     main="Residuals of the Colorado 1981-2010 July Tmean vs. Elevation",
     cex.lab=1.5, cex.axis=1.5, cex.main = 1.2)
dev.off()

#
#Plot Fig. 4.5: R code
reg = lm(y ~ x)
setEPS() #Plot the figure and save the file
postscript("fig0405.eps", height = 6, width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
qqnorm(reg$residuals, pch=5,
       main="QQ-Normal Plot for the Colorado TLR Residuals",
       cex.lab = 1.4, cex.axis = 1.4)
qqline(reg$residuals, lty=2)
dev.off()

#
#R code for the DW-test for independence
#install.packages("lmtest")
library(lmtest)
ElevTemp=cbind(x,y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
dwtest(reg1)
#DW = 2.3072, p-value = 0.7062


#
#R code for the Mann-Kendall test
#install.packages("trend")
library(trend)
ElevTemp=cbind(x, y, 1:24)
#Sort the data for ascending elevation
ElevTemp=ElevTemp[order(x),]
reg1=lm(ElevTemp[,2] ~ ElevTemp[,1])
ElevTemp[,3]=reg1$residuals
mk.test(ElevTemp[,3])
#data:  ElevTemp[, 3]
#z = 0.47128, n = 24, p-value = 0.6374
mk.test(ElevTemp[,2])
#z = -5.9779, n = 24, p-value = 2.261e-09

#
#
lat=c(39.9919, 38.4600, 39.2203, 38.8236, 39.2425, 37.6742,
      39.6261, 38.4775, 40.6147, 40.2600, 39.1653, 38.5258,
      37.7717, 38.0494, 38.0936, 38.0636, 37.1742, 38.4858,
      38.0392, 38.0858, 40.4883, 37.9492, 37.1786, 40.0583)
lon=c(
  -105.2667, -105.2256, -105.2783, -102.3486, -107.9631, -106.3247,
  -106.0353, -102.7808, -105.1314, -103.8156, -108.7331, -106.9675,
  -107.1097, -102.1236, -102.6306, -103.2153, -105.9392, -107.8792,
  -103.6933, -106.1444, -106.8233, -107.8733, -104.4869, -102.2189)

#R code for the TLR multivariate linear regression
elev = x; temp = y #The x and y data were entered earlier
dat = cbind(lat, lon, elev, temp)
datdf = data.frame(dat)
datdf[1:2,] #Show the data of the first two stations
#      lat       lon   elev     temp
# 39.9919 -105.2667 1671.5 22.064
# 38.4600 -105.2256 1635.6 23.591

#Multivariate linear regression
reg=lm(temp ~ lat + lon + elev, data = datdf) 
summary(reg)  #Display the regression results 
#                  Estimate   Std. Error   t value  Pr(>|t|)    
#(Intercept) 36.4399561  9.4355746   3.862 0.000971 ***
#  lat         -0.4925051  0.1320096  -3.731 0.001319 ** 
#  lon         -0.1630799  0.0889159  -1.834 0.081564 .  
#  elev        -0.0075693  0.0003298 -22.953 7.67e-16 ***
#Residual standard error: 0.6176 on 20 degrees of freedom
#Multiple R-squared:  0.979

round(reg$coefficients, digits=5)
colnames(datdf) <- c('x1', 'x2', 'x3', 'y')
reg=lm(y ~ x1 + x2 + x3, data = datdf)
reg

#
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["*degree*"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"*degree*"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["*degree*"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#
#
#Kolmogorov-Smirnov (KS) test for normality
library(fitdistrplus)
resi_mean = mean(reg$residuals)
resi_sd = sd(reg$residuals)
test_norm = rnorm(length(reg$residuals), 
                  mean = 0, sd = 1)
testvar = (reg$residuals - resi_mean)/resi_sd
ks.test(testvar, test_norm)
#D = 0.057554, p-value = 0.9754
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

#degrees of freedom and critical t values
rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof #[1] 5.183904 effective degrees of freedom
qt(.975, df=137) #[1] 1.977431 critical t value
qt(.975, df=5) #[1] 2.570582 critical t value

#
#Plot Fig. 4.7: R code
#Polynomial fitting by multiple linear  regression
x1=x
x2=x1^2
x3=x1^3
dat3=data.frame(cbind(x1,x2,x3,y))
reg3 = lm(y ~ x1 + x2 + x3, data=dat3)
# simply use
# reg3 = lm(y ~ x + I(x^2) + I(x^3))
setEPS() #Plot the figure and save the file
postscript("fig0407.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  type="o", xaxt="n",
     cex.lab=1.4, cex.axis=1.4, xlab="Year", 
     ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
lines(x, predict(reg3), col="black", lwd=3)
reg3
#(Intercept)           x1           x2           x3  
#-1.426e+03    2.333e+00   -1.271e-03    2.308e-07  
text(1940, 0.3, 
     "The third order polynomial fit",
     col="black",cex=1.4)
text(1880, 0.58, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg3$residuals, 
     pch=5, cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Residuals ["~degree~"C]"))
text(1880, 0.32, "(b)", cex=1.4)
dev.off()


######Below is for an earlier Fig. 4.6 in the book draft 
# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = x1 = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x1) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x1)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x1, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x1, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


################## New Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot the figure and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')
text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582

### New ### Fig. 4.6

# Plot Fig. 4.6 and make regression diagnostics: R code
setwd("/Users/sshen/climstats")
dtmean<-read.table(
  "data/aravg.ann.land_ocean.90S.90N.v5.0.0.201909.txt", 
  header=F)
dim(dtmean)
#[1] 140   6
x = dtmean[1:139,1]
y = dtmean[1:139,2]
reg =  lm(y ~ x) #linear regression
reg
#(Intercept)       yrtime  
#-14.574841     0.007348 

#Confidence interval of the linear model
xbar = mean(x)
SSE = sum((reg$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x
xbar = mean(x)
Sxx = sum((x-xbar)^2)
n = length(y)
CIupperModel= modT + 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModel= modT - 
  qt(.975, df=n-2)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponse= modT + 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponse= modT - 
  qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

CIupperModelr= modT + 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIlowerModelr= modT - 
  qt(.975, df=5)*s*sqrt((1/n)+(x-xbar)^2/Sxx)
CIupperResponser= modT + 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)
CIlowerResponser= modT - 
  qt(.975, df=5)*s*sqrt(1+(1/n)+(x-xbar)^2/Sxx)

setEPS() #Plot figure Fig. 4.6 and save the file
postscript("fig0406.eps", height = 8, width = 8)
par(mfrow=c(2,1))
par(mar=c(0,4.5,2.5,0.7))
plot(x, y,  ylim = c(-1.5, 1),
     type="o", xaxt="n", yaxt="n",
     cex.lab=1.4, cex.axis=1.4,
     xlab="Year", ylab=bquote("Temperature ["~degree~"C]"), 
     main="Global Annual Mean Surface Temperature Anomalies",
     cex.lab=1.4, cex.axis=1.4
)
axis(side = 2, at = c(-1.0, 0, 1.0), cex.axis = 1.4)
abline(reg, col="black", lwd=3)
lines(x,CIupperModel,type="l",col='red')
lines(x,CIlowerModel,type="l",col='red')
lines(x,CIupperResponse,type="l",col='blue')
lines(x,CIlowerResponse,type="l",col='blue')

lines(x,CIupperModelr,type="l", lty = 3, col='red')
lines(x,CIlowerModelr,type="l", lty = 3, col='red')
lines(x,CIupperResponser,type="l",lty = 3, col='blue')
lines(x,CIlowerResponser,type="l",lty = 3, col='blue')

text(1940, 0.5, 
     bquote("Linear trend: 0.7348"~degree~"C per century"), 
     col="black",cex=1.4)
text(1880, 0.9, "(a)", cex=1.4)
par(mar=c(4.5,4.5,0,0.7))
plot(x, reg$residuals, ylim = c(-0.6,0.6),
     pch=5, cex.lab=1.4, cex.axis=1.4,
     yaxt = 'n', xlab="Year", 
     ylab=bquote("Residuals ["~degree~"C]"))
axis(side = 2, at = c(-0.3, 0, 0.3), cex.axis = 1.4)
text(1880, 0.5, "(b)", cex=1.4)
dev.off()

#Kolmogorov-Smirnov (KS) test for normality
ks.test(reg$residuals, test_norm)
#D = 0.086331, p-value = 0.6782
#The normality assumption is accepted

#Diagnostics on independence and normality
# Durbin-Watson (DW) test for independence
dwtest(reg)
#DW = 0.45235, p-value < 2.2e-16
#The independence assumption is rejected

rho1 = acf(y)[[1]][2] #Auto-correlation function
rho1 #[1] 0.9270817
edof = (length(y) - 2)*(1 - rho1)/(1 + rho1)
edof # [1] 5.183904
qt(.975, df=137) #[1] 1.977431
qt(.975, df=5) #[1] 2.570582


yautoc = acf(y) #Auto-correlation function
yautoc$acf[2]
#[1] 0.9270817

cor(y[1:138], y[2:139])
#[1] 0.9459649

length(y)
#[1] 139

#Confidence interval of the linear model
x1 = x
xbar = mean(x1)
reg8018 = lm(y ~ x1)
SSE = sum((reg8018$residuals)^2)
s_squared = SSE/(length(y)-2)
s = sqrt(s_squared)
modT = -14.574841 + 0.007348 *x1
xbar = mean(x)
Sxx = sum((x-xbar)^2)
CIupperModel= modT + qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIlowerModel= modT - qt(.975, df=n-2)*s*sqrt((1/n)+(x1-xbar)^2/Sxx)
CIupperResponse= modT + qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)
CIlowerResponse= modT - qt(.975, df=n-2)*s*sqrt(1+(1/n)+(x1-xbar)^2/Sxx)

plot(x1, y, 
     xlab="Elevation [m]",
     ylab=bquote("Temperature ["~degree~"C]"),
     main="Colorado Elevation and July Tmean: 1981-2010 Average",
     cex.lab=1.5, cex.axis=1.5)
lines(x1,CIupperModel,type="l",col='red')
lines(x1,CIlowerModel,type="l",col='red')
lines(x1,CIupperResponse,type="l",col='blue')
lines(x1,CIlowerResponse,type="l",col='blue')


#Linear regression diagnostics: Check the assumptions
#Normality test by Q-Q plot
par(mfrow=c(1,1))
par(mar=c(4.5,4.5,2.5,0.7))
qqnorm(reg8018$residuals, pch=5,
       ylim=c(-0.4,0.4),xlim=c(-3,3),
       main="QQ-Normal Plot for the NOAAGlobalTemp: 1880-2018",
       cex.lab=1.4, cex.axis=1.4)
qqline(reg8018$residuals, lty=2, col = 'red')


#Kolmogorov-Smirnov (KS) test for normality
resi_sd=sd(reg8018$residuals)
resi_mean=mean(reg8018$residuals)
test_norm = rnorm(length(x1), mean = resi_mean,
                  sd=resi_sd)
ks.test(reg8018$residuals,test_norm)
#D = 0.086331, p-value = 0.6782
#Conclusion: Normal distribution is not rejected. 

#Check independence by Durbin-Watson (DW) test 
dwtest(reg8018)
#DW = 0.45235, p-value < 2.2e-16
#Conclusion: There is a significant serial correlation. 

yautoc = acf(y)
yautoc$acf[2]

cor(y,y)

#rm(list=ls()) #R forgets all the defined variables 
