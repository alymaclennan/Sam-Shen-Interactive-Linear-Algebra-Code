################################################################
#
# Chapter 3: Basics of Machine Learning
#
################################################################

#
#tWCSS calculation for N = 3 and K = 2
N = 3; K =2
mydata <- matrix(c(1, 1, 2, 1, 3, 3.5), 
                 nrow = N, byrow = TRUE)
x1 = mydata[1, ]
x2 = mydata[2, ]
x3 = mydata[3, ]

#Case C1 = (P1, P2)
c1 = (mydata[1, ] + mydata[2, ])/2
c2 = mydata[3, ]
tWCSS = norm(x1 - c1, type = '2')^2 + 
  norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c2, type = '2')^2
tWCSS
#[1] 0.5

#Case C1 = (P1, P3)
c1 = (mydata[1, ] + mydata[3, ])/2
c2 = mydata[2, ]
norm(x1 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x2 - c2, type = '2')^2
#[1] 5.125

#Case C1 = (P2, P3)
c1 = (mydata[2, ] + mydata[3, ])/2
c2 = mydata[1, ]
norm(x2 - c1, type = '2')^2 + 
  norm(x3 - c1, type = '2')^2 + 
  norm(x1 - c2, type = '2')^2
#[1] 3.625

#The case C1 = (P1, P2) can be quickly found by 
kmeans(mydata, 2) 
#Clustering vector:
#[1] 1 1 2 #points P1, P2 in C1
#
#
#
# R plot Fig. 9.1: K-means for N = 3 and K = 2
setwd("~/climstats")
N = 3 #The number of data points
K = 2 #Assume K clusters
mydata = matrix(c(1, 1, 2, 1, 3, 3.5), 
                nrow = N, byrow = TRUE)
Kclusters = kmeans(mydata, K) 
Kclusters #gives the K-means results, 
#e.g., cluster centers and WCSS 
#Cluster means:
#[,1] [,2]
#1  1.5  1.0
#2  3.0  3.5
#Within cluster sum of squares by cluster:
#  [1] 0.5 0.0
Kclusters$centers
par(mar = c(4,4,2.5,0.5))
plot(mydata[,1], mydata[,2], lwd = 2,
     xlim =c(0, 4), ylim = c(0, 4),
     xlab = 'x', ylab = 'y', col = c(2, 2, 4),
     main = 'K-means clustering for 
     three points and two clusters',
     cex.lab = 1.4, cex.axis = 1.4)
points(Kclusters$centers[,1], Kclusters$centers[,2],
       col = c(2, 4), pch = 4)
text(1.5, 0.8, bquote(C[1]), col = 'red', cex = 1.4)
text(3.2, 3.5, bquote(C[2]), col = 'skyblue', cex = 1.4)
text(1, 1.2, bquote(P[1]), cex = 1.4, col = 'red')
text(2, 1.2, bquote(P[2]), cex = 1.4, col = 'red')
text(3, 3.3, bquote(P[3]), cex = 1.4, col = 'skyblue')

#
#R for Fig. 9.2: K-means clustering for 2001 daily weather 
#data at Miami International Airport, Station ID USW00012839
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']
# plot the scatter diagram Tmin vs WDF2
setEPS() #Plot the data of 150 observations
postscript("fig0902a.eps",  width=5, height=5)
par(mar=c(4.5, 4.5, 2, 4.5))
plot(tmin[2:366], wdf2[2:366], 
     pch =16, cex = 0.5,
     xlab = 'Tmin [deg C]',
     ylab = 'Wind Direction [deg]', grid())
title('(a) 2001 Daily Miami Tmin vs WDF2', cex.main = 0.9, line = 1)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)
dev.off()
#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[2:366], wdf2[2:366])
fit = kmeans(mydata, K) # K-means clustering
#Output the coordinates of the cluster centers
fit$centers 
#1 18.38608 278.8608
#2 21.93357 103.9161
fit$tot.withinss # total WCSS
#[1] 457844.9 for # the value may vary for each run

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 4.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means Clusters for Daily Tmin vs WDF2", 
      cex.main = 0.8)
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)



#
#R plot Fig. 9.3: tWCSS(K) and pWCSS(K)
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
tmax = dat[,'TMAX']
wdf2 = dat[,'WDF2']
mydata = cbind(tmin[2:366], wdf2[2:366])
twcss = c()
for(K in 1:8){
  mydata=cbind(tmax[2:366], wdf2[2:366])
  twcss[K] = kmeans(mydata, K)$tot.withinss 
}
twcss
par(mar = c(4.5, 6, 2, 0.5))
par(mfrow=c(1,2))
plot(twcss/100000, type = 'o', lwd = 2,
     xlab = 'K', ylab = bquote('tWCSS [ x' ~  10^5 ~ ']'),
     main = '(a) The elbow principle from tWCSS scores',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, twcss[2]/100000, pch =16, cex = 3, col = 'blue')
text(4, 5, 'Elbow at K = 2', cex = 1.5, col = 'blue')
#compute percentage of variance explained
pWCSS = 100*(twcss[1]- twcss)/twcss[1] 
plot(pWCSS, type = 'o', lwd = 2,
     xlab = 'K', ylab = 'pWCSS [percent]',
     main = '(b) The knee principle from pWCSS variance',
     cex.lab = 1.5, cex.axis = 1.5)
points(2, pWCSS[2], pch =16, cex = 3, col = 'blue')
text(4, 80, 'Knee at K = 2', cex = 1.5, col = 'blue')
dev.off()


for(K in 1:8){
  mydata=cbind(tmax[2:366], wdf2[2:366])
  clusterK <- kmeans(mydata, K) # 5 cluster solution
  twcss[K] = fit$tot.withinss
}
twcss
plot(twcss, type = 'o')
pvariance = 100*(twcss[1]- twcss)/twcss[1] 
plot(pvariance, type = 'o')

#R plot Fig. 9.3b: pWCSS(K) for the knee principle


######################
#scale the data because of different units
#equivalent to standardized anomalies
mydata = data.frame(cbind(tmin[2:366], wdf2[2:366]))
tminS = scale(tmin[2:366])
wdf2S = scale(wdf2[2:366])
K = 2 #assuming K = 2, i.e., 2 clusters
mydataS = data.frame(cbind(tminS, wdf2S))
clusterK = kmeans(mydataS, K) # K-means clustering
#Output the scaled coordinates of the cluster centers
clusterK$centers 
#1 -0.9165034  1.7527775
#2  0.2252159 -0.4307167
clusterK$tot.withinss # total WCSS
#[1] 377.103 for # the value may vary for each run
#Centers in non-scaled units
Centers = matrix( nrow = 2, ncol = 2) 
Centers[,1] =
  clusterK$centers[,1] * attr(tminS, 
                              "scaled:scale") + attr(tminS, "scaled:center")
Centers[,2] =
  clusterK$centers[,2] * attr(wdf2S, 
                              "scaled:scale") + attr(wdf2S, "scaled:center")
Centers
#[1,] 17.14306 282.5000
#[2,] 22.15427 107.2014

#Plot clusters using convex hull
i = 1
N = 365
mycluster = clusterK$cluster
plotdat = cbind(1:N, mydata, mycluster)
X = plotdat[which(mycluster == i), 1:3]
plot(X[,2:3], pch = 16, cex = 0.5,
     xlim = c(0, 30), ylim = c(0, 365),
     col = 'red')
grid(5, 5)
hpts <- chull(X[, 2:3])
hpts <- c(hpts, hpts[1])
lines(X[hpts, 2:3], col = 'red')
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = 'red', cex = 0.8)
}



d1 = tminS[1:365] * attr(tminS, "scaled:scale") + attr(tminS, "scaled:center")
d1 - tmin[2:366]
#Visualize the clusters by fviz_cluster()
library(factoextra)
fviz_cluster(clusterK, data = mydataS, stand = FALSE,
             main = 'K-means Clustering for the Miami Daily Tmin vs WDF2')

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, clusterK$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#Visualize the clusters by kmeans.ani()
mycluster <- data.frame(mydata, fit$cluster)
names(mycluster)<-c('Daily Tmin [deg C]', 
                    'Wind Direction [deg]',
                    'Cluster')
library(animation)
par(mar = c(4.5, 4.5, 2, 0.5))
kmeans.ani(mycluster, centers = K, pch=1:K, col=1:K,
           hints = '')
title(main=
        "(b) K-means for Tmin vs WDF2")

#
#
#R plot Fig. 9.4: Convex hull for a cluster 
setwd("~/climstats")
dat = read.csv("data/MiamiIntlAirport2001_2020.csv", 
               header=TRUE)
dim(dat)
#[1] 7305   29
tmin = dat[,'TMIN']
wdf2 = dat[,'WDF2']
#K-means clustering 
K = 2 #assuming K = 2, i.e., 2 clusters
mydata = cbind(tmin[2:366], wdf2[2:366]) #2001 data
clusterK = kmeans(mydata, K) # K-means clustering
mycluster <- data.frame(mydata, clusterK$cluster)
plotdat = cbind(1:N, mycluster)

par(mar = c(4.5, 6, 2, 4.5))#set up the plot margin
i = 2 # plot Cluster 1
N = 365 #Number of data points
X = plotdat[which(mycluster[,3] == i), 1:3]
colnames(X)<-c('Day', 'Tmin [deg C]', 'Wind Direction [deg]')
plot(X[,2:3], pch = 16, cex = 0.5, col = i,
     xlim = c(0, 30), ylim = c(0, 365),
     main = 'Cluster 1 of Miami Tmin vs WDF2' )
grid(5, 5)
#chull() finds the boundary points of a convex hull
hpts = chull(X[, 2:3]) 
hpts1 = c(hpts, hpts[1]) #close the boundary
lines(X[hpts1, 2:3], col = i)
for(j in 1:length(X[,1])){
  text(X[j,2], X[j,3] + 8, paste("", X[j,1]), 
       col = i, cex = 0.8)
} #Put the data order on the cluster
axis(4, at = c(0, 45, 90, 135, 180, 225, 270, 315, 360),
     lab = c('N', 'NE', 'E', 'SE', 'S', 'SW',  'W', 'NW', 'N'))
mtext('Wind Direction', side = 4, line =3)

#
#
#R plot Fig. 9.5: Maximum difference between two points
x = matrix(c(1, 1, 3, 3), 
           ncol = 2, byrow = TRUE)#Two points
y= c(-1, 1) #Two labels -1 and 1
#Plot the figure and save it as a .eps file
setEPS() 
postscript("fig0905.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19, 
     xlim = c(-2, 6), ylim = c(-2, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5, 
     main = "Maximum Difference Between Two Points")
axis(2, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):6, tck = 1, lty = 2, 
     col = "grey", labels = NA)
segments(1, 1, 3, 3)
arrows(1, 1, 1.71, 1.71, lwd = 2,
       angle = 9, length= 0.2)
text(1.71, 1.71-0.4, quote(bold('n')), cex = 1.5)
arrows(4, 0, 4 + 0.5, 0 + 0.5, lwd = 3,
       angle = 15, length= 0.2, col = 'green' )
text(4.7, 0.5 -0.2, quote(bold('w')), 
     cex = 1.5, col = 'green')
x1 = seq(-2, 6, len = 31)
x20 = 4 - x1
lines(x1, x20, lwd = 1.5, col = 'purple')
x2m = 2 - x1
lines(x1, x2m, lty = 2, col = 2)
x2p = 6 - x1
lines(x1, x2p, lty = 2, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(3+0.2,3+0.5, bquote(P[2]), cex = 1.5, col = 4)
text(1-0.2,1-0.5, bquote(P[1]), cex = 1.5, col = 2)
text(0,4.3, 'Separating Hyperplane', 
     srt = -45, cex = 1.2, col = 'purple')
text(1.5, 4.8, 'Positive Hyperplane', 
     srt = -45, cex = 1.2, col = 4)
text(-1, 3.3, 'Negative Hyperplane', 
     srt = -45, cex = 1.2, col = 2)
dev.off()


#
#
#R plot Fig. 9.6: SVM for three points
#training data x
x = matrix(c(1, 1, 2, 1, 3, 3.5), 
           ncol = 2, byrow = TRUE)
y = c(1, 1, -1) #two categories 1 and -1
plot(x, col = y + 3, pch = 19,
     xlim = c(-2, 8), ylim = c(-2, 8))
library(e1071)
dat = data.frame(x, y = as.factor(y))
svm3P = svm(y ~ ., data = dat, 
            kernel = "linear", cost = 10, 
            scale = FALSE, 
            type = 'C-classification')
svm3P #This is the trained SVM
xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE) #New data
predict(svm3P, xnew) #prediction using the trained SVM
# 1  2 
# 1 -1

# Find hyperplane, normal vector, and SV (wx + b = 0)
w = t(svm3P$coefs) %*% svm3P$SV 
w
#[1,] -0.2758621 -0.6896552
b = svm3P$rho
b
#[1] -2.241379
2/norm(w, type ='2') #maximum margin of separation
#[1] 2.692582

x1 = seq(0, 5, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]
#plot the SVM results
setEPS() 
postscript("fig0906.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 3, pch = 19,
     xlim = c(0, 6), ylim = c(0, 6),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM for three points labeled in two categories')
axis(2, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
axis(1, at = (-2):8, tck = 1, lty = 2, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 4)
lines(x1, x2m, lty = 2, col = 2)
lines(x1, x20, lwd = 1.5, col = 'purple')
xnew = matrix(c(0.5, 2.5, 4.5, 4), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 18, cex = 2)
for(i in 1:2){
  text(xnew[i,1] + 0.5, xnew[i,2] , paste('Q',i),
       cex = 1.5, col = 6-2*i)
}
text(2.2,5.8, "Two blue points and a red point 
are training data for an SVM ", 
     cex = 1.5, col = 4)
text(3.5,4.7, "Two black diamond points 
         are to be predicted by the SVM", 
     cex = 1.5)
dev.off()

#
#
#R plot Fig. 9.7: SVM for many points
#Training data x and y
x = matrix(c(1, 6, 2, 8, 3, 7.5, 1, 8, 4, 9, 5, 9, 
             3, 7, 5, 9, 1, 5,
             5, 3, 6, 4, 7, 4,   8, 6, 9, 5, 10, 6, 
             5, 0, 6, 5, 8, 2, 2, 2, 1, 1), 
           ncol = 2, byrow = TRUE)
y= c(1, 1, 1, 1, 1, 1, 
     1, 1, 1,
     2, 2, 2, 2, 2, 2 ,
     2, 2, 2, 2, 2)

library(e1071)
dat = data.frame(x, y = as.factor(y))
svmP = svm(y ~ ., data = dat, 
           kernel = "linear", cost = 10, 
           scale = FALSE, 
           type = 'C-classification')
svmP
#Number of Support Vectors:  3
svmP$SV #SVs are #x[9,], x[17,], x[19,]
#9   1  5
#17  6  5
#19  2  2

# Find SVM parameters: w, b, SV (wx+c=0)
w = t(svmP$coefs) %*% svmP$SV 
# In essence this finds the hyper plane that separates our points
w
#[1,] -0.39996 0.53328
b <- svmP$rho
b
#[1] 1.266573
2/norm(w, type ='2')
#[1] 3.0003 is the maximum margin
x1 = seq(0, 10, len = 31)
x2 = (b - w[1]*x1)/w[2]
x2p = (1 + b - w[1]*x1)/w[2]
x2m = (-1 + b - w[1]*x1)/w[2]
x20 = (b - w[1]*x1)/w[2]
#plot the svm results
setEPS() 
postscript("fig0907.eps", height=7, width=7)
par(mar = c(4.5, 4.5, 2.0, 2.0))
plot(x, col = y + 9, pch = 19, cex =1.5,
     xlim = c(0, 10), ylim = c(0, 10),
     xlab = bquote(x[1]), ylab = bquote(x[2]),
     cex.lab = 1.5, cex.axis = 1.5,
     main = 'SVM application to many points of two labels')
axis(2, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
axis(1, at = 0:10, tck = 1, lty = 3, 
     col = "grey", labels = NA)
lines(x1, x2p, lty = 2, col = 10)
lines(x1, x2m, lty = 2, col = 11)
lines(x1, x20, lwd = 1.5, col = 'purple')
thetasvm = atan(-w[1]/w[2])*180/pi
thetasvm
#[1] 36.8699 #36.9 deg angle of the hyperplane
#linear equations for the hyperplanes
delx = 1.4
dely = delx * (-w[1]/w[2])
text(5 + 2*delx, 6.5 + 2*dely, bquote(bold(w%.%x) - b == 0), 
     srt = thetasvm, cex = 1.5, col = 'purple')
text(5 - delx, 7.6 - dely, bquote(bold(w%.%x) - b == 1), 
     srt = thetasvm, cex = 1.5, col = 10)
text(5, 4.8, bquote(bold(w%.%x) - b == -1), 
     srt = thetasvm, cex = 1.5, col = 11)
#normal direction of the hyperplanes
arrows(2, 3.86, 2 + w[1], 4 + w[2], lwd = 2,
       angle = 15, length= 0.1, col = 'blue' )
text(2 + w[1] + 0.4, 4 + w[2], bquote(bold(w)), 
     srt = thetasvm, cex = 1.5, col = 'blue')

#new data points to be predicted
xnew = matrix(c(0.5, 2.5, 7, 2, 6, 9), 
              ncol = 2, byrow = TRUE)
points(xnew, pch = 17, cex = 2)
predict(svmP, xnew) #Prediction
#1 2 3 
#2 2 1

for(i in 1:3){
  text(xnew[i,1], xnew[i,2] - 0.4 , 
       paste('Q',i), cex = 1.5)
}
dev.off()


#
#
#R plot Fig. 9.8: R.A. Fisher data of three iris species
setwd('~/climstats')
data(iris) #read the data already embedded in R
dim(iris)
#[1] 150   5
iris[1:2,] # Check the first two rows of the data
#    Sepal.Length Sepal.Width Petal.Length Petal.Width Species
#1          5.1         3.5          1.4         0.2  setosa
#2          4.9         3.0          1.4         0.2  setosa

str(iris) # Check the structure of the data
#'data.frame':	150 obs. of  5 variables:
#$ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#$ Species     : Factor w/ 3 levels "setosa","versicolor",...

setEPS() #Plot the data of 150 observations
postscript("fig0908.eps",  width=7, height=5)
par(mar= c(4.5, 4.5, 2.5, 0.2))
plot(iris[,1], type = 'o', pch = 16, cex = 0.5, ylim = c(-1, 9), 
     xlab = 'Sorted order of the flowers for measurement', 
     ylab = 'Length or width [cm]',
     main = 'R. A. Fisher data of irish flowers', col = 1, 
     cex.lab = 1.3, cex.axis = 1.3)
lines(iris[,2], type = 'o', pch = 16, cex = 0.5, col=2)
lines(iris[,3], type = 'o', pch = 16, cex = 0.5, col=3)
lines(iris[,4], type = 'o', pch = 16, cex = 0.5, col=4)
legend(0, 9.5, legend = c('Sepal length', 'Sepal width', 
                          'Petal length', 'Petal width'), 
       col = 1:4, lty = 1, lwd = 2, bty = 'n', 
       y.intersp = 0.8, cex = 1.2)
text(25, -1, 'Setosa 1-50', cex = 1.3)
text(75, -1, 'Versicolor 51-100', cex = 1.3)
text(125, -1, 'Virginica 101 - 150', cex = 1.3)
dev.off()

#
#
#R code of the RF experiment on the Fisher iris data 
#install.packages("randomForest") 
library(randomForest)
#set.seed(8)  # run this line to get the same result
#randomly select 120 observations as training data 
train_id = sort(sample(1:150, 120, replace = FALSE))
train_data = iris[train_id, ]
dim(train_data)
#[1] 120   5
#use the remaining 30 as the new data for prediction
new_data = iris[-train_id, ]
dim(new_data)
#[1] 30  5

#train the RF model
classifyRF = randomForest(x = train_data[, 1:4],
                          y = train_data[, 5], ntree = 800)
classifyRF #output RF training result
#Type of random forest: classification
#Number of trees: 800
#No. of variables tried at each split: 2


#OOB estimate of  error rate: 4.17%
#Confusion matrix:
#  		setosa versicolor virginica class.error
#setosa         41          0         0  0.00000000
#versicolor      0         34         2  0.05555556
#virginica       0          3        40  0.06976744

plot(classifyRF, 
     main='RF model error rate for each tree') 
#plot the errors vs RF trees Fig. 9.9a
#This plots the error rate data in the 
#matrix named classifyRF$err.rate
errRate = classifyRF$err.rate
dim(errRate)
#[1] 800   4
#Fig. 9.9a is a plot for this matrix data
#Fig. 9.9a can also be plotted by the following code
tree_num = 1:800
plot(tree_num, errRate[,1], 
     type = 's', col='black',
     ylim = c(0, 0.2),
     xlab = 'Trees', ylab = 'Error rate',
     main = 'RF model error rate for each tree',
     cex.axis = 1.3, cex.lab = 1.3)
lines(tree_num, errRate[,2], lwd =1.8,
      lty = 2, type = 's', col='red')
lines(tree_num, errRate[,3], lwd =1.8,
      lty = 3, type = 's', col='green')
lines(tree_num, errRate[,4], lwd =1.8,
      lty = 4, type = 's', col='skyblue')
legend(400, 0.21, lwd = 2.5,
       legend = c('OOB','Setosa', 'Vericolor', 'Virginica'),
       col=c('black', 'red', 'green', 'skyblue'), 
       lty=1:4, cex=1.2, y.intersp = 0.6,
       text.font = 2,  box.lty=0)

classifyRF$importance #classifyRF$ has many outputs
#             MeanDecreaseGini
#Sepal.Length         8.313131
#Sepal.Width          1.507188
#Petal.Length        31.075960
#Petal.Width         38.169763
#plot the importance result Fig. 9.9b
varImpPlot(classifyRF, sort = FALSE, 
           lwd = 1.5, pch = 16,
           main = 'Importance plot of the RF model',
           cex.axis = 1.3, cex.lab = 1.3)

classifyRF$confusion

#RF prediction for the new data based on the trained trees
predict(classifyRF, newdata = new_data[,1:4])
# 2          4         10         11         12         14 
#setosa     setosa     setosa     setosa     setosa     setosa 

#It got two wrong: 120 versicolor and 135 versicolor 

#Another version of the randomForest() command
anotherRF = randomForest(Species ~ ., 
                         data = train_data, ntree = 500)

#
#
#R plot Fig. 9.10: RF regression for ozone data
library(randomForest)
airquality[1:2,] #use R's RF benchmark data "airquality"
#  Ozone Solar.R Wind Temp Month Day
#1    41     190  7.4   67     5   1
#2    36     118  8.0   72     5   2
dim(airquality)
#[1] 153   6
ozoneRFreg = randomForest(Ozone ~ ., data = airquality, 
                          mtry = 2, ntree = 500, importance = TRUE, 
                          na.action = na.roughfix)
#na.roughfix allows NA to be replaced by medians 
#to begin with when training the RF trees
ozonePred = ozoneRFreg$predicted #RF regression result
t0 = 1:153
n1 = which(airquality$Ozone > 0) #positions of data
n0 = t0[-n1] #positions of missing data
ozone_complete = ozone_filled= airquality$Ozone 
ozone_complete[n0] = ozonePred[n0] #filled by RF
ozone_filled = ozonePred #contains the RF reg result
ozone_filled[n1] <- NA #replace the n1 positions by NA 
t1 = seq(5, 10, len = 153) #determine the time May - Sept

par(mfrow = c(2, 1))
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, airquality$Ozone,   
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', xaxt="n",
     main = '(a) Ozone data: Observed (black) and RF filled (blue)', 
     col = 1, cex.lab = 1.3, cex.axis = 1.3) 
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)
points(t1, ozone_filled, col = 'blue', 
       type = 'o', pch = 16, cex = 0.5)#RF filled data

#Plot the complete data
par(mar = c(3, 4.5, 2, 0.1))
plot(t1, ozone_complete, 
     type = 'o', pch = 16, cex = 0.5, ylim = c(0, 170), 
     xlab = '', ylab = 'Ozone [ppb]', 
     xaxt="n", col = 'brown', 
     main = '(b) RF-filled complete ozone data series', 
     cex.lab = 1.3, cex.axis = 1.3)
MaySept = c("May","Jun", "Jul", "Aug", "Sep")
axis(side=1, at=5:9, labels = MaySept, cex.axis = 1.3)


#R plot of Fig. 9.11: A tree in a random forest 
#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)

setwd('~/climstats')
setEPS() #Plot the data of 150 observations
postscript("fig0911.eps",  width=6, height=6)
par(mar = c(0, 2, 1, 1))
iris_tree = rpart( Species ~. , data = train_data)
rpart.plot(iris_tree, 
           main = 'An decision tree for the RF training data')
dev.off()

#
#
#R code for NN recruitment decision and Fig. 9.12
TKS = c(20,10,30,20,80,30)
CSS = c(90,20,40,50,50,80)
Recruited = c(1,0,0,0,1,1)
# Here, you will combine multiple columns or features into a single set of data
df = data.frame(TKS, CSS, Recruited)

require(neuralnet) # load 'neuralnet' library
# fit neural network
set.seed(123)
nn = neuralnet(Recruited ~ TKS + CSS, data = df, 
               hidden=5, act.fct = "logistic",
               linear.output = FALSE)
plot(nn) #Plot Fig 9.12: A neural network

TKS=c(30,51,72) #new data for decision
CSS=c(85,51,30) #new data for decision
test=data.frame(TKS,CSS)
Predict=neuralnet::compute(nn,test)
Predict$net.result #the result is probability
#[1,] 0.99014936
#[2,] 0.58160633
#[3,] 0.01309036

# Converting probabilities into decisions
ifelse(Predict$net.result > 0.5, 1, 0) #threshold = 0.5
#[1,]    1
#[2,]    1
#[3,]    0

#print bias and weights
nn$weights[[1]][[1]]
#print the last bias and weights before decision
nn$weights[[1]][[2]]
#print the random start bias and weights
nn$startweights 
#print error and other technical indices of the nn run
nn$result.matrix #results data


#
#
#R plot Fig. 9.13: Curve of a logistic function
z = seq(-2, 4, len = 101)
k = 3.2
z0 = 1
setEPS() #Automatically saves the .eps file
postscript("fig0913.eps", height=5, width=7)
par(mar = c(4.2, 4.2, 2, 0.5))
plot(z, 1/(1 + exp(-k*(z - z0))),
     type = 'l', col = 'blue', lwd =2,
     xlab = 'z', ylab = 'g(z)',
     main = bquote('Logistic function for k = 3.2,'~z[0]~'= 1.0'),
     cex.lab = 1.3, cex.axis = 1.3)
dev.off()

#
#
#R NN code for the Fisher iris flower data
#Ref: https://rpubs.com/vitorhs/iris 
data(iris) #150-by-5 iris data
#attach True or False columns to iris data
iris$setosa = iris$Species == "setosa" 
iris$virginica = iris$Species == "virginica"
iris$versicolor = iris$Species == "versicolor"
p = 0.5 # assign 50% of data for training
train.idx = sample(x = nrow(iris), size = p*nrow(iris))
train = iris[train.idx,] #determine the training data
test = iris[-train.idx,] #determine the test data
dim(train) #check the train data dimension
#[1] 75  8

#training a neural network
library(neuralnet)
#use the length, width, True and False data for training 
iris.nn = neuralnet(setosa + versicolor + virginica ~ 
                      Sepal.Length + Sepal.Width + 
                      Petal.Length + Petal.Width, 
                    data = train, hidden=c(10, 10), 
                    rep = 5, err.fct = "ce", 
                    linear.output = F, lifesign = "minimal", 
                    stepmax = 1000000, threshold = 0.001)

plot(iris.nn, rep="best") #plot the neural network

#Prediction for the rest data
prediction = neuralnet::compute(iris.nn, test[,1:4])
#prediction$net.result is 75-by-3 matrix
prediction$net.result[1:3,] #print the first 3 rows
#1    1 7.882440e-13 2.589459e-42
#2    1 7.890670e-13 2.586833e-42
#6    1 7.848803e-13 2.600133e-42
#The largest number in a row indicates species

#find which column is for the max of each row
pred.idx <- apply(prediction$net.result, 1, which.max)
pred.idx[70:75] #The last 6 rows
#140 142 144 148 149 150 
#3   3   3   3   3   3 

#Assign 1 for setosa, 2 for versicolor, 3 for virginica
predicted <- c('setosa', 'versicolor', 'virginica')[pred.idx]
predicted[1:6] #The prediction result
#[1] "setosa" "setosa" "setosa" "setosa" "setosa" "setosa"

#Create confusion matrix: table(prediction,observation)
table(predicted, test$Species)
#predicted    setosa versicolor virginica
#setosa         27          0         0
#versicolor      0         19         2
#virginica       0          1        26
