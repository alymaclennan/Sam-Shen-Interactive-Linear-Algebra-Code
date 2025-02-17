################################################################
#
# Chapter 1: Basic Matrix Operations
#
################################################################


#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg')
#/Users/sshen/LinAlg
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

#Some toy examples of R
1 + 2
1 - 3

x = c(1, 2)
y = c(3, 0)
plot(x, y)
plot(x,y, xlim = c(-3, 3),
     ylim = c(-1, 4),
     col = 'blue',
     pch = 16, type = 'o',
     lty = 2)

plot(1, col = 'red' , cex = 2, pch = 3)

x = c(1, 2)
y = c(3, 2)
plot(x, y, xlim = c(-2, 2), ylim = c(-3,3),
     col = 'blue', type = 'o')

plot(sin, c(-9,9), lwd =3, col = 'purple',
     main = 'Sam\'s color plot')


A = matrix(c(1,1,1,-1), nrow=2)
A
A1 = matrix(c(1, 1, 1, -1), 
            byrow = TRUE, 
            ncol = 2, nrow = 2)
A1

A2 = matrix(1, nrow =5, ncol =4)
A2

B = matrix(c(1,3,2,0), nrow=2)
B

#Matrix subtraction
A = matrix(c(1,0,3,-1), nrow=2) 
A
#    [,1] [,2]
#[1,]    1    3
#[2,]    0   -1
#R arranges matrix entries according to column
B = matrix(c(1,3,2,4), nrow=2)
B
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
A - B
#     [,1] [,2]
#[1,]    0    1
#[2,]   -3   -5

#Dot product
#install.packages('geometry')
library(geometry)
a = c(1, 1)
b = c(1, 0)
# Calculating dot product using dot()
dot(a, b)

#Another way to compute dot product is to 
a%*%b
#[,1]
#[1,]    1

#Calculate the angle between two vectors
#Euclidean norm or called amplitude of a vector 
am = norm(a, type="2")  
am
#[1] 1.414214
bm = norm(b, type="2") 
bm
#[1] 1
#Angle in deg between two vectors
angleab = acos(dot(a, b)/(am*bm))*180/pi
angleab
#[1] 45 degrees

#Cross product of two 3D vectors
library(pracma)
x = 1:3
y = 4:6
cross(x, y)	
#[1] -3  6 -3
dot(x, y)
#[1] 32

#Scalar times a matrix
A = matrix(c(1,0,3,-1), nrow=2) 
A
#    [,1] [,2]
#[1,]    1    3
#[2,]    0   -1

3*A
#     [,1] [,2]
#[1,]    3    9
#[2,]    0   -3

a
A
b*A
b

#Matrix multiplication by command %*%
A = matrix(c(1,0,3,-1), nrow=2) 
B = matrix(c(1,3,2,4), nrow=2)
A%*%B
#     [,1] [,2]
#[1,]   10   14
#[2,]   -3   -4

B%*%A
#     [,1] [,2]
#[1,]    1    1
#[2,]    3    5

#Matrix transpose
C = matrix(c(1,2,3,4), ncol =2, byrow=T)
C
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
t(C)
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4

#Generate a diagonal matrix
D = diag(c(2, 1, -3))
round(D, digits = 0)
#     [,1] [,2] [,3]
#[1,]    2    0    0
#[2,]    0    1    0
#[3,]    0    0   -3

#Generate a 3-dimensional identity matrix
I = diag(3)
I
#     [,1] [,2] [,3]
#[1,]    1    0    0
#[2,]    0    1    0
#[3,]    0    0    1

#Generate a 2-by-3 zero matrix
M = matrix(0, 2, 3)
M
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    0    0    0

#Compute the inverse of a matrix
A = matrix(c(1,1,1,-1), nrow=2)
invA = solve(A)
invA
#     [,1] [,2]
#[1,]  0.5  0.5
#[2,]  0.5 -0.5

#example: 9/9/2024 class
par(mfrow = c(1, 2))
H = matrix(rnorm(64), nrow = 8)
image(H)
G = solve(H)
image(G)
dev.off()

filled.contour(H, grid())

#Verify the inverse
invA %*% A
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1

###9/9/2024 class
m = 9
n = 8
A = matrix(2, m, n)
A

A = matrix(2, nrow = 9, ncol = 8)
A #print A
A = matrix(1:72, nrow = 9, ncol = 8)
A #print A
#visualize a amtrix
image(A)

rnorm(4000)
hist(rnorm(4000), breaks = 99)

A = matrix(rnorm(800*1000), nrow = 800, ncol = 1000)
A #print A
image(A)


#Delete rows or/and columns
A = matrix(1:9, nrow = 3)
A
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8
#[3,]    3    6    9
image(A)

A[-3,]
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

A[,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8
#[3,]    6    9
A[-3,-1]
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, 2:3] #Sub-matrix
#     [,1] [,2]
#[1,]    4    7
#[2,]    5    8

A[1:2, ] #keep all the columns
#     [,1] [,2] [,3]
#[1,]    1    4    7
#[2,]    2    5    8

#Insert a row or column to a matrix
A = matrix(1:4, nrow = 2)
br = 5:6
bc = 7:8
rbind(A, br)
#   [,1] [,2]
#1    3
#2    4
#br    5    6

rbind(br, A)
#   [,1] [,2]
#br    5    6
#      1    3
#      2    4

rbind(rbind(A[1,], br), A[2,])
#   [,1] [,2]
#      1    3
#br    5    6
#      2    4
Abc = cbind(A, bc)
Abc
Abc = cbind(A, bc)
#         bc
#[1,] 1 3  7
#[2,] 2 4  8

cbind(Abc, A)#stack two matrices
#         bc    
#[1,] 1 3  7 1 3
#[2,] 2 4  8 2 4


#
#
#Row or column statistics
library(matrixStats)
A = matrix(1:6, nrow = 2)
A
rowSums(A)
#[1]  9 12
rowMeans(A)
#[1] 3 4

library(matrixStats)
rowCumsums(A) #cumulative sum
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    2    6   12
colMeans(A)
#[1] 1.5 3.5 5.5
#install.packages('matrixStats')
library(matrixStats) #SD needs the library
rowSds(A) 
#[1] 2 2
colSds(A)
#[1] 0.7071068 0.7071068 0.7071068

#Sweep a matrix by a vector using subtraction
A = matrix(1:6, nrow=2, byrow = TRUE)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    4    5    6
u = 1:3
u
Br = sweep(A, 2, u) #2 means sweep every row
Br
#     [,1] [,2] [,3]
#[1,]    0    0    0
#[2,]    3    3    3
v= 1:2
Bc = sweep(A, 1, v) #1 means sweep every column
Bc
#     [,1] [,2] [,3]
#[1,]    0    1    2
#[2,]    2    3    4

c = colMeans(A) #means of each column
sweep(A, 2, c) #anomaly data matrix
#[1,] -1.5 -1.5 -1.5
#[2,]  1.5  1.5  1.5

A

sin(A)#function operation on each matrix element
#           [,1]       [,2]       [,3]
#[1,]  0.8414710  0.9092974  0.1411200
#[2,] -0.7568025 -0.9589243 -0.2794155

A^2 #not equal to A%*%A
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]   16   25   36

#Sweep a matrix by a vector using multiplication
A = matrix(1:6, nrow=2, byrow = TRUE)
w = 1:2
A
w
w*A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]    8   10   12
A*w # yields the same result as w*A
w3 = 1:3
#sweep each row by transposing A
t(w3*t(A))
#     [,1] [,2] [,3]
#[1,]    1    4    9
#[2,]    4   10   18
w3*A #multiplication sweep by row-dimensions not matching
#     [,1] [,2] [,3]
#[1,]    1    6    6
#[2,]    8    5   18

A/w #sweeping by division
#     [,1] [,2] [,3]
#[1,]    1  2.0    3
#[2,]    2  2.5    3


#Conversions between a Vector and a Matrix
v = c(60, 58, 67, 70, 55, 53)
M = matrix(v, nrow = 2) #from vector to matrix
M
#     [,1] [,2] [,3]
#[1,]   60   67   55
#[2,]   58   70   53
c(M) #from matrix to vector by column
#[1] 60 58 67 70 55 53
c(t(M)) #from matrix to vector by row
#[1] 60 67 55 58 70 53

#Reduce the dimension of an nD array
x <- array(1:(2*3*4), dim=c(2,3,4))
dim(x)
#[1] 2 3 4
x #a stack of four 2-by-3 matrices
#, , 1 #the first of the 3rd dim index

#[,1] [,2] [,3]
#[1,]    1    3    5
#[2,]    2    4    6
# ...
#install.packages('R.utils')
library(R.utils)
#flat all the other dim except the 3rd one
#flat the 1st and 2nd dim
y <- wrap(x, map=list(3, NA)) 
dim(y)
#[1] 4 6
y
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    2    3    4    5    6
#[2,]    7    8    9   10   11   12
#[3,]   13   14   15   16   17   18
#[4,]   19   20   21   22   23   24

#back to the original 3D array
array(t(y), dim = c(2,3,4))


#Solve linear equations
A = matrix(c(1,1,1,-1), nrow = 2)
b = c(20,4)
solve(A, b)
#[1] 12  8  #This is the result x1=12, and x2=8.

A = matrix(c(1,2,-1,4),nrow =2)
A
res = eigen(A)
res$values
res$vectors

B = matrix(rnorm(6), nrow = 2)
B
svd(B)

#Spatial covariance matrix
dat = matrix(c(0,-1,1,2,3,4), nrow=3)
dat
colMeans(dat)
A = sweep(dat, 2, colMeans(dat))
A
#     [,1] [,2]
#[1,]    0   -1
#[2,]   -1    0
#[3,]    1    1
covm=(1/(dim(A)[2]))*A%*%t(A)
covm #is the covariance matrix.
#     [,1] [,2] [,3]
#[1,]  0.5  0.0 -0.5
#[2,]  0.0  0.5 -0.5
#[3,] -0.5 -0.5  1.0

u = c(1, 1, 0)
v = covm %*% u
v
#     [,1]
#[1,]  0.5
#[2,]  0.5
#[3,] -1.0
#u and v are in different directions

#Eigenvectors of a covariance matrix 
ew = eigen(covm)
ew
#$values
#[1] 1.500000e+00 5.000000e-01 1.332268e-15

#$vectors
#           [,1]          [,2]      [,3]
#[1,] -0.4082483 -7.071068e-01 0.5773503
#[2,] -0.4082483  7.071068e-01 0.5773503
#[3,]  0.8164966  8.881784e-16 0.5773503
#This is the first eigenvector

#Verify the eigenvectors and eigenvalues
covm%*%ew$vectors[,1]/ew$values[1]
#           [,1]
#[1,] -0.4082483
#[2,] -0.4082483
#[3,]  0.8164966

w = ew$vectors[,1] # is an eigenvector

#R code for SVD
#Develop a 2-by-3 space-time data matrix for SVD
A=matrix(c(1,-1,2,0,3,1),nrow=2)
A
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#Perform SVD calculation
msvd=svd(A)
msvd
msvd$d
#[1] 3.784779 1.294390
msvd$u
#          [,1]       [,2]
#[1,] -0.9870875 -0.1601822
#[2,] -0.1601822  0.9870875
msvd$v
#           [,1]       [,2]
#[1,] -0.2184817 -0.8863403
#[2,] -0.5216090 -0.2475023
#[3,] -0.8247362  0.3913356
#One can verify that A=UDV', where V' is transpose of V.
verim=msvd$u%*%diag(msvd$d)%*%t(msvd$v)
verim
#     [,1]         [,2] [,3]
#[1,]    1 2.000000e+00    3
#[2,]   -1 1.665335e-16    1
round(verim)
#     [,1] [,2] [,3]
#[1,]    1    2    3
#[2,]   -1    0    1
#This is the original data matrix A

covm = (1/(dim(A)[2]))*A%*%t(A)
eigcov = eigen(covm)
eigcov$values
#[1] 4.7748518 0.5584816
eigcov$vectors
#           [,1]       [,2]
#[1,] -0.9870875  0.1601822
#[2,] -0.1601822 -0.9870875

((msvd$d)^2)/(dim(A)[2])
#[1] 4.7748518 0.5584816
eigcov$values
#[1] 4.7748518 0.5584816

x1=c(1,2,3) #Given the coordinates of the 3 points
x2=c(2,1,3)
y=c(-1,2,1)
df=data.frame(x1,x2,y) #Put data into the data.frame format
fit <- lm(y ~ x1 + x2, data=df)
fit#Show the regression results
#Call:
#  lm(formula = y ~ x1 + x2, data = df)
#Coefficients:
#  (Intercept)           x1           x2  
#-5.128e-16    1.667e+00   -1.333e+00  

1.667*x1-1.333*x2  #Verify that 3 points determining a plane
#[1] -0.999  2.001  1.002


#Multilinear regression
u=c(1,2,3,1)
v=c(2,4,3,-1)
w=c(1,-2,3,4)
mydata=data.frame(u,v,w)
myfit <- lm(w ~ u + v, data=mydata)
summary(myfit)#Show the result
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)   1.0000     1.8708   0.535    0.687
#u             2.0000     1.2472   1.604    0.355
#v            -1.5000     0.5528  -2.714    0.225

#Multilinear regression example for more data
dat=matrix(rnorm(40),nrow=10, dimnames=list(c(letters[1:10]), c(LETTERS[23:26])))
fdat=data.frame(dat)
fit=lm(Z~ W + X + Y, data=fdat)
summary(fit)

#Coefficients\index{linear regression!coefficients}
#            Estimate Std. Error t value Pr(>|t|)  
#(Intercept)  0.36680    0.16529   2.219   0.0683 
#W            0.11977    0.20782   0.576   0.5853  
#X           -0.53277    0.19378  -2.749   0.0333 
#Y           -0.04389    0.14601  -0.301   0.7739


#Image analysis using SVD
#Ref: imager: an R package for image processing
#https://dahtah.github.io/imager/imager.html 

setwd("~/LinAlg")
#install.packages('imager') #run this if not installed yet
library(imager) 
dat <- load.image('data/SamPhoto.png') #355 KB file size
dim(dat)
#[1] 430 460   1   3
#430 rows and 460 columns, 430*460 = 197,800 pixels
#1 photo frame, 3 RGB colors 
#If a video, 1 will become 150 frames or more

dat[1:3,1:4,1,1]

image(dat[, , 1, 3])
#Show part of the data

#plot the color figure
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'A Color Photo of Sam')

#Make the photo black-and-white
#dat = dat[,,1,1:3]
graydat = grayscale(dat)
dim(graydat)
#[1] 430 460   1   1
#430 rows and 460 columns,  1 photo frame, 1 grayscale [0, 1]
#plot the gray b/w photo
graydat[1:3, 1:5, 1, 1]
image(graydat[,,1,1])

graydat1 = graydat[,,1,1] - 
  0.1*matrix(rchisq(430*460, df =5), nrow = 430)
image(graydat1)

plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Gray Sam')

#Plot the color and b/w photos together
par(mfrow = c(1, 2))
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'Color Sam')
plot(graydat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'B/W Sam')
dev.off()

#SVD analysis of the grayscale data
svdDat = svd(graydat)
SVDd = svdDat$d
percentD = 100*(SVDd^2)/sum(SVDd^2)
cumpercentD = cumsum(percentD)
modeK = 1:length(SVDd)
dev.off()
plot(modeK[1:20], percentD[1:20], 
     type = 'o', col = 'blue',
     xlab = 'Mode number', pch = 16,
     ylab = 'Percentage of mode variance',
     main = 'Scree Plot of SVD B/W Photo Data')
K = 20
lam = (svdDat$d)^2
lamK=lam[1:K]
lamK
#setEPS() #Plot the figure and save the file
#postscript("fig0608.eps", width = 6, height = 4)
par(mar=c(4,4,2,4), mgp=c(2.2,0.7,0))
plot(1:K, 100*lamK/sum(lam), ylim=c(0,100), type="o", 
     ylab="Percentage of Variance [%]",
     xlab="EOF Mode Number", 
     cex.lab=1.2, cex.axis = 1.1, lwd=2, 
     main="Scree Plot of the First 20 Eigenvalues")
legend(3,30, col=c("black"),lty=1, lwd=2.0,
       legend=c("Percentange Variance"),bty="n",
       text.font=2,cex=1.0, text.col="black")
par(new=TRUE)
plot(1:K,cumsum(100*lamK/sum(lam)),
     ylim = c(90,100), type="o",
     col="blue",lwd=2, axes=FALSE,
     xlab="",ylab="")
legend(3,94.5, col=c("blue"),lty=1,lwd=2.0,
       legend=c("Cumulative Percentage Variance"),bty="n",
       text.font=2,cex=1.0, text.col="blue")
axis(4, col="blue", col.axis="blue", mgp=c(3,0.7,0))
mtext("Cumulative Variance [%]",col="blue", 
      cex=1.2, side=4,line=2)

#Reconstructing b/w photo from SVD modes
U = svdDat$u
V = svdDat$v
D = diag(svdDat$d)
dim(D)

#100% reconstruction using all the modes
recon = U%*%D%*%t(V)
dim(recon)
#[1] 430 460
image(recon)

dev.off()
par(mfrow = c(2, 2))
kR = 430 #Recon from all 413 modes
R430 = as.cimg(U[,1:kR]%*%D[1:kR, 1:kR]%*%t(V[, 1:kR]))
plot(R430, main = "All 430 modes")
kB = 20 #The first 20 modes
R20 = as.cimg(U[,1:kB]%*%D[1:kB, 1:kB]%*%t(V[, 1:kB]))
plot(R20, main = "The first 20 modes")
k = 3 #Recon from the first 3 modes
R3 = as.cimg(U[,1:k]%*%D[1:k, 1:k]%*%t(V[, 1:k]))
plot(R3, main = "The first 3 modes")
k1 = 21; k2 = 100 #Recon from 21st to 100th modes
Rk1_k2 = as.cimg(U[,k1:k2]%*%D[k1:k2, k1:k2]%*%t(V[, k1:k2]))
plot(Rk1_k2, main = "21st to 100th modes")
dev.off()

#Three monotone photos and their color photo
dim(dat) #4D array of a color photo
#[1] 430 460   1   3
dev.off()
par(mfrow = c(2, 2))
R = as.cimg(apply(t(dat[,, 1, 1]), 1, rev))
plot(R, main = 'Monotone color R')
G = as.cimg(apply(t(dat[,, 1, 2]), 1, rev))
plot(G, main = 'Monotone color G')
B = as.cimg(apply(t(dat[,, 1, 3]), 1, rev))
plot(B, , main = 'Monotone color B')
#Bind the three monotone photos into one 
trippy = imappend(list(R,G,B), "c") 
dim(trippy) #color figure data
#[1] 430 460   1   3
plot(trippy, main ='Blend RGB Colors')
dev.off()
