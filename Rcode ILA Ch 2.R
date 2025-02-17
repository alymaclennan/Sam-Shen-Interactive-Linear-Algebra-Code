################################################################
#
# Chapter 2: Matrix Theory and Visualization
#
################################################################


#Create your working directory named LinAlg and go there
setwd('/Users/sshen/LinAlg') 
getwd() #Verify that you are in the directory/folder
#[1] "/Users/sshen/LinAlg"

#R code: Computational examples of matrices
A = matrix(c(1,0,0,4,3, 2), nrow = 3, byrow = TRUE)
B = matrix(c(0,1,-1,2), nrow = 2) #form a matrix by columns
C = A%*%B #matrix multiplication
C
#[1,]    0   -1
#[2,]    4    8
#[3,]    2    1
t(C) # transpose matrix of C
#[1,]    0    4    2
#[2,]   -1    8    1

A = matrix(c(1, -1, 1, 2), nrow =2, byrow = TRUE)
A
A[,1]
A[1,]
solve(A) #compute the inverse of A
#[1,]  0.6666667 0.3333333
#[2,] -0.3333333 0.3333333
A%*%solve(A) #verify the inverse of A
#[1,] 1.000000e+00    0
#[2,] 1.110223e-16    1

#Solve linear equations
A = matrix(c(30, 40, 1, 1), 
           nrow =2, byrow = TRUE)
b = c(1000, 30)
solve(A,b)
#[1] 20 10
solve(A)%*%b #Another way to solve the equations
det(A) #compute the determinant
#[1] -10

#Dot product of two vectors
u = c(2,0,1)
v = c(2,-4,3)
t(u)%*%v

library(pracma)
dot(u,v) #for nD
cross(u,v) #only for 3D


library(Matrix)
rankMatrix(A) #Find the rank of a matrix
#[1] 2 #rank(A) = 2

#Orthogonal matrices
p = sqrt(2)/2
Q = matrix(c(p,-p,p,p), nrow=2) 
Q #is an orthogonal matrix 
#           [,1]      [,2]
#[1,]  0.7071068 0.7071068
#[2,] -0.7071068 0.7071068
Q%*%t(Q) #verify O as an orthogonal matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
det(Q) #The determinant of an orthogonal matrix is 1 or -1
#[1] 1

#Generate a 2-by-4 zero matrix 
zeroM = matrix(0, nrow = 2, ncol = 4)
zeroM
matrix(rep(0, 8), nrow =2)

#Generate a 3-by-3 identity matrix
diag(1, 3)
diag(3)

#generate a 3-by-3 diagonal matrix
diag(c(2.4, 3.1, -0.9))

#R code for eigenvectors and eigenvalues
A = matrix(c(1, 2, 2, 1), nrow=2)
eigen(A)
#$values
#[1]  3 -1
#$vectors
#[,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068

#R plot eigenvector v vs a non-eigenvector u
setwd('/Users/sshen/LinAlg')
setEPS() #Plot the figure and save the file
postscript("fig0502.eps", width = 6)
par(mar=c(4.5,4.5,2.0,0.5))
plot(9,9,
     main = 'An eigenvector vs a non-eigenvector',
     cex.axis = 1.4, cex.lab = 1.4,
     xlim = c(0,3), ylim=c(0,3),
     xlab = bquote(x[1]), ylab = bquote(x[2]))
arrows(0,0, 1,0, length = 0.25, 
       angle = 8, lwd = 5, col = 'blue')
arrows(0,0, 1,2, length = 0.3, 
       angle = 8, lwd = 2, col = 'blue',  lty = 3)
arrows(0,0, 1,1, length = 0.25, 
       angle = 8, lwd = 5, col='red') 
arrows(0,0, 3,3, length = 0.3, 
       angle = 8, lwd = 2, col='red', lty = 3)
text(1.4,0.1, 'Non-eigenvector u', cex =1.4, col = 'blue')
text(1.0,2.1, 'Au', cex =1.4, col = 'blue')
text(1.5,0.9, 'Eigenvector v', cex =1.4, col = 'red')
text(2.8, 2.95, 'Av', cex =1.4, col = 'red')
dev.off()

# Verify diagonalization and decomposition: R code
C = matrix(c(2,1,1,2), nrow = 2)
eigen(C)
#$values
#[1] 3 1
#$vectors
#  [,1]       [,2]
#[1,] 0.7071068 -0.7071068
#[2,] 0.7071068  0.7071068
Q = eigen(C)$vectors
D = t(Q)%*%C%*%Q #Matrix diagonalization
D
#[1,]    3    0
#[2,]    0    1
Q%*%D%*%t(Q) #Matrix decomposition
#[1,]    2    1
#[2,]    1    2
D[1,1]*Q[,1]%*%t(Q[,1]) + D[2,2]*Q[,2]%*%t(Q[,2])
#[1,]    2    1
#[2,]    1    2

#Hadamard product of two matrices
#install.packages('matrixcalc')
library(matrixcalc)
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 4, 6, 8 ), nrow=2, byrow=TRUE )
hadamard(A, B)
#     [,1] [,2]
#[1,]    2    8
#[2,]   18   32

#Jordan product of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
(A%*%B + B%*%A)/2
#     [,1] [,2]
#[1,]  5.5  5.5
#[2,]  9.5  7.5

#R commutator of A and B
A = matrix( c( 1, 2, 3, 4 ), nrow=2, byrow=TRUE )
B = matrix( c( 2, 1, 2, 1 ), nrow=2, byrow=TRUE )
A%*%B - B%*%A
#     [,1] [,2]
#[1,]    1   -5
#[2,]    9   -1
#install.packages('psych')
library(psych)
tr(A%*%B - B%*%A) #tr for trace
#[1] 0

#Cross product
library(pracma)
x = 1:3
y = 4:6
cross(x, y)	
#[1] -3  6 -3
dot(x, y)
#[1] 32

#Outer product of two vectors
a = 1:2
b = 1:4
a%o%b #outer product a_2-by-1 times t(b)_1-by-4
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    2    4    6    8


#Outer product of A_mn and B_nq
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Outer product of A_mn and B_pq
A = matrix(1:4, ncol = 2)
B = matrix(1:9, ncol = 3)
A%o%B
dim(A%o%B)
#[1] 2 2 2 3

#Kronecker product
library(fastmatrix)
A <- diag(1:2)
B <- matrix(1:4, ncol = 2)
kronecker.prod(A, B)
#     [,1] [,2] [,3] [,4]
#[1,]    1    3    0    0
#[2,]    2    4    0    0
#[3,]    0    0    2    6
#[4,]    0    0    4    8

# an example with vectors
ones <- rep(1, 2)
y <- 1:4
kronecker.prod(ones, t(y)) # 2-by-4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]    1    2    3    4

#Cholesky decomposition
dat = matrix(1:4, ncol = 2)
A = dat%*%t(dat)
chol(A)

#Direct sum of two matrices
A = matrix(1:4, ncol = 2)
B = matrix(1:6, ncol = 3)
A1 = rbind(A, matrix(rep(0, 4), ncol = 2))
B1 = rbind(matrix(rep(0, 6), ncol = 3), B)
C = cbind(A1, B1) #= direct sum of A and B
C
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#Express the direct sum by Kronecker products
kronecker.prod(diag(1,0), A) + kronecker.prod(diag(0,1), B)
#     [,1] [,2] [,3] [,4] [,5]
#[1,]    1    3    0    0    0
#[2,]    2    4    0    0    0
#[3,]    0    0    1    3    5
#[4,]    0    0    2    4    6

#SVD example for a 2-by-3 matrix: R code
A=matrix(c(-1,1,0,2,-2,3),nrow=2)
A #Show the 2-by-3 matrix
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3
svdA=svd(A) #Compute the SVD of A and put the results in svdA
svdA #Show SVD results: d, U, and V
round(svdA$d, digits=2) #Show only the singular values
#[1] 4.22 1.09
round(svdA$u, digits=2) #Show only matrix U
#      [,1] [,2]
#[1,] -0.48 0.88
#[2,]  0.88 0.48
round(svdA$v, digits=2)#Show only matrix V
#     [,1]  [,2]
#[1,] 0.32 -0.37
#[2,] 0.42  0.88
#[3,] 0.85 -0.29
sqrt(eigen(A%*%t(A))$values)
#[1] 4.221571 1.085514


#Data reconstruction by singular vectors: R code
round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]), 
      digits=1)
#     [,1] [,2] [,3]
#[1,] -0.7 -0.8 -1.7
#[2,]  1.2  1.5  3.2

round(svdA$d[1]*svdA$u[,1]%*%t(svdA$v[,1]) + 
        svdA$d[2]*svdA$u[,2]%*%t(svdA$v[,2]), 
      digits =2)
#     [,1] [,2] [,3]
#[1,]   -1    0   -2
#[2,]    1    2    3


#R plot schematic diagram of SVD
setwd('/Users/sshen/climstats')
setEPS() #Plot the figure and save the file
postscript("fig0503.eps", width = 11)
par(mar=c(0,0,0,0))
plot(200, axes = FALSE,
     xlab = "", ylab = "",
     xlim = c(-3,28), ylim = c(-3,16))
text(13,15.5, cex=2.2,
     bquote("SVD:" ~ A==UDV^t~ "when n > m or n < m"))
#Space-time data matrix A when n>m
segments(x0 = c(0,0,3,3),
         y0 = c(6,12,12,6) +1,
         x1 = c(0,3,3,0),
         y1 = c(12,12,6,6) +1, 
         col = c('blue','red','blue','red'),lwd =3)
segments(x0 = c(0.5,1.0),
         y0 = c(6,6)+1,
         x1 = c(0.5,1.0),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3)
text(-.8, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(1.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(2.0, 9+1, '...',  cex = 1.4)
text(2, 5+1, bquote(A[n%*%m]),  cex = 2.5)
text(5, 9+1, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(7,7,10,10),
         y0 = c(6,12,12,6)+1,
         x1 = c(7,10,10,7),
         y1 = c(12,12,6,6)+1, 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(7.5,8),
         y0 = c(6,6)+1,
         x1 = c(7.5,8),
         y1 = c(12,12)+1,
         lwd =1.3, lty = 3, col = 'blue')
text(6.2, 9+1, 'n', srt=90, col ='blue', cex = 1.4)
text(8.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(9, 9+1, '...',  cex = 1.4, col='blue')
text(8.7, 5.0+1, bquote(U[n%*%m]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(12,12,15,15),
         y0 = c(9,12,12,9)+1,
         x1 = c(12,15,15,12),
         y1 = c(12,12,9,9)+1, 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 12, y0 = 12+1, x1 = 15, y1 = 9+1, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(11.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(13.5, 12.8+1, 'm', col = 'red',  cex = 1.4)
text(14.1, 11.3+1, '0', col = 'brown',  cex = 1.4)
text(12.9, 10.0+1, '0', col = 'brown',  cex = 1.4)
text(13.9, 8.0+1, bquote(D[m%*%m]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(17,17,20,20),
         y0 = c(9,12,12,9)+1,
         x1 = c(17,20,20,17),
         y1 = c(12,12,9,9)+1, 
         col = c('red','red','red','red'), lwd =3)
segments(x0 = c(17,17),
         y0 = c(11.5,10.8)+1,
         x1 = c(20,20),
         y1 = c(11.5,10.8)+1, 
         col = c('red','red'), lty=3, lwd =1.3)
text(16.2, 10.5+1, 'm', srt=90, col ='red', cex = 1.4)
text(18.5, 12.5+1, 'm', col = 'red',  cex = 1.4)
text(19.5, 8+1, bquote((V^t)[m%*%m]),  cex = 2.5, col='red')
text(18.5, 10+1, '...',  col='red', srt=90, cex =1.4)
# Space-time data matrix B when n < m
segments(x0 = c(0,0,6,6),
         y0 = c(0,3,3,0),
         x1 = c(0,6,6,0),
         y1 = c(3,3,0,0), 
         col = c('blue','red','blue','red'), lwd =3)
segments(x0 = c(1,2,5),
         y0 = c(0,0,0),
         x1 = c(1,2,5),
         y1 = c(3,3,3),
         lwd =1.3, lty = 3)
text(-0.8, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(3, 3.8, 'm', col = 'red',  cex = 1.4)
text(3.5, 1.5, '...',  cex = 1.4)
text(3, -1.5, bquote(A[n%*%m]),  cex = 2.5)
text(8, 1.5, '=',  cex = 3)
#Spatial matrix U
segments(x0 = c(11,11,14,14),
         y0 = c(0,3,3,0),
         x1 = c(11,14,14,11),
         y1 = c(3,3,0,0), 
         col = c('blue','blue','blue','blue'), lwd =3)
segments(x0 = c(11.5,12.2),
         y0 = c(0,0),
         x1 = c(11.5,12.2),
         y1 = c(3,3),
         lwd =1.3, lty = 3, col = 'blue')
text(10.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(12.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(13.2, 1.5, '...',  cex = 1.4, col='blue')
text(12.5, -1.5, bquote(U[n%*%n]),  cex = 2.5, col= 'blue')
#Singular value diagonal matrix D
segments(x0 = c(16,16,19,19),
         y0 = c(0,3,3,0),
         x1 = c(16,19,19,16),
         y1 = c(3,3,0,0), 
         col = c('brown','brown','brown','brown'), lwd =3)
segments(x0 = 16, y0 = 3, x1 = 19, y1 = 0, lty=3,
         col = c('brown'), lwd =1.3)#diagonal line
text(15.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(17.5, 3.8, 'n', col = 'blue',  cex = 1.4)
text(18.1, 2.3, '0', col = 'brown',  cex = 1.4)
text(16.9, 1.0, '0', col = 'brown',  cex = 1.4)
text(17.5, -1.5, bquote(D[n%*%n]),  cex = 2.5, col='brown')
#Temporal matrix V
segments(x0 = c(21,21,27,27),
         y0 = c(0,3,3,0),
         x1 = c(21,27,27,21),
         y1 = c(3,3,0,0), 
         col = c('red','red','red','red'),
         lwd =3)
segments(x0 = c(21,21),
         y0 = c(2.5,1.8),
         x1 = c(27,27),
         y1 = c(2.5,1.8), 
         col = c('red','red'), lty=3, lwd =1.3)
text(20.2, 1.5, 'n', srt=90, col ='blue', cex = 1.4)
text(24, 3.8, 'm', col = 'red',  cex = 1.4)
text(24, -1.5, bquote((V^t)[n%*%m]),  cex = 2.5, col='red')
text(24, 1, '...',  col='red', srt=90, cex =1.4)
dev.off()

#R SVD analysis for the weighted SOI from SLP data
#for seven years: 2009-2015
setwd("~/LinAlg")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
Pda
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
pdaDec
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix
ptada1
#Space-time data format
ptada = t(ptada1[59:65,]) #2009-2015 data
colnames(ptada)<-2009:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada #6 year of data for two stations
#       2009 2010 2011 2012 2013 2014 2015
#Darwin  0.5 -2.3 -2.2  0.3  0.3  0.1 -0.4
#Tahiti -0.7  2.5  1.9 -0.7  0.4 -0.8 -1.3
dim(ptada)
svdptd = svd(ptada) #SVD for the 2-by-6 matrix
U=round(svdptd$u, digits=2)
U
#[1,] -0.66 0.75
#[2,]  0.75 0.66
D=round(diag(svdptd$d), digits=2)
D
#[1,]  4.7 0.00
#[2,]  0.0 1.42
V =round(svdptd$v, digits=2)
t(V)
#[1,] -0.18  0.72  0.61 -0.15 0.02 -0.14 -0.15
#[2,] -0.06 -0.06 -0.28 -0.17 0.34 -0.32 -0.82


#R SVD analysis for the weighted SOI from SLP data
#For the entire period of 1951-2015
setwd("/Users/sshen/LinAlg")
Pda<-read.table("data/PSTANDdarwin.txt", header=F)
dim(Pda) 
#[1] 65 13 #Monthly Darwin data from 1951-2015
pdaDec<-Pda[,13] #Darwin Dec standardized SLP anomalies data
Pta<-read.table("data/PSTANDtahiti.txt", header=F)
ptaDec=Pta[,13] #Tahiti Dec standardized SLP anomalies
ptada1 = cbind(pdaDec, ptaDec) #space-time data matrix

#Space-time data format
ptada = t(ptada1) #2009-2015 data
colnames(ptada)<-1951:2015
rownames(ptada)<-c("Darwin", "Tahiti")
ptada[,1:6] #6 year of data for two stations
#       1951 1952 1953 1954 1955 1956
#Darwin  1.4  0.9  0.6 -0.8  0.2 -1.6
#Tahiti  0.1 -1.1 -0.1  1.5  1.8  0.1
#df = data.frame(ptada)
dim(ptada)
#[1]  2 65
svdptd = svd(ptada) #SVD for the 2-by-65 matrix
U=round(svdptd$u, digits=2)
U
#      [,1] [,2]
#[1,] -0.75 0.66
#[2,]  0.66 0.75
D=round(diag(svdptd$d), digits=2)
D
#      [,1] [,2]
#[1,] 10.02 0.00
#[2,]  0.00 7.09
V =round(svdptd$v, digits=2)
t(V)[,1:5] #The first five year from 1951-1955
#      [,1]  [,2]  [,3] [,4] [,5]
#[1,] -0.10 -0.14 -0.05 0.16 0.10
#[2,]  0.14 -0.03  0.05 0.08 0.21

#
plot(1951:2015, -V[,1], type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-0.45, 0.45))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')

t = 1951:2015
y = -3*V[,1] + 0.3*cumsum(V[,1]) - 0.3*cumsum(V[,2])
plot(t, y, type = 'o', 
     ylab = "PC", xlab = 'Year',
     main = "Tahiti-Darwin SLP Principal Components",
     col = 'black', ylim = c(-1, 1))
lines(1951:2015, -V[,2], type = 'l', 
      lty = 2, col = 'purple')
legend(1950, 0.5, lwd = 2, c("PC1", "PC2"), col = c('blue', 'Purple'),
       lty = c(1,2), bty="n")
#El Nino samples
points(c(1982, 1997), c(-V[32,1], -V[47,1]), 
       pch = 16, col = 'red')
text(1982, -V[32,1] + 0.07, "EN", col = 'red')
text(1997, -V[47,1] + 0.07, "EN", col = 'red')
#La Nina samples
points(c(1975, 2010), c(-V[25,1], -V[60,1]), 
       pch = 16, col = 'blue')
text(1975, -V[25,1] - 0.07, "LN", col = 'blue')
text(2010, -V[60,1] - 0.07, "EN", col = 'blue')


t= 1951:2015
plot(t, cumsum(V[,1]), type = 'l', 
     ylim = c(-1,1), ylab = "Cumsum index")
lines(t, cumsum(V[,2]), 
      type = 'l', col = 'red') 
lines(t, -cumsum(V[,1]) + cumsum(V[,2]), 
      type = 'l', col = 'blue') 

#For a better computer display
library(plotly)
PC = data.frame(1951:2015, V)
colnames(PC) = c("Year", "PC1", "PC2")
plot_ly(data = PC, x = ~Year, y = ~(-PC1), 
        type = 'lines+markers') %>%
  layout(yaxis = list(title = 'PC1'))


#Minors and co-factor
minors <- function(b){
  n <- nrow(b)
  a <- matrix(NA, n, n)
  for(i in 1:n)
    for(j in 1:n)
      a[i, j] = det(b[-i, -j])
  a
}
b = matrix(c(2,3,5,6,7,1,9,4,5), 
           nrow = 3, ncol = 3)
minors(b) 
#     [,1] [,2] [,3]
#[1,]   31   -5  -32
#[2,]   21  -35  -28
#[3,]  -39  -19   -4


cofactors <- function(b) (-1)^(row(b)+col(b)) *minors(b)
cofactors(b)
#     [,1] [,2] [,3]
#[1,]   31    5  -32
#[2,]  -21  -35   28
#[3,]  -39   19   -4
#Adjoint matrix, aka, adjugate of a square matrix
A <- matrix(c(1,4,5,3,7,2,2,8,3),nrow=3,ncol=3)
A
#install.packages('RConics')
library(RConics)
B <- adjoint(A)
B
#B = det(A) * inverse of A
C = det(A)*solve(A)
C - B


#Characteristic polynomials, Cayley-Hamilton theorem, Jordon forms
A = matrix(c(1,1,0,2), nrow = 2)
A
det(A)
# [1] 2
library(pracma)
cpA = charpoly(A, info = TRUE)
cpA
#[1]  1 -3  2
cpA$cp
#[1]  1 -3  2
#1 x^2 - 3 x + 2
cpA$inv
#= solve(A)
cpA$det
#= det(A)

cpA$inv %*% A
#zapsmall(cpA$inv %*% A)
# is equal to a 2-by-2 identity matrix

#Cayley-Hamilton theorem
#cp(x) = a2 x^2 + a1 x + a0
#cp(A) = a2 A^2 + a1 A + a0 I = 0
library(expm)
A%^%2 - 3*A%^%1 + 2*diag(2) 


#Jordan normal form
#install.packages("pracma")
library(pracma)
A = matrix(c(2, 1, 0, 2), nrow = 2)
A
jordan(A)

#install.packages("pracma")
library(pracma)
A <- matrix(c(2, 2, 0, 2), nrow = 2, byrow = TRUE)
J <- jordan(A)
J
#     [,1] [,2]
#[1,]    2    1
#[2,]    0    2


#Jordan matrix
#install.packages('mcompanion')
library(mcompanion)
Jordan_matrix(4, 2)
eigen(Jordan_matrix(4, 2))
#$values
#[1] 4 4  #repeated eigenvalues 2 times

Jordan_matrix(5, 3)
eigen(Jordan_matrix(5, 3))
#$values
#[1] 5 5 5 #repeated eigenvalues 3 times

Jordan_matrix(6, 1)
Jordan_matrix(6, 4)
eigen(Jordan_matrix(6, 4))
#$values
#[1] 6 6 6 6 #repeated eigenvalues 4 times

A = Jordan_matrix(4, 3)
A
cpA = charpoly(A, info = TRUE)
cpA
#$cp
#[1]   1 -12  48 -64
library(expm)
A%^%3 - 12*A%^%2 + 48*A%^%1- 64*diag(3) 

#Cayley-Hamilton theorem

## a matrix with the above 3 blocks
Jordan_matrix(c(4, 5, 6), c(2, 3, 1))
## a matrix with a 2x2 Jordan block for eval 1 and two simple 0 eval's
m <- make_mcmatrix(eigval = c(1), co = cbind(c(1,1,1,1), c(0,1,0,0)),
                   dim = 4, len.block = c(2))
m
m.X <- cbind(c(1,1,1,1), c(0,1,0,0), c(0,0,1,0), c(0,0,0,1))
m.X
m.J <- cbind(c(1,0,0,0), c(1,1,0,0), rep(0,4), rep(0,4))
m.J
from_Jordan(m.X, m.J) # == m
m.X %*% m.J %*% solve(m.X) # == m
all(m == from_Jordan(m.X, m.J)) && all(m == m.X %*% m.J %*% solve(m.X))
## TRUE
## which column(s) in m.X correspond to 1st Jordan block?
chain_ind(1, c(2,1,1)) # c(1, 2) since 2x2 Jordan block
## which column(s) in m.X correspond to 2nd Jordan block?
chain_ind(2, c(2,1,1)) # 3, simple eval
## which column(s) in m.X correspond to 1st and 2nd Jordan blocks?
chain_ind(c(1, 2), c(2,1,1)) # c(1,2,3)
## non-contiguous subset are ok:
chain_ind(c(1, 3), c(2,1,1)) # c(1,2,4)
## split the chains into a list of matrices
chains_to_list(m.X, c(2,1,1))
m.X %*% m.J
m %*% m.X # same
all(m.X %*% m.J == m %*% m.X) # TRUE
m %*% c(1,1,1,1) # = c(1,1,1,1), evec for eigenvalue 1
m %*% c(0,1,0,0) # gen.e.v. for eigenvalue 1
## indeed:
all( m %*% c(0,1,0,0) == c(0,1,0,0) + c(1,1,1,1) ) # TRUE
## m X = X jordan.block
cbind(c(1,1,1,1), c(0,1,0,0)) %*% cbind(c(1,0), c(1,1))
m %*% cbind(c(1,1,1,1), c(0,1,0,0))


#Image analysis using SVD
#Ref: imager: an R package for image processing
#https://dahtah.github.io/imager/imager.html 

setwd("~/mathmodel")
#install.packages('imager') #run this if not installed yet
library(imager)
dat <- load.image('data/SamPhoto.png') #355 KB file size
dim(dat)
#[1] 430 460   1   3
#430 rows and 460 columns, 430*460 = 197,800 pixels
#1 photo frame, 3 RGB colors 
#If a video, 1 will become 150 frames or more

dat[1:3, 1:4,1,1]
#Show part of the daat

#plot the color figure
plot(dat, 
     xlim = c(0, 430), ylim = c(0, 460),
     main = 'A Color Photo of Sam')

#Make the photo black-and-white
graydat = grayscale(dat)
dim(graydat)
#[1] 430 460   1   1
#430 rows and 460 columns,  1 photo frame, 1 grayscale [0, 1]
#plot the gray b/w photo
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
