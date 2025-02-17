############################################################################
#
# Appendix A: A Tutorial of R and R Studio
#
############################################################################

1+4
#[1] 5  This is the result
2+pi/4-0.8
#[1] 1.985398 
x<-1
y<-2
z<-4
t<-2*x^y-z
t
#[1] -2
u=2        # "=" sign and "<-" are almost equivalent
v=3        # The text behind the "#" sign is comments
u+v
#[1] 5
sin(u*v)    # u*v = 6 in the sine function is considered radian by R 
#[1] -0.2794155

#Enter temperature data in c() 
tmax <- c(77, 72, 75, 73, 66, 64, 59)
#Show the data
tmax
#[1] 77 72 75 73 66 64 59

#Generate a sequence using different methods
seq(1,8)
seq(8)
seq(1,8, by=1)
seq(1,8, length=8)
seq(1,8, length.out =8)

#Define a function
samfctn <- function(x) x*x
samfctn(4)
#[1] 16
fctn2 <- function(x,y,z) x+y-z/2
fctn2(1,2,3)
#[1] 1.5 

#Plot temperature data
plot(1:7, c(77, 72, 75, 73, 66, 64, 59))

#More plot examples
plot(sin, -pi, 2*pi)   #plot the curve of y=sin(x) from -pi to 2 pi

square <- function(x) x*x   #Define a function
plot(square, -3,2)   # Plot the defined function

# Plot a 3D surface
x <- seq(-1, 1, length=100)
y <- seq(-1, 1, length=100)
z <- outer(x, y, function(x, y)(1-x^2-y^2))  
#outer (x,y, function) renders z function on the x, y grid
persp(x,y,z, theta=330) 
# yields a 3D surface with perspective angle 330 deg

#Contour plot
contour(x,y,z) #lined contours
filled.contour(x,y,z) #color map of contours

#Symbolic calculations by R

D(expression(x^2,'x'), 'x') 
# Take derivative of x^2 w.r.t. x 
2 * x #The answer is 2x

fx= expression(x^2,'x')  #assign a function  
D(fx,'x') #differentiate the function w.r.t. x
2 * x  #The answer is 2x

fx= expression(x^2*sin(x),'x') 
#Change the expression and use the same derivative command
D(fx,'x')
2 * x * sin(x) + x^2 * cos(x)

fxy = expression(x^2+y^2, 'x','y') 
#One can define a function of 2 or more variables
fxy #renders an expression of the function in terms of x and y
#expression(x^2 + y^2, "x", "y")
D(fxy,'x') #yields the partial derivative with respect to x: 2 * x
D(fxy,'y') #yields the partial derivative with respect to y: 2 * y

square = function(x) x^2
integrate (square, 0,1) 
#Integrate x^2 from 0 to 1 equals to 1/3 with details below
#0.3333333 with absolute error < 3.7e-15

integrate(cos,0,pi/2) 
#Integrate cos(x) from 0 to pi/2 equals to 1 with details below
#1 with absolute error < 1.1e-14

#Vectors and matrices by R

c(1,6,3,pi,-3) #c() gives a vector, considered a 4X1 column vector
#[1]  1.000000  6.000000  3.000000  3.141593 -3.000000
seq(2,6) #Generate a sequence from 2 to 6
#[1] 2 3 4 5 6
seq(1,10,2) # Generate a sequence from 1 to 10 with 2 increment 
#[1] 1 3 5 7 9
x=c(1,-1,1,-1)
x+1 #1 is added to each element of x
#[1] 2 0 2 0
2*x #2 multiplies each element of x
#[1]  2 -2  2 -2
x/2 # Each element of x is divided by 2
#[1]  0.5 -0.5  0.5 -0.5
y=seq(1,4)
x*y  # This multiplication * multiples each pair of elements 
#[1]  1 -2  3 -4
x%*%y #This is the dot product of two vectors and yields 
#     [,1]
#[1,]   -2
t(x)  # Transforms x into a row 1X4 vector
#     [,1] [,2] [,3] [,4]
#[1,]    1   -1    1   -1
t(x)%*%y #This is equivalent to dot product and forms 1X1 matrix
#     [,1]
#[1,]   -2
x%*%t(y) #This column times row yields a 4X4 matrix
#     [,1] [,2] [,3] [,4]
#[1,]    1    2    3    4
#[2,]   -1   -2   -3   -4
#[3,]    1    2    3    4
#[4,]   -1   -2   -3   -4
my=matrix(y,ncol=2) 
#Convert a vector into a matrix of the same number of elements
#The matrix elements go by column, first column, second, etc
#Commands matrix(y,ncol=2, nrow=2)  or matrix(y,2)  
#or matrix(y,2,2) does the same job
my
#     [,1] [,2]
#[1,]    1    3
#[2,]    2    4
dim(my)  #find dimensions of a matrix
#[1] 2 2
as.vector(my) #Convert a matrix to a vector, again via columns
#[1] 1 2 3 4
mx <- matrix(c(1,1,-1,-1), byrow=TRUE,nrow=2)
mx*my #multiplication between each pair of elements
#     [,1] [,2]
#[1,]    1    3
#[2,]   -2   -4
mx/my #division between each pair of elements
#     [,1]       [,2]
#[1,]  1.0  0.3333333
#[2,] -0.5 -0.2500000
mx-2*my  
#     [,1] [,2]
#[1,]   -1   -5
#[2,]   -5   -9
mx%*%my #This is the real matrix multiplication in matrix theory
#     [,1] [,2]
#[1,]    3    7
#[2,]   -3   -7
det(my) #determinant
#[1] -2
myinv = solve(my) #yields the inverse of a matrix
myinv
#     [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5
myinv%*%my #verifies the inverse of a matrix
#     [,1] [,2]
#[1,]    1    0
#[2,]    0    1
diag(my) #yields the diagonal vector of a matrix
#[1] 1 4
myeig=eigen(my) #yields eigenvalues and unit eigenvectors
myeig
myeig$values
#[1]  5.3722813 -0.3722813
myeig$vectors
#           [,1]       [,2]
#[1,] -0.5657675 -0.9093767
#[2,] -0.8245648  0.4159736
mysvd = svd(my) #SVD decomposition of a matrix M=UDV'
#SVD can be done for a rectangular matrix of mXn
mysvd$d
#[1] 5.4649857 0.3659662
mysvd$u
#           [,1]       [,2]
#[1,] -0.5760484 -0.8174156
#[2,] -0.8174156  0.5760484
mysvd$v
#           [,1]       [,2]
#[1,] -0.4045536  0.9145143
#[2,] -0.9145143 -0.4045536

ysol=solve(my,c(1,3)) 
#solve linear equations matrix %*% x = b
ysol  #solve(matrix, b)
#[1]  2.5 -0.5
my%*%ysol #verifies the solution
#     [,1]
#[1,]    1
#[2,]    3

#Simple statistics by R

x=rnorm(10) #generate 10 normally distributed numbers
x
#[1]  2.8322260 -1.2187118  0.4690320 -0.2112469  0.1870511
#[6]  0.2275427 -1.2619005  0.2855896  1.7492474 -0.1640900
mean(x)
#[1] 0.289474
var(x)
#[1] 1.531215
sd(x)
#[1] 1.237423
median(x)
#[1] 0.2072969
quantile(x)
#        0%        25%        50%        75%       100% 
#-1.2619005 -0.1994577  0.2072969  0.4231714  2.8322260 
range(x) #yields the min and max of x
#[1] -1.261900  2.832226
max(x)
#[1] 2.832226

boxplot(x) #yields the box plot of x
w=rnorm(1000)

summary(rnorm(12)) #statistical summary of the data sequence
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#-1.9250 -0.6068  0.3366  0.2309  1.1840  2.5750 

hist(w) 
#yields the histogram of 1000 random numbers with a normal distribution

#Linear regression and linear trend line
#2007-2016 data of the global temperature anomalies
#Source: NOAAGlobalTemp data
t=2007:2016
T=c(.36,.30, .39, .46, .33, .38, .42, .50, .66, .70)
lm(T ~ t) #Linear regression model of temp vs time
#(Intercept)            t  
#-73.42691      0.03673 
#Temperature change rate is 0.03673  deg C/yr or 0.37 deg C/decade
plot(t,T, type="o",xlab="Year",ylab="Temperature [deg C]",
     main="2007-2016 Global Temperature Anomalies 
     and Their Linear Trend [0.37 deg C/decade] ")
abline(lm(T ~ t), lwd=2, col="red") #Regression line

#The R packages and the datasets used in this book are 
#listed below and can be downloaded and installed first 
#before proceeding to the R codes in the rest of the book. 
#The R packages: 
#animation, chron, e1071, fields, ggplot2, lattice, 
#latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf, 
#NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR

#To load the package "animation", you can do 
#library(animation)

#You can also load all these packages in one shot
#using pacman
#install.packages("pacman")
library(pacman)
pacman::p_load(animation, chron, e1071, fields, ggplot2, lattice, 
               latticeExtra, maps, mapdata, mapproj, matrixStats, ncdf4, 
               NLRoot, RColorBrewer, rgdal, rasterVis, raster, sp, TTR)

#The zipped data file:
# https://cambridge.org/climatemathematics/data.zip

#On your computer, you can create a directory called 
#climmath under your user name. 
#The one used in the book is Users/sshen/climmath  
#You unzip the data and move the data folder under 
#the Users/sshen/climmath directory. 
#A data folder will created:  
#Users/sshen/climmath/data. 
#The data folder contains about 400 MB of data. 
#Place all the R codes in the directory  Users/sshen/climmath. 
#Then, you can run all the codes in this book after replacing sshen 
#by your user name on your own computer. 

