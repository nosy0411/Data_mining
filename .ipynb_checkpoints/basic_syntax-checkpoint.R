#matrix
y<-matrix(1:20, nrow=5, ncol=4)
cells <- c(1,26,24,68)
rnames<- c("R1", "R2")
cnames<- c("C1", "C2")
mymatrix<-matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames, cnames))

y[,4]
y[3,]
y[2:4, 1:3]

#array
array(1:50, dim=c(5,5,2))
x<-array(1:100, dim=c(5,5,2,2))
x[,,2,2]

#data frame
d<-c(1,2,3,4)
e<-c("red","white","red", NA)
f<-c(TRUE, TRUE, TRUE, FALSE)
mydata<-data.frame(d,e,f)
names(mydata)<-c("ID","Color","Passed")
mydata
mydata[2:3]
mydata[c("ID","Color")]
mydata$Passed

cities <- data.frame(
  city = c("Seattle","Washington","Chicago",
           "New York","Portland","St Louis",
           "Denver","Boston","Minneapolis","Austin",
           "Philadelphia","San Francisco","Atlanta",
           "Los Angeles","Richardson"),
  rank = c(100, 96, 94, 93, 93, 92, 90, 90, 89, 87,
           85, 84, 82, 80, 80)
)
cities

#List
l<-list(1,2,3,4,5)
l[1]
l[[1]]
a<-"Design of Experiments"
b<-c(3,4)
c<-matrix(1:70, 7, 10)
d<-list(class=a, year=b, ID=c)
d
d[[1]]
d[[2]]
d[[3]]
d$class

#Factors
eye.colors <- c("brown","blue","blue","green","brown","brown","brown")
eye.colors
eye.colors <- factor(c("brown","blue","blue","green","brown","brown","brown"))
levels(eye.colors)
eye.colors
str(eye.colors)

#Conversion
d$ID
is.matrix(d$ID)
d$ID<-as.data.frame(d$ID)
is.matrix(d$ID)
is.data.frame(d$ID)
d$ID

#Computation
A <- matrix(1:6, nrow=2, ncol=3, byrow=TRUE)
B <- matrix(7:12, 2, 3)
A
B
A*B
A/B
A[,1:2]
B[,2:3]
A[,1:2]%*%B[,2:3]

a<-c(1,3,5,7,9)
b<-seq(1,9,2)
a*b
sum(a*b)
cumsum(a)
sum(A)
colSums(A)
rowSums(A)
mean(a)
mean(A)
colMeans(A)
rowMeans(A)

#apply function
A
apply(A, 1, mean)
apply(A, 2, mean)
apply(A, 1, cumsum)
apply(A, 2, cumsum)

#Standard deviation and Variance
a<-1:10
sd(a)
var(a)
sqrt(var(a))
A<-matrix(a,5,2)
A
sd(c(A))
var(A)
cov(A)
apply(A,1,sd)
apply(A,2,sd)

#min max
a<-c(2,5,1,8,10)
min(a)
max(a)
which.min(a)
which.max(a)
A
apply(A,1,min)
apply(A,2,min)
min(A)
max(A)
A[2,2]<- -2
A[4,1]<- 100
A
min(A)
max(A)
which.min(A)
which.max(A)

#transform
A <- matrix(c(3,2,6,10,3,1), 2, 3)
A
t(A)
B <- matrix(c(4,2,7,6), 2, 2)
B
det(B)
solve(B) #inverse matrix
diag(B)
sum(diag(B))

#CSV
cities
write.csv(cities, "myCTs.csv", row.names=FALSE, col.names=TRUE)
cities2<-read.csv("myCTs.csv",header=TRUE, sep=",")
cities2

#Data1
USArrests
USArrests[c(10,20,30),]
USArrests.sub1<-USArrests[USArrests$UrbanPop>=70,]
USArrests.sub1
USArrests.sub2<-USArrests[USArrests$UrbanPop>=70&USArrests$Rape>=40,]
USArrests.sub2
USArrests.sub3 <- USArrests[USArrests$Murder>=15,]
USArrests.sub3
USArrests.sub3$ID <- 1:4
USArrests.sub3
USArrests.sub3<-USArrests.sub3[,-2]
USArrests.sub3

#Data2
Orange
sub1<-Orange[Orange$Tree==1,]
sub2<-Orange[Orange$Tree==5,]
sub1.sub2<-rbind(sub1,sub2)
sub1.sub2
sub3<-Orange[Orange$Tree==4,]
sub1.sub3<-cbind(sub1,sub3)
sub1.sub3

#Probability and R.V
runif(1)
runif(5)
floor(10*runif(1))+1
sample(10)
sample(10,6)
rnorm(1,5,1)
dnorm(3,5,1)
pnorm(6,5,1)
set.seed(1)
runif(1)

#function
f<-function(x,y) x+y
f(2,3)
f<-function(a,b) {a+b}
f(2,3)
dnorm(0,5,4)
mydnorm<-function(x, mu, sigma){
  y<-exp(-(x-mu)^2/(2*sigma^2))/(sigma*sqrt(2*pi))
  return(y)
}
mydnorm(0,5,4)

#if else
x<-5
if(x<3) print("x is less than 3") else print("x is not less than 3")
x<-0
if(x<3) print("x is less than 3") else print("x is not less than 3")

#for
x<-c(1:10)
y<-0
for(i in 1:length(x)) y<-y+x[i]
y

#while
x<-c(1:10)
y<-0
i<-1
while(i<=length(x)){
  y<-y+x[i]
  i<-i+1
}
i
y
library(MASS)
shoes
#Histogram
cnames<-colnames(iris)
for(i in 1:4){
  hist(iris[,i], breaks=10, col=i+1, main="")
  title(main=cnames[i])
}

x<-rnorm(1000)
y<-rnorm(1000)+x
summary(y)
var(y)
hist(x, col="lightblue")
plot(x,y)

#box plot
boxplot(Sepal.Length~Species, data=iris, main="Iris Data",xlab="Species", ylab="Sepal Length")
boxplot(list(SL=iris[,1], SW=iris[,2], PL=iris[,3], PW=iris[,4]))

x<-iris[,2]
boxplot(x)
y<-sort(x,decreasing=FALSE)
m<-median(y)
q1<-0.25*150 + 0.5
q3<-0.75*150 + 0.5
upper.fence<-y[q3]+1.5*(y[q3]-y[q1])
lower.fence<-y[q1]-1.5*(y[q3]-y[q1])
top.bar<-max(y[y<upper.fence])
bottom.bar<-min(y[y>lower.fence])
top.outliers<-y[y>top.bar]
bottom.outliers<-y[y<bottom.bar]

#scatter plot
pairs(iris[,-5], pch=1, col=c(2,3,4)[iris[,5]])
iris[,-5]
library(lattice)
densityplot(~Sepal.Length, data=iris)
densityplot(~Sepal.Length, groups=Species, data=iris, auto.key=TRUE)

library(nlme)
data(Alfalfa)
head(Alfalfa)
xyplot(Yield ~ Date , data = Alfalfa)
xyplot(Yield ~ Date | Block , data = Alfalfa, cex = 1.5)
xyplot(Yield ~ Date | Block , type = "o", auto.key = TRUE,
       groups = Variety, data = Alfalfa, cex = 1.5)
bwplot(yield ~ variety , data = barley,
       par.settings=list(box.rectangle=list(col="blue"),
                         box.umbrella=list(col="green"),
                         plot.symbol=list(col="red",pch=4,cex=2)), aspect=0.7)

#install package
install.packages("ggplot2")
library(ggplot2)
install.packages("e1071")
library(e1071)
install.packages("MVB")
library(MVB)
install.packages(c("dplyr", "hflights"))
library(dplyr)