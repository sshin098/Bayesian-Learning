#Assignment 1########
library(stats)
require(graphics)
library(MASS)
library(mixtools)
library(stats4)
library(kernlab)
library(expss)

data <- read.csv("SolarDataBroomApt.csv")
head (data)

#Assign data to a matrix
data <- as.matrix(read.csv("SolarDataBroomApt.csv", header= TRUE, sep=","))
#Generate a sample of 1800 data
set.seed(2881)
my.data <- data[sample(1:2881, 1800), c(1:5)]
print (my.data)
head(my.data)
write.table(my.data,"Shraddha-220506171-SolarMyData.txt")

######### (1.1) ##############

#Draw histogram and boxplot for the temperature variable
hist(my.data[ ,2], xlab= "Temperature", main = "Histogram of Temperature",col='green')

#Draw a boxplot for the temperature variable
boxplot(my.data[ ,2], xlab= "Temperature", main = "Boxplot of Temperature",col='blue',horizontal =  TRUE)
summary (my.data[ ,2])

####Five number summary for temperature ########
min <- min(my.data[ ,2], na.rm = TRUE)          #Minimum value
print (min)
max <- max(my.data[ ,2], na.rm = TRUE)          #Maximum value
print (max)

####Calculate Qaurtile 1 value for Temperature
q1 <- sort(my.data[ ,2])
lower.data <- q1[1:round(length(q1)/2)]
lower <- median(lower.data)                     #Lower value
print (lower)

####Calculate Quartile 1 value for Temperature
q3 <- sort(my.data[ ,2])
upper.data <- q3[round((length(q3)/2)+1):length(q3)]
upper <- median(upper.data)
print (upper)                                     #Upper value

###Calculate median for Temperature variable
median = median(my.data[ ,2], na.rm = TRUE)
print (median)                                    #Median value

#Five number summary for Temperature variable
fivenumber <- cbind(min, lower,median, upper, max)
colnames(fivenumber) <- c("Min", "Lower value","Median", "Upper value", "Max")
fivenumber

##############(1.3) ###################

#Plot scatter plot and linear regression for irradiance and humidity
plot(my.data[ ,1], my.data[ ,4] , xlab= "irradiance", ylab="humidity",main ="Scatter and Linear regression plot")

#Plot a linear regression line for same plot
lm(my.data[,4]~my.data[,1])
abline(lm(my.data[,4]~my.data[,1]))

#Compute the correlation coef and coef of determination
cor(my.data[ ,1], my.data[ ,4])                    #Correlation value
coefofD = cor(my.data[ ,1], my.data[ ,4])^2*100    #Coef of Determination
coefofD

############### (1.4) ##################
#Create 3 new variables
# Function to create "IrrBkt" variable
compute.irr_bucketing <- function(x){
  a = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 700){
      a[i] <- 'High'
    } else if (x[i] >= 200 && x[i] <= 700){
      a[i] <- 'Moderate'
    }
    else {
      a[i] <- 'Low'
    }
  }
  a
}

my.data <- cbind(my.data, IrrBkt = compute.irr_bucketing(my.data[,1]))
head(my.data)

# Function to create "WSBkt" variable
compute.ws_bucketing <- function(x){
  b = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 5){
      b[i] <- 'High'
    }
    else {
      b[i] <- 'Low'
    }
  }
  b
}

my.data <- cbind(my.data, WSBkt = compute.ws_bucketing(my.data[,3]))
head(my.data)

# Function to create "PreBkt" variable
compute.pr_bucketing <- function(x){
  c = array(0,length(x))
  for (i in 1:length(x)){
    if(x[i] > 1007){
      c[i] <- 'High'
    }
    else {
      c[i] <- 'Low'
    }
  }
  c
}

my.data <- cbind(my.data, PreBkt = compute.pr_bucketing(my.data[,5]))
head(my.data)


###1.4 a) Create a cross table of above three obtained variables###########
cro(my.data[,6], my.data[,7], my.data[,8])
length(my.data[,1])

####1.4b-i##############
#prob(IrrBkt is High)
(202 + 103 +192 + 12)/ 1800

######1.4-ii#############
#Prob(IrrBkt is Moderate and WsBkt is High, PreBkt is high)
56/322

####1.4b-iii########
#Prob( IrrBkt is high given that the WSBkt is low)
(103 + 12)/(739+342)


##### 3.2 c) #######
servers = c(6,8,7,11)
sum = sum(servers)
print (sum)
len = length(servers)
len
a = 10
b = 2
thta = seq(0.01, 1 , len=1000)
g = dgamma(thta, a,b)
g = g/sum(g)
print (g)
ss= dgamma(thta, sum + a, len + b)
ss = ss/sum(ss)
val = sum(ss * g)
post_thta = ss * g/val
print(post_thta)
post_mean = sum(post_thta * thta)
print(post_mean)
colors = c("blue", "red", "green")
labels = c("prior", "likelihood", "posterior")
plot(tht, post_thta, main = "Bayesian Estimation", axes = TRUE)
lines(tht, post_tht, lwd = 2, lty = c(1, 1, 1, 1, 2), col = colors[1])
lines(tht, servers1, lwd = 2, col = colors[2])
lines(tht, post_tht, lwd = 2, col = colors[3])



####Task 5

zz<-read.table("lettersdata.txt")
zz<-as.matrix(zz)
print (zz)

#### 5.a #####
plot(zz,xlab="V1",ylab="V2", main = "Scatter Plot")

#####5.b ####
set.seed(123)
Kmean <-kmeans(zz, centers=5, nstart=25)
plot(zz,col = Kmean$cluster, main="K-mean clustering with K=5")


######5.c######
TOTWSS = array( , c(20, 1))
for(i in 2:20)
{
  print(i)
  TOTWSS[i,1]= (kmeans(zz,centers=i))$tot.withinss
  print(TOTWSS[i])
  
}
plot(TOTWSS,xlab="'k' number of cluster",ylab="total within sum of squares",main="plot of totwss vs K")

#####Q5.2###########
#####Spectral clustering for zz dataset#####
sc <- specc(zz, centers=4)
sc
centers(sc) 
size(sc) 
withinss(sc)
plot(zz, col=sc)








