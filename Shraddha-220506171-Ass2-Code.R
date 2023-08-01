####SIT 743 - Assignment 2 ########
library(igraph)              #import the library
library(ggm)                 #import the library
library(MASS)                #import the library
library(bnlearn)             #import the library
library(mixtools)            #import the library

data <- read.csv("MelbCentPedCntMarch2022.csv") #Read the csv file 
head(data)                                      #Header of the data
set.seed(2881)
###### Q1.1) Plot Histogram #########
data <- data[!is.na(data)]              #remove the NULL values
head(data)
hist(data, xlab= "Count data", main = "Histogram of Count data",col='green') #plot histogram
summary (data)                          #Summary of the data

##### Q1.2) Single Gaussian model ######
set.seed(2881)
newdata<-fitdistr(data,"Normal")     #Apply Gaussian distribution
newdata$loglik                       #Log likelihood of the data
Emt<- newdata$estimate               #Calculate estimate value of data
print (Emt)                          #Print estimate value
sdev <- getElement(Emt,"sd")         #Calculate standard deviation
mean <- getElement(Emt,"mean")       #calculate mean 
cal<-c(seq(mean-5*sdev,mean+5*sdev,1))    
Gau<-dnorm(cal,getElement(Emt,"mean"), getElement(Emt,"sd"))
Gau <- Gau[!is.na(Gau)]
newdata <- as.numeric(unlist(data))
hist(newdata ,col='red',freq=FALSE)   #Plot the histogram 
par(new=TRUE)
plot(cal, Gau , col="blue", 'l')      #Plot density 

###### Q1.3) Mixture of Gaussian model using distribution of data #####
set.seed(2881)
mixdata = normalmixEM(data,k=4)      #Mix the data where no of Gaussian is 4
print (mixdata)
plot(mixdata, which=2)               #Plot mixture of data   
mixdata$loglik                       #Calculate the log likelihood of data
mixdata$mu                           #Calculate mean
mixdata$sigma                        #Calculate standard deviation
mixdata$lambda                       #Calculate lamda value
summary (mixdata)                    #Summary of the data

#plotting the combined curve ######
c <- seq(min(data),max(data),length=5000)      #input data
x = array(0,c(5000,length(mixdata$lambda)))    #array of data
for (i in (1:length(mixdata$lambda)))          #assigning the for loop 
{ 
  x[,i] <- dnorm(c,mean=mixdata$mu[i], sd=mixdata$sigma[i]) 
}
comb=array(0,c(5000,1)) 
for (j in 1:length(mixdata$lambda)) 
{
  comb[,1]<-comb[,1] + mixdata$lambda[j]*x[,j] 
} 
lines(c,comb, col="orange", lwd=1, type="l")
par(mfrow=c(1,1))
plot(mixdata,which=2)                        #Plot the data
lines(density(data), lty=2)                  #Plotting density lines

###### Q1.4) Plot of loglikelihood of all values obtained ######

plot(mixdata$all.loglik)    #plot all the log likelihood data 

####### Q2.7a) Produce Bayesian network ##########

dsep = DAG(F ~ S, O ~ S, P ~ F + O, T ~ A, W ~ P + O + T)           #D-seperation test 
plotGraph (dsep, nodesize = 18, tcltk = FALSE, vc = "green")

###### Q2.8c) Markov blanket of dissolved Oxygen (O) ####
nodes <- "[S][A][F|S][O|S][P|F:O][T|A][W|P:O:T]"
newnode <- model2network(nodes)
MarkovBlanket <- mb(newnode, "O")           #Calculate markov blanket of dissolved oxygen
print (MarkovBlanket)                       #Print the value

plot(newnode, main = "Bayseian Network",)   #Plotting the Bayesian network 

###### Installing all the required packages #######
install.packages("BiocManager")         
BiocManager::install("Rgraphviz")
BiocManager::install("RGBL")
BiocManager::install("gRbase")
library(Rgraphviz) 
library(RBGL) 
library(gRbase) 
library(gRain) 

#Markov plot for node O
graphviz.plot(newnode, highlight = list (nodes = mb(newnode, "O"),col = "grey", fill = "green"))

####### Q4) ###########
library (bnlearn) 
# load the data. 
data("insurance")
head(insurance)
#create and plot the network structure. 
modelstring = 
  paste0("[Age][Mileage][SocioEcon|Age][GoodStudent|Age:SocioEcon]", 
         "[RiskAversion|Age:SocioEcon][OtherCar|SocioEcon][VehicleYear|SocioEcon:RiskAversion]", 
         "[MakeModel|SocioEcon:RiskAversion][SeniorTrain|Age:RiskAversion]", 
         "[HomeBase|SocioEcon:RiskAversion][AntiTheft|SocioEcon:RiskAversion]", 
         "[RuggedAuto|VehicleYear:MakeModel][Antilock|VehicleYear:MakeModel]", 
         "[DrivingSkill|Age:SeniorTrain][CarValue|VehicleYear:MakeModel:Mileage]", 
         "[Airbag|VehicleYear:MakeModel][DrivQuality|RiskAversion:DrivingSkill]", 
         "[Theft|CarValue:HomeBase:AntiTheft][Cushioning|RuggedAuto:Airbag]", 
         "[DrivHist|RiskAversion:DrivingSkill][Accident|DrivQuality:Mileage:Antilock]", 
         "[ThisCarDam|RuggedAuto:Accident][OtherCarCost|RuggedAuto:Accident]", 
         "[MedCost|Age:Accident:Cushioning][ILiCost|Accident]", 
         "[ThisCarCost|ThisCarDam:Theft:CarValue][PropCost|ThisCarCost:OtherCarCost]") 
dag = model2network(modelstring) 
graphviz.plot(dag)       #Plot the bayesian network


