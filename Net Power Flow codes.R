##----Analysis for Positive and Negative Net Power flows----------
### Peak2Average ratio of Positive & Negative Net power Flows as a function of Battery size

setwd("C:/Users/Ewa/Desktop/storage model")
data <- read.csv("1169.csv", header=TRUE, na.strings = "")

data <- data[1:525600,] #needed for house no 1169 
mydata <- data
mydata <- na.omit(mydata)
mydata[!(mydata$gen>=0), "gen"] <- 0.0
x <- sum(mydata$use)/sum(mydata$gen) #or meanD/meanS

mydata$gen <- mydata[,4]*x #for P (Gen/Demand) = 1
mean(mydata$gen)==mean(mydata$use)

pos <- subset(mydata[mydata$gen-mydata$use >= 0,])
neg <- subset(mydata[mydata$gen-mydata$use < 0,])

NetPowerFlowPos <- pos$gen - pos$use #Vector wit Positive (+ and 0) Net Power Flows
NetPowerFlowNeg <- abs(neg$gen - neg$use)

PARnetFlowPos <- max(NetPowerFlowPos)/mean(NetPowerFlowPos) #Peak-to-average ratio of Positive Net FLow for scenario without Storage
PARnetFlowNeg <- max(NetPowerFlowNeg)/mean(NetPowerFlowNeg) #PAR of Negative for B=0

S <- mydata$gen
D <- mydata$use


T <- (60*24*365)
b <- rep(0, T)
L <- rep(0, T)
W <- rep(0, T)
T_u <- 1/60 # Time unit (hour)
Battery <- seq(0, 10, by=1) #Sizes of storage to be examined in the loop
PAR_net_values <- rep(0, length(Battery)) #Declaring vector for PA ratios for positive flows (needed for the loop)
PAR_net2_values <- rep(0, length(Battery)) #Declaring vector for PAR for negative flows
PAR_net_values[1] <- PARnetFlowPos #assignment of the PAR positive flow for B=0 (computed above)
PAR_net2_values[1] <- PARnetFlowNeg #assignment of the PAR negative flow for B=0 (computed above)

length.pos <- rep(0,11) #declaring vector for counts of positive net flow times per scenario
length.neg <- rep(0, 11) #declaring vector for counts of negative net flow times per scenario
length.pos[1] <- nrow(pos) #for the first value (case B=0) there are already counts from pos & neg
length.neg[1] <- nrow(neg)

## Loop for Net Power Flows depending on the battery size
for (k in 2:length(Battery)) {
  
  B <- Battery[k] # Battery size (KWh)
  NetFlowPos <- rep(-1, T) # I set -1 as NA for the net flow vectors to keep is numeric (all the values are >=0 so this would be easily excluded)
  NetFlowNeg <- rep(-1, T) #as above, since all the values are >0, -1 will be easily excluded.
                            #Declaring the size of the vectors in advance substantially speeds the loop.
  
  
  b[1] <- B  # Initializing the SoC of the battery (KWh)
  
  # Computing battery energy content
  for (i in 2:T){
    b[i] = b[i-1] + (S[i] - D[i]) * T_u
    
    ## If the net flow is positive I attach value to the Positive vector and -1 to negative one.
    if (S[i] + (b[i-1] / T_u) - D[i]>=0) {
      NetFlowPos[i] = S[i] + (b[i-1] / T_u) - D[i]
      NetFlowNeg[i] = -1}
    
    #Otherwise, I assign abs of negative flow to negative vector, and -1 to positive one.
    else {
      NetFlowNeg[i] = abs(S[i] + (b[i-1] / T_u) - D[i])
      NetFlowPos[i] = -1
    }
    
    if (b[i] > B){
      W[i] = (b[i] - B) / T_u
      b[i] = B
      L[i] = 0
      
    } else if(b[i] < 0) {
      L[i] = - b[i] / T_u
      b[i] = 0
      W[i] = 0  
      
    }else{
      W[i] = 0
      L[i] = 0
    }
  }
  NetFlowPos <- NetFlowPos[2:T] #excluding 1st row cause the loop starts from i==2
  NetFlowNeg <- NetFlowNeg[2:T] #as above
  NetFlowPos <- NetFlowPos[!(NetFlowPos==-1)] #excluding NA's to have a proper size of the vector
  NetFlowNeg <- NetFlowNeg[!(NetFlowNeg==-1)] #excludiing NA's for the proper size
  
  length.pos[k] <- length(NetFlowPos) #length as count of Positive net flow Times (events)
  length.neg[k] <- length(NetFlowNeg) #length as count of Negative flow times
  
  PAR_net_values[k] <- max(NetFlowPos)/mean(NetFlowPos) #collecting peak2ave ratio for positive for the given iteration
  PAR_net2_values[k] <- max(NetFlowNeg)/mean(NetFlowNeg) #collecting p2a r for negative
}
library(ggplot2)
len <- as.data.frame(cbind(Battery, length.pos, length.neg))
ggplot(len, aes(x=Battery))+geom_line(aes(y=length.pos, colour="Positive flows"))+geom_line(aes(y=length.neg, colour="Negative flows"))

r <- cbind(Battery, PAR_net_values, PAR_net2_values)
r <- as.data.frame(r)
ggplot(r, aes(x=Battery, y=PAR_net_values)) + geom_line() + ggtitle("Peak-to-average Positive Net Power Flow as a function of battery size")
ggplot(r, aes(x=Battery, y=PAR_net2_values)) + geom_line() + ggtitle("Peak-to-average ABS Negative Net Power Flow as a function of battery size")

plot(x=c(1:5000), y=mydata$gen[1:5000]-mydata$use[1:5000], type = "l", main = "Net Power Flow")

###--------- Peak2Average ratio for Net Power flow in Absolute values and in Raw values as a function of B size

### The following analysis is conducted based on the whole dataset and in the first case I take absolute values of
### all the flows, while in the second I take the Net flows without changing the sign.


NetPowerFlow <- abs(mydata$gen - mydata$use)
NetPowerFlow2 <- mydata$gen - mydata$use

PARnetFlow <- max(NetPowerFlow)/mean(NetPowerFlow)
PARnetFlow2 <- max(NetPowerFlow2)/mean(NetPowerFlow2)

S <- mydata$gen
D <- mydata$use
mean(S)==mean(D)

T <- (60*24*365)
b <- rep(0, T)
L <- rep(0, T)
W <- rep(0, T)
T_u <- 1/60 # Time unit (hour)
Battery <- seq(0,3, by=0.1)
PAR_net_values <- rep(0, length(Battery)) #declaring vectors to collect p2a ratios for each iteration
PAR_net2_values <- rep(0, length(Battery))
PAR_net_values[1] <- PARnetFlow
PAR_net2_values[1] <- PARnetFlow2

netFlows_iterations <- data.frame(NetPowerFlow2) #Creating data frame to collect all the net flows aver T for each iteration

##
for (k in 2:length(Battery)) {
  
  B <- Battery[k] # Battery size (KWh)
  NetFlow <- rep(0, T)
  NetFlow2 <- rep(0, T)
  
  b[1] <- B  # Initializing the SoC of the battery (KWh)
  
  # Computing battery energy content
  for (i in 2:T){
    b[i] = b[i-1] + (S[i] - D[i]) * T_u
    NetFlow[i] = abs(S[i] + (b[i-1] / T_u) - D[i])
    NetFlow2[i] = S[i] + (b[i-1] / T_u) - D[i]
    
    if (b[i] > B){
      W[i] = (b[i] - B) / T_u
      b[i] = B
      L[i] = 0
     
    } else if(b[i] < 0) {
      L[i] = - b[i] / T_u
      b[i] = 0
      W[i] = 0  
      
    }else{
      W[i] = 0
      L[i] = 0
    }
  }
  PAR_net_values[k] <- max(NetFlow[2:T])/mean(NetFlow[2:T])
  PAR_net2_values[k] <- max(NetFlow2[2:T])/mean(NetFlow2[2:T])
  netFlows_iterations <- cbind.data.frame(netFlows_iterations, NetFlow2)
}

library(ggplot2)
r <- cbind(Battery, PAR_net_values, PAR_net2_values)
r <- as.data.frame(r)
ggplot(r, aes(x=Battery, y=PAR_net_values)) + geom_line() + ggtitle("Peak-to-average Ratio Absolute Net Power Flow as a function of battery size")
ggplot(r, aes(x=Battery, y=PAR_net2_values)) + geom_line() + ggtitle("Peak-to-average Ratio Net Power Flow as a function of battery size")

avFlows <- apply(netFlows_iterations, 2, mean)
plot(x=Battery, y=avFlows, type = "l", xlab ="Battery size", ylab = "Average Net flow value", main = "Average Net Flow value as a function of Battery size")
maxflows <- apply(netFlows_iterations, 2, max)
plot(Battery, maxflows, type="l")
peaksRatios <- maxflows/avFlows
c <- data.frame(maxflows, avFlows, Battery)
ggplot(c, aes(Battery)) +geom_line(aes(y=maxflows, colour="MAx" )) + geom_line(aes(y=avFlows, colour="Average")) 
#+ geom_line(aes(y=maxflows/avFlows, colour="ratio"))


for (l in 1:length(netFlows_iterations)) {
  
  plot(x=c(1:20000), y=netFlows_iterations[1:20000, l], type = "l", xlab ="Time", ylab = "Net Power Flow (KW)", main = c("Power Net flow for battery size ", Battery[l], "KWh"))
}


#####----------Peak 2 Average Ratio of Generation as a function of Battery size

PARgen <- max(mydata$gen)/mean(mydata$gen)
PARuse <- max(mydata$use)/mean(mydata$use)
S <- mydata$gen
D <- mydata$use


T <- (60*24*365)
b <- rep(0, T)
L <- rep(0, T)
W <- rep(0, T)
T_u <- 1/60 # Time unit (hour)
Battery <- seq(0, 3, by=.2)
PAR_gen_values <- rep(0, 11)

PAR_gen_values[1] <- max(S)/mean(S)

for (k in 2:length(Battery)) {
  
  B <- Battery[k] # Battery size (KWh)
  S_prim <- mydata$gen #Declaring vector for new values of S (S+b)

  
  b[1] <- B  # Initializing the SoC of the battery (KWh)
  
  # Computing battery energy content
  for (i in 2:T){
    b[i] = b[i-1] + (S[i] - D[i]) * T_u
    S_prim[i]=S_prim[i] + (b[i-1] / T_u) #Updating S values with the b SOC as storage is a kinda source of generation
                                          #since it stores alreagy generated energy 
    
    if (b[i] > B){
      W[i] = (b[i] - B) / T_u
      b[i] = B
      L[i] = 0

      
    } else if(b[i] < 0) {
      L[i] = - b[i] / T_u
      b[i] = 0
      W[i] = 0  
      
    }else{
      W[i] = 0
      L[i] = 0
    }
  }
  PAR_gen_values[k] <- max(S_prim[2:T])/mean(S_prim[2:T])
}

library(ggplot2)
r <- cbind(Battery, PAR_gen_values)
r <- as.data.frame(r)
ggplot(r, aes(x=Battery, y=PAR_gen_values)) + geom_line() + ggtitle("Peak-to-average ratio of Generation as a function of battery size")
