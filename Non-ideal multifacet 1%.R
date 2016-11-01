
setwd("C:/Users/IEUSer/Dropbox/storage model")
data <- read.csv("1169.csv", header=TRUE, na.strings = "")

data <- data[1:525600,] #needed for house no 1169 
mydata <- data
mydata <- na.omit(mydata)
mydata[!(mydata$gen>=0), "gen"] <- 0.0
T <- (60*24*365)
T_u <- 1/60 # Time unit (hour)

ratio <- sum(mydata$gen)/(sum(mydata$use))
#x <- 1/ratio # P = 1
x_a <- 1.1/ratio # P = 1.1
x_b <- 1.2/ratio # P = 1.2
x_c <- 1.4/ratio # P = 1.4
x_d <- 1.6/ratio # P = 1.6
x_e <- 1.8/ratio # P= 1.8
x_f <- 2/ratio # P = 2
x_g <- 3/ratio # P = 3
x_h <- 4/ratio # P = 4
x_i <- 5/ratio # P = 5
x_j <- 6/ratio # P = 6



y <- c(x_a, x_b, x_c, x_d, x_e, x_f, x_g, x_h, x_i, x_j)
results <- data.frame(LOLP=rep(0, 10000),freq=rep(0, 10000), Pratio=rep(0, 10000), Storage=rep(0, 10000), chargeNum=rep(0,10000), charge_amount=rep(0, 10000))

####Analysis Parameters to be examined 

t_control <- c(7*24*60, 30*24*60, 90*24*60, 262800)
charge_num <- c(52, 12, 4, 1)

P <- c(1.1, 1.2, 1.4, 1.6, 1.8, 2, 3, 4, 5, 6)

LOLP_target = 0
delta = 1
counter = 0

eta_c <- 0.95 # Charge inefficiency
eta_d <- 0.95 # Disharge inefficiency
DoD <- 0.8 # Depth of discharge


for (l in 1:length(t_control)) {
  
  for (m in 1:length(y)) {
    
    S <- mydata$gen*y[m]
    D <- mydata$use
    
    #Ideal input and output power to the storage
    I <- pmax(rep(0,T), S[1:T] - D[1:T])
    O <- pmax(rep(0,T), D[1:T] - S[1:T])
    
    
    
    ## Loop for Net Power Flows depending on the battery size
    
    LOLP <- 1.0
    B <- -delta
    
    while (LOLP > LOLP_target){
      LOLP=0
      B=B+delta 
      count = 0
      charge_amount = 0
      charging = 0
      
      alpha_c <- B * 0.25 # Maximum charge power (KW) 
      alpha_d <- B * 1 # Maximum discharge power (KW) 
      NCharg <-  pmin(rep(alpha_c,T), I) * rep(eta_c * T_u,T) 
      NDisCh <- pmin(rep(alpha_d,T), O) * rep(T_u / eta_d, T)
      Loss <- pmax(0, O - rep(alpha_d,T))
      
      B_eff <- DoD * B
      b <- numeric(T) #Declaring storage energy content
      L <- numeric(T)  #Declaring Loss of load vector in each month
      b[1] <- B_eff  # Initializing the SoC of the battery
      
      # Computing battery energy content
      for (i in 2:T){
        
        b[i] = b[i-1] + NCharg[i] - NDisCh[i];
        b[i] = min(B_eff, b[i])
        if (b[i]<(0.2*B) & b[i]>0) {
          L[i] = Loss[i] + (min(b[i],0.2*B) / T_u)
        }
        else {
          L[i] = Loss[i] - (min(b[i],0) / T_u)}
        b[i] = max(b[i],0.2*B)
        
        if (b[i]<((0.2*B)+(0.01*B_eff)) & charging < charge_num[l]) {
          charge_amount = charge_amount + B_eff - b[i]
          b[i]=B_eff
          charging = charging + 1
        }
      }
      count = L[L>0]
      LOLP = length(count) / T 
      counter = counter+1
      results$LOLP[counter] <- LOLP
      results$Storage[counter] <- B
      
      results$freq[counter] <- t_control[l]
      results$Pratio[counter] <- P[m]
      results$chargeNum[counter] = charging
      results$charge_amount[counter]=charge_amount
    }
  }
}

write.csv(results, "NonIdeal_1%_results.csv")    

plot(x=c(1:5000), y=mydata$gen[1:5000]-mydata$use[1:5000], type = "l", main = "Net Power Flow")

