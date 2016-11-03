setwd("C:/Users/Ewa/Dropbox/storage model/Multi facet reversed P ratio/Non-Ideal Storage")
data<- read.csv(file.choose(), header = TRUE)
library(scales)
cbPalette <- c('#a6cee3','#1f78b4','#b2df8a','#33a02c','#fb9a99','#e31a1c','#fdbf6f','#ff7f00','#cab2d6','#6a3d9a')


attach(data)

library(plyr)
library(ggplot2)

lolp15 <- read.csv(file.choose(), header = TRUE)

lolp10 <- read.csv(file.choose(), header = TRUE)

lolp5<- read.csv(file.choose(), header = TRUE)

lolp1<- read.csv(file.choose(), header = TRUE)

combo <- rbind(lolp1, lolp5, lolp10, lolp15)
levels(factor(combo$freq))
combo <- combo[!combo$Pratio==1,]
data <- data[!data$Pratio==1,]
combo$freq <- mapvalues(combo$freq, from = c("10080" , "43200",  "129600" ,"262800"), to=c("weekly", "monthly", "seasonal", "biannual"))
data$freq <- mapvalues(data$freq, from = c("10080" , "43200",  "129600" ,"262800"), to=c("weekly", "monthly", "seasonal", "biannual"))
combo$freq <- factor(combo$freq, levels=c("weekly", "monthly", "seasonal", "biannual"))

####################### Storage ~ P ratio #############
### For flexible Y axis 
ggplot(combo, aes(x=Pratio, y=Storage, color=factor(LOLP), linetype=factor(LOLP))) + geom_line(size=1.5) + 
  facet_grid(freq~., scales = "free_y")+ scale_x_continuous(breaks=c(1, 1.4, 1.8, 2, 3, 4, 5, 6))+
  labs(x="P ratio (Supply / Demand)", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12, face="bold"), strip.text.x = element_text(size = 14, face="bold"),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="LOLP",values=c("solid", "dotted", "4C88C488", "twodash"), labels=c("1%", "5%", "10%", "15%"))+
  scale_color_manual(name ="LOLP", values = cbPalette[c(2, 4, 6, 8)], labels=c("1%", "5%", "10%", "15%"))+
  theme(panel.margin.x=unit(0.5, "lines"),panel.margin.y=unit(0.5,"lines"))


### For LOLP 5, 10 and 15 without 1%
p <- combo[combo$LOLP!=0.01,]

ggplot(p, aes(x=Pratio, y=Storage, color=factor(LOLP), linetype=factor(LOLP))) + geom_line(size=1.5) + 
  facet_grid(freq~., scales = "free_y")+ scale_x_continuous(breaks=c(1, 1.4, 1.8, 2, 3, 4, 5, 6))+
  labs(x="P ratio (Supply / Demand)", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12, face="bold"), strip.text.x = element_text(size = 14, face="bold"),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="LOLP",values=c("solid", "dotted", "4C88C488"), labels=c("5%", "10%", "15%"))+
  scale_color_manual(name ="LOLP", values = cbPalette[c(2, 4, 6)], labels=c("5%", "10%", "15%"))+
  theme(panel.margin.x=unit(0.5, "lines"),panel.margin.y=unit(0.5,"lines"))

install.packages("viridis")
library(viridis)

####################### Storage ~ Charging Frequency #############
##LOLP 15%

lolp15$LOLP <- percent(lolp15$LOLP)
a <- ggplot(lolp15[!lolp15$Pratio==1,], aes(x=freq, y=Storage, colour=factor(Pratio), linetype=factor(Pratio))) + geom_line(size=1.2) + 
  facet_grid(LOLP~., labeller = label_both)+ scale_x_continuous(breaks=c(10080, 43200, 129600, 262800), labels = c("weekly", "monthly", "seasonal", "biannual"))+
  labs(x="Charging Frequency", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_discrete(name="P ratio")+
  scale_color_manual(name ="P ratio",values=cbPalette)

a

##LOLP 10%
lolp10$LOLP <- percent(lolp10$LOLP)
b <- ggplot(lolp10[!lolp10$Pratio==1,], aes(x=freq, y=Storage, colour=factor(Pratio), linetype=factor(Pratio))) + geom_line(size=1.2) + 
  facet_grid(LOLP~., labeller = label_both)+ scale_x_continuous(breaks=c(10080, 43200, 129600, 262800), labels = c("weekly", "monthly", "seasonal", "biannual"))+
  labs(x="Charging Frequency", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="P ratio",values=c(1:10))+
  scale_color_manual(name ="P ratio",values=cbPalette)
##LOLP 5%
lolp5$LOLP <- percent(lolp5$LOLP)
c <- ggplot(lolp5[!lolp5$Pratio==1,], aes(x=freq, y=Storage, colour=factor(Pratio), linetype=factor(Pratio))) + geom_line(size=1.2) + 
  facet_grid(LOLP~., labeller = label_both)+ scale_x_continuous(breaks=c(10080, 43200, 129600, 262800), labels = c("weekly", "monthly", "seasonal", "biannual"))+
  labs(x="Charging Frequency", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="P ratio",values=c(1:10))+
  scale_color_manual(name ="P ratio",values=cbPalette)
##LOLP 1%
lolp1$LOLP <- percent(lolp1$LOLP)
d <- ggplot(lolp1[!lolp1$Pratio==1,], aes(x=freq, y=Storage, colour=factor(Pratio), linetype=factor(Pratio))) + geom_line(size=1.2) + 
  facet_grid(LOLP~., labeller = label_both)+ scale_x_continuous(breaks=c(10080, 43200, 129600, 262800), labels = c("weekly", "monthly", "seasonal", "biannual"))+
  labs(x="Charging Frequency", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="P ratio",values=c(1:10))+
  scale_color_manual(name ="P ratio",values=cbPalette)
library(Rmisc)
multiplot(a, c, b, d, cols=2)



######################## Storage ~ Pratio + Factor: Charging Frequency############################################
## For LOLP = 15%
m <- combo[combo$Pratio >= 2 & combo$LOLP==0.15,]

a <- ggplot(combo[combo$LOLP==0.15,], aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(1.1, 1.4, 1.6, 2, 3, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4, y = 30, label = "LOLP 15%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])


v <- ggplot(m, aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(2, 3, 4, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4.5, y = 17, label = "LOLP 15%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

## For LOLP=10%

r <- combo[combo$Pratio >= 2 & combo$LOLP==0.1,]

b <- ggplot(combo[combo$LOLP==0.1,], aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(1.1, 1.4, 1.6, 2, 3, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4, y = 40, label = "LOLP 10%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

x <- ggplot(r, aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(2, 3, 4, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4.5, y = 20, label = "LOLP 10%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

## For LOLP=5%
s <- combo[combo$Pratio >= 2 & combo$LOLP==0.05,]
c <- ggplot(combo[combo$LOLP==0.05,], aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(1.1, 1.4, 1.6, 2, 3, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4, y = 75, label = "LOLP 5%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

y <- ggplot(s, aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(2, 3, 4, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4.5, y = 25, label = "LOLP 5%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

## For LOLP=1%
t <- combo[combo$Pratio >= 2 & combo$LOLP==0.01,]
d <- ggplot(combo[combo$LOLP==0.01,], aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(1.1, 1.4, 1.6, 2, 3, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 4, y = 200, label = "LOLP 1%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

z <- ggplot(t, aes(x=Pratio, y=Storage, colour=factor(freq), linetype=factor(freq))) + geom_line( size=1.2) + 
  scale_x_continuous(breaks=c(2, 3, 4, 5, 6))+
  labs(x="P ratio", y="Storage Size (kWh)")+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), 
        legend.title = element_text(colour="black", size=14, face="bold"), 
        panel.background = element_rect(fill = "white"), panel.grid.major = element_line(colour = "grey"),
        panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
        axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"), strip.text = element_text(size=16))+
  scale_linetype_manual(name="Charging Frequency", values=c("dotted", "twodash", "longdash", "dashed"))+annotate("text", x = 5, y = 50, label = "LOLP 1%", size=10)+
  scale_color_manual(name ="Charging Frequency",values=cbPalette[c(2, 4, 6, 8)])

library(Rmisc)
multiplot(a, c, b, d, cols = 2)
multiplot(v, y, x, z, cols=2)
###############################################################################


f <- data[!data$Pratio==1,]
f$freq <- factor(data$freq)
f$freq <- mapvalues(f$freq, from = c("10080" , "43200",  "129600" ,"262800"), to=c("weekly", "monthly", "seasonal", "biannual"))
f$freq <- factor(f$freq, levels=c("weekly", "monthly", "seasonal", "biannual"))

interaction.plot(f$Pratio, f$freq, f$Storage,type = "b",
                 lwd = 2.5, col = cbPalette[c(3, 10, 9, 5)] ,
                 pch = c (15 ,16 ,17, 18), xlab = " P ratio (Supply/Demand ratio)", ylab="Mean of storage size (kWh)", trace.label = "Charging Frequency")

interaction.plot(f$freq, f$Pratio, f$Storage,type = "b",
                 lwd = 2.5, col = cbPalette,
                 pch = c (15 ,16 ,17, 18), xlab = "Charging Frequency", ylab="Mean of storage size (kWh)", trace.label = "P ratio")

ggplot(data = f,
       aes(x = Pratio, y = Storage, colour = freq, group=freq)) +
  stat_summary(fun.y=mean, geom="point")+
  stat_summary(fun.y=mean, geom="line")

