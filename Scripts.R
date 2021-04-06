###Figure 1


library(ggplot2)
library(viridis)

setwd("~/Desktop/")

data<- read.csv("Fig1&2.csv", header=TRUE) 


data.summary <- aggregate(Score ~ Population, mean, data=data)

data.summary

ggplot(data = data, aes(x=Population, y=Score, fill=Population)) +
  geom_boxplot ()+
  annotate("text", x = c("LA 3786","LA 1958","LA 1963","LA 2747","LA 3111", "LA 2932", "LA 4107","LA 4117A","LA 4330"),
           y = .1, label =c("2194","1588","2148","1685","2129", "2101", "1601\n No. of inoculations","1715","2249"))+
  scale_fill_manual(values=c("#F8766D", "#F8766D", "#F8766D",  "#00BA38","#F8766D", "#C77CFF", "#00BA38","#619CFF", "#619CFF"), 
                    name = "Geographical location", 
                    labels = c( "Central", "", "",  "Southern coastal", "","Northern", "", "Southern highlands", ""))+
  scale_x_discrete(limits=c("LA 3786","LA 1958","LA 1963","LA 2747","LA 3111", "LA 2932", "LA 4107","LA 4117A","LA 4330"))+
  ylim(-0.01,1.01) +
  geom_jitter(width = 0.1)+
  labs(x= "Population", y= "Pi100 infection score")+
  theme_bw() +
  theme(axis.title=element_text(size=14, family = 'Times New Roman'))+
  theme(axis.text.y=element_text(size=14, colour = "black", family = 'Times New Roman'))+ 
  theme(axis.text.x=element_text(size=14, colour = "black", angle = 40 , 
                                 family = 'Times New Roman'))+    
  theme(legend.title = element_text(size = 14, family = 'Times New Roman'))+
  theme(legend.text = element_text(size = 14, family = 'Times New Roman'))+
  theme(axis.line = element_line(colour = 'black', size = 1))+
  theme(axis.ticks = element_line(colour = "black", size = 1))+
  theme(plot.title = element_text(size=14, hjust = 0.5, family = 'Times New Roman')) 

###Stats

anova <- aov(Score ~ Population, data=data)
summary(anova)

TukeyHSD(anova)

#Figure 2

library(ggplot2)
library(viridis)
library(dplyr)
setwd("~/Desktop/")

data<- read.csv("Fig1&2.csv", header=TRUE) 

data.1 <- ddply(data, .(Plant, Population), summarize, med = mean(Score))
data.1


data.summary <- aggregate(Score ~ Population, mean, data=data)

data.summary
data$Population_1 = factor(data$Population, levels=c("LA 3786","LA 1958","LA 1963","LA 2747","LA 3111", "LA 2932", "LA 4107","LA 4117A","LA 4330"))

ggplot(data = data, aes(x=Plant, y=Score, fill=Population)) +
  geom_boxplot ()+
  #annotate("text", x = c("LA 3786","LA 1958","LA 1963","LA 2747","LA 3111", "LA 2932", "LA 4107","LA 4117A","LA 4330"), y = .1, label =c("2194","1588","2148","1685","2129", "2101", "1601\n No. of inoculations","1715","2249"))+
  scale_fill_manual(values=c("#F8766D", "#F8766D", "#F8766D",  "#00BA38","#F8766D", "#C77CFF", "#00BA38","#619CFF", "#619CFF"), 
                    name = "Geographical location", labels = c( "Central", "", "",  "Southern coastal", "","Northern", "", "Southern highlands", ""))+
  #scale_x_discrete(limits=c("LA 3786","LA 1958","LA 1963","LA 2747","LA 3111", "LA 2932", "LA 4107","LA 4117A","LA 4330"))+
  ylim(-0.01,1.01) +
  geom_jitter(width = 0.1)+
  facet_wrap(~Population_1, scales = "free")+
  labs(x= "Plants", y= "Pi100 infection score")+
  #ggtitle("Infection Assay P100") +
  theme_bw()+
  theme(axis.text.x = element_blank())+
  theme(axis.line = element_line(colour = 'black', size = 1))+
  theme(axis.ticks = element_line(colour = "black", size = 1))+
  theme(axis.title.y=element_text(size=14, family = 'Times New Roman'))+
  theme(axis.text.y=element_text(size=14, colour = "black", family = 'Times New Roman'),
        strip.text = element_text(size = 14, face = "bold", family = 'Times New Roman'))+
  #theme(axis.text.x=element_text(size=14, colour = "black", angle = 40))+ 
  theme(axis.title.x=element_text(size=14, family = 'Times New Roman'))+
  theme(legend.title = element_text(size = 14, family = 'Times New Roman'))+
  theme(legend.text = element_text(size = 14, family = 'Times New Roman'))+
  theme(plot.title = element_text(size=14, hjust = 0.5, family = 'Times New Roman')) 


###stats

data1 <- data[data$Population == "LA 3786",]
anova1 <- aov(Score ~ Plant, data=data1)
tuktab1 <- as.data.frame(TukeyHSD(anova1)$Plant)
tuktab1sig <- tuktab1[tuktab1$`p adj` < 0.05,]

data2 <- data[data$Population == "LA 1958",]
anova2 <- aov(Score ~ Plant, data=data2)
tuktab2 <- as.data.frame(TukeyHSD(anova2)$Plant)
tuktab2sig <- tuktab2[tuktab2$`p adj` < 0.05,]


data3 <- data[data$Population == "LA 1963",]
anova3 <- aov(Score ~ Plant, data=data3)
tuktab3 <- as.data.frame(TukeyHSD(anova3)$Plant)
tuktab3sig <- tuktab3[tuktab3$`p adj` < 0.05,]

data4 <- data[data$Population == "LA 2747",]
anova4 <- aov(Score ~ Plant, data=data4)
tuktab4 <- as.data.frame(TukeyHSD(anova4)$Plant)
tuktab4sig <- tuktab4[tuktab4$`p adj` < 0.05,]

data5 <- data[data$Population == "LA 3111",]
anova5 <- aov(Score ~ Plant, data=data5)
tuktab5 <- as.data.frame(TukeyHSD(anova5)$Plant)
tuktab5sig <- tuktab5[tuktab5$`p adj` < 0.05,]


data6 <- data[data$Population == "LA 2932",]
anova6 <- aov(Score ~ Plant, data=data6)
tuktab6 <- as.data.frame(TukeyHSD(anova6)$Plant)
tuktab6sig <- tuktab6[tuktab6$`p adj` < 0.05,]

data7 <- data[data$Population == "LA 4107",]
anova7 <- aov(Score ~ Plant, data=data7)
tuktab7 <- as.data.frame(TukeyHSD(anova7)$Plant)
tuktab7sig <- tuktab7[tuktab7$`p adj` < 0.05,]

data8 <- data[data$Population == "LA 4117A",]
anova8 <- aov(Score ~ Plant, data=data8)
tuktab8 <- as.data.frame(TukeyHSD(anova8)$Plant)
tuktab8sig <- tuktab8[tuktab8$`p adj` < 0.05,]

data9 <- data[data$Population == "LA 4330",]
anova9 <- aov(Score ~ Plant, data=data9)
tuktab9 <- as.data.frame(TukeyHSD(anova9)$Plant)
tuktab9sig <- tuktab9[tuktab9$`p adj` < 0.05,]

#####Figure 3

library(reshape2)
library(ggplot2)
setwd("~/Desktop/")


data<- read.csv("Fig3.csv", header=TRUE) 


data.summary <- aggregate(Plate ~ Strain, mean, data=data)
data.summary

ggplot(data, aes(x = Strain, y = Plate)) +
  geom_boxplot()+
  geom_jitter(width = .3, size = 1.8,(aes(colour = Strain)))+
  scale_x_discrete(limits=c("Score P100","Score P078","Score P054","EC1","Score 88069", "Score 3928", "Score T-30-4"))+
  scale_color_manual(values=c("#009E73", "#E69F00", "#56B4E9",  "#F0E442","#D55E00", "#CC79A7", "#0072B2"), 
                     name =c("Isolates"),
                     breaks=c("Score P100","Score P078","Score P054","EC1","Score 88069", "Score 3928", "Score T-30-4"),
                     labels=c("Pi100","Pi078","Pi054","EC1","88069", "3928", "T-30-4"))+
  ylim (0,10)+
  coord_polar() +
  theme_bw() +
  theme(axis.text.x = element_blank())+
  labs(y = "Growth (cm)")+
  theme(axis.line = element_line(colour = 'black', size = 1))+
  theme(axis.ticks = element_line(colour = "black", size = 1))+
  theme(axis.title.y=element_text(size=12))+
  theme(axis.text.y=element_text(size=12, colour = "black"))+
  theme(axis.title.x=element_text(size=12))+
  theme(legend.title = element_text(size = 12))+
  theme(legend.text = element_text(size = 12))+
  theme(plot.title = element_text(size=12, hjust = 0.5))  


####Figure 4

library(ggplot2)
library(plyr)
setwd("~/Desktop/")
data<- read.csv("Fig4.csv", header=TRUE) 
data.summary <- aggregate(Score ~ Pathogen, median, data=data)

data.summary

data.1 <- ddply(data, .(Plant, Pathogen), summarize, med = median(Score))
data.1
ggplot(data, aes(x = Pathogen, y = Score)) +
  geom_boxplot()+
  ylim (-0.1,1.1)+
  geom_jitter(width = .2,  size = 1.8,(aes(colour = Pathogen)))+
  labs(title="Population LA3111", y="Infection score",
       x=expression(paste(italic("P. infestans"),"isolates"))) +
  #scale_fill_manual(values=c("#009E73", "#E69F00", "#56B4E9",  "#F0E442","#D55E00", "#CC79A7", "#0072B2"), 
  #name =c("Isolates"), 
  #breaks=c("P100","P078","P054","EC1","88069", "3928", "T-30-4"),
  #labels=c("Pi100","Pi078","Pi054","EC1","88069", "3928", "T-30-4"))+
  facet_wrap(~Plant, scales = "free")+
  scale_x_discrete(limits=c("P100","P078","P054","EC1","88069", "3928", "T-30-4"))+
  scale_color_manual(values=c("#009E73", "#E69F00", "#56B4E9",  "#F0E442","#D55E00", "#CC79A7", "#0072B2"), 
                     name =c(expression(paste(italic("P. infestans"),"isolates "))),
                     breaks=c("P100","P078","P054","EC1","88069", "3928", "T-30-4"),
                     labels=c("Pi100","Pi078","Pi054","EC1","88069", "3928", "T-30-4"))+
  #geom_text(data = data.1, aes(y = med, label = round(med,2)),size = 3, vjust = -0.5)+
  theme_bw() +
  theme(plot.title = element_text(size = 18, face = "bold", family = 'Times New Roman'))+
  theme(axis.text.x = element_blank())+
  theme(axis.title =element_text(size=14, family = 'Times New Roman'))+
  theme(axis.text.y=element_text(size=14, colour = "black", family = 'Times New Roman'), 
        strip.text = element_text(size = 14, face = "bold", family = 'Times New Roman'))+
  theme(legend.title = element_text(size = 14, family = 'Times New Roman'))+
  theme(legend.text = element_text(size = 14, family = 'Times New Roman'))+
  theme(axis.line = element_line(colour = 'black', size = 1))+
  theme(axis.ticks = element_line(colour = "black", size = 1))+
  theme(plot.title = element_text(size=14, hjust = 0.5, family = 'Times New Roman')) 

###stats

data1 <- data[data$Plant == "Plant 02",]
anova1 <- aov(Score ~ Pathogen, data=data1)
tuktab1 <- as.data.frame(TukeyHSD(anova1)$Pathogen)
tuktab1sig <- tuktab1[tuktab1$`p adj` < 0.05,]

data2 <- data[data$Plant == "Plant 03",]
anova2 <- aov(Score ~ Pathogen, data=data2)
tuktab2 <- as.data.frame(TukeyHSD(anova2)$Pathogen)
tuktab2sig <- tuktab2[tuktab2$`p adj` < 0.05,]


data3 <- data[data$Plant == "Plant 05",]
anova3 <- aov(Score ~ Pathogen, data=data3)
tuktab3 <- as.data.frame(TukeyHSD(anova3)$Pathogen)
tuktab3sig <- tuktab3[tuktab3$`p adj` < 0.05,]

data4 <- data[data$Plant == "Plant 07",]
anova4 <- aov(Score ~ Pathogen, data=data4)
tuktab4 <- as.data.frame(TukeyHSD(anova4)$Pathogen)
tuktab4sig <- tuktab4[tuktab4$`p adj` < 0.05,]

data5 <- data[data$Plant == "Plant 10",]
anova5 <- aov(Score ~ Pathogen, data=data5)
tuktab5 <- as.data.frame(TukeyHSD(anova5)$Pathogen)
tuktab5sig <- tuktab5[tuktab5$`p adj` < 0.05,]


data6 <- data[data$Plant == "Plant 11",]
anova6 <- aov(Score ~ Pathogen, data=data6)
tuktab6 <- as.data.frame(TukeyHSD(anova6)$Pathogen)
tuktab6sig <- tuktab6[tuktab6$`p adj` < 0.05,]

data7 <- data[data$Plant == "Plant 12",]
anova7 <- aov(Score ~ Pathogen, data=data7)
tuktab7 <- as.data.frame(TukeyHSD(anova7)$Pathogen)
tuktab7sig <- tuktab7[tuktab7$`p adj` < 0.05,]

data8 <- data[data$Plant == "Plant 13",]
anova8 <- aov(Score ~ Pathogen, data=data8)
tuktab8 <- as.data.frame(TukeyHSD(anova8)$Pathogen)
tuktab8sig <- tuktab8[tuktab8$`p adj` < 0.05,]

data9 <- data[data$Plant == "Plant 14",]
anova9 <- aov(Score ~ Pathogen, data=data9)
tuktab9 <- as.data.frame(TukeyHSD(anova9)$Pathogen)
tuktab9sig <- tuktab9[tuktab9$`p adj` < 0.05,]


####GxG test

setwd("~/Desktop/")

library(scales)


data<- read.csv("Fig4.csv", header=TRUE) 

anova.aov = aov(Score~Plant*Pathogen)
summary(anova.aov)


####GLMM
library(lme4)
setwd("~/Desktop/")
Data<- read.csv("Fig4.csv", header=TRUE) 
Data <- Data

attach(Data)

y<- cbind(infected,not_infected)
model1 <- glmer(y~Plant+(1|Rep),family=binomial)
summary(model1)
model2 <- glmer(y~Pathogen+(1|Rep),family=binomial)
summary(model2)
model3 <- glmer(y~Plant+Pathogen+(1|Rep),family=binomial)
summary(model3)

