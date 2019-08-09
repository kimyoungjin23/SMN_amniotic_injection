library(tidyverse)
library(gplots)
library(ggplot2)
source("/Users/youngjinkim/Dropbox/CSHL/R/Functions/multiplot.R")
dir<-"/Users/youngjinkim/Dropbox/CSHL/R/SMN_amniotic_injection"
bw<-read_csv(file.path(dir,"bodyweight.csv"))
bw$Time<-factor(bw$Time, levels = unique(bw$Time))
bw$ASO_dose<-factor(bw$ASO_dose, levels = unique(bw$ASO_dose))
p1<-ggplot(data = bw) + geom_boxplot(aes(x=Time , y = Weight, col = ASO_dose))
p2<-ggplot(data = bw[-372,]) + geom_boxplot(aes(x=Time , y = Tail_length, col = ASO_dose))
p3<-ggplot(data = bw[which(bw$ASO_dose=="MOE50"),]) + geom_boxplot(aes(x=Time , y = Weight, col = Cage)) 
p4<-ggplot(data = bw[which(bw$ASO_dose=="MOE100"),]) + geom_boxplot(aes(x=Time , y = Weight, col = Cage))
p5<-ggplot(data = bw[which(bw$ASO_dose=="MOE200"),]) + geom_boxplot(aes(x=Time , y = Weight, col = Cage))
p6<-ggplot(data = bw[which(bw$ASO_dose=="MOE400"),]) + geom_boxplot(aes(x=Time , y = Weight, col = Cage))
multiplot(p1,p2)
multiplot(p3,p4,p5,p6)
bw_mean<-aggregate(Weight ~ Time + ASO_dose, bw, mean)
bw_sd<-aggregate(Weight ~ Time + ASO_dose, bw, sd)
bw_summary<-tibble(Time = bw_mean$Time, ASO_dose = bw_mean$ASO_dose, mean = bw_mean$Weight, sd = bw_sd$Weight)
p7<-ggplot(data = bw_summary, aes(x=Time , y = mean, col = ASO_dose))+
  geom_point(aes(), position = position_dodge(width = .5))+
  geom_errorbar(aes(ymin = -sd+mean, ymax = sd+mean),alpha = 0.3, width = 0.5, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),position = position_dodge(width = .5))
p7

p8<-ggplot(data = bw) + geom_boxplot(aes(x=Time , y = Weight, col = ASO_dose), position = position_dodge(width = 1)) +
  geom_dotplot(binaxis = "y", binwidth = .1, 
               stackdir = "center",
               aes(x = Time, y = Weight, col = ASO_dose), 
               position = position_dodge(width = 1))
p8
test<-list()
times<-unique(bw$Time)
for (q in 1:length(times)){
  p = times[q] 
  d <- bw[which(bw$Time == p),]; 
  test[[p]]<-aov(Weight ~ ASO_dose, data = d)
}
bw_anova<-list()
for(n in 1:14){
  bw_anova[[n]]<-(summary(test[[n]]) )
}
bw_Tukey<-list()
for(n in 1:14){
  bw_Tukey[[n]]<- TukeyHSD(test[[n]])
}
# pretty much mostly difference is seen between 200 vs 50
