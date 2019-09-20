library(grid)
library(gridExtra)
library(ggsignif)

src_dir<-"C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection/src/strength/grip_rotarod.R"
source(file.path(src_dir))

# significance annotation based on linear modeling `strength`
annotation_df <- data.frame(start=rep("Het", 4), 
                            end=c("MOE50", "MOE100","MOE200","MOE400"), 
                            y=c(210, 225, 238, 250),
                            label=c("ns", "*", "*","ns"))
annotation_df2 <- data.frame(start=rep("Het", 4), 
                            end=c("MOE50", "MOE100","MOE200","MOE400"), 
                            y=c(210, 225, 238, 250),
                            label=c("ns", "*", "*","ns"))
annotation_df3 <- data.frame(start=rep("Het", 4), 
                             end=c("MOE50", "MOE100","MOE200","MOE400"), 
                             y=c(180, 200, 220, 240),
                             label=c("*", "*", "**","ns"))

# rotarod boxplot
p1<-ggplot(max_rotarod,aes(x= `AI_E16-17_dose`, y = max_r)) + 
  geom_boxplot() + 
  geom_point() +
  labs(x="genotype/ASO dose", y="mean max rotarod time") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))+
  scale_y_continuous(breaks=seq(0,250,50)) +
  geom_signif(data=annotation_df,
              aes(xmin=start, xmax = end, annotations = label, y_position = y),
              textsize = 3, vjust = -.1,
              manual=TRUE)
  
print(p1)

# rotarod barplot
p2<-ggplot(data=summary_rr, aes(`AI_E16-17_dose`))+
  geom_bar(aes(weight = mean_rr_max, fill = `AI_E16-17_dose` ),color= "black") + 
  geom_errorbar(aes(ymin = mean_rr_max, ymax=mean_rr_max+sd), width=.2,
                position=position_dodge(.9)) +
  labs(x="genotype/ASO dose", y="mean max rotarod time") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.position = "none")+
  scale_y_continuous(breaks=seq(0,250,50))+
  geom_signif(data=annotation_df2,
              aes(xmin=start, xmax = end, annotations = label, y_position = y),
              textsize = 3, vjust = -.1,
              manual=TRUE)
  
print(p2)


# grip boxplot
p3<- ggplot(grip, aes(x=`AI_E16-17_dose`, y=grip)) + 
  geom_boxplot()+
  geom_point() +
  labs(x="genotype/ASO dose", y="grip strength (unit?)")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  scale_y_continuous(breaks = seq(0, 250, 20)) +
  geom_signif(data=annotation_df3,
              aes(xmin=start, xmax = end, annotations = label, y_position = y),
              textsize = 5, vjust = -.1,
              manual=TRUE)

print(p3)

grid.arrange(arrangeGrob(top=textGrob("Amniotic Injection grip and rotarod"),p1,p2, ncol=2),
             arrangeGrob(p3, bottom=textGrob(
             "E16-E17 AI injection (late injection compared to E13-13.5),
assay age unknown, too few data points for MOE400",
                                             gp=gpar(fontsize=15, col="grey"))))
