source("C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection/src/survival_bw_tail/bw_tl_data.R")

p1<-ggplot(data = bw_tl) + geom_boxplot(aes(x=Time , y = Weight, col = ASO_dose))
p2<-ggplot(data = bw_tl[-372,]) + geom_boxplot(aes(x=Time , y = Tail_length, col = ASO_dose))
p3<-ggplot(data = bw_tl[which(bw_tl$ASO_dose=="MOE50"),]) + 
  geom_boxplot(aes(x=Time , y = Weight, col = Cage)) + 
  labs(title = "MOE50") +
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0,30))
p4<-ggplot(data = bw_tl[which(bw_tl$ASO_dose=="MOE100"),]) + 
  geom_boxplot(aes(x=Time , y = Weight, col = Cage))+ 
  labs(title = "MOE100")+
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0,30))
p5<-ggplot(data = bw_tl[which(bw_tl$ASO_dose=="MOE200"),]) + 
  geom_boxplot(aes(x=Time , y = Weight, col = Cage))+ 
  labs(title = "MOE200")+
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0,30))
p6<-ggplot(data = bw_tl[which(bw_tl$ASO_dose=="MOE400"),]) + 
  geom_boxplot(aes(x=Time , y = Weight, col = Cage))+ 
  labs(title = "MOE400")+
  scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0,30))

grid.arrange(p1,p2,p3,p4,p5,p6)

# dot plot of body weight change, colored by ASO dose
p7bw<-ggplot(data = bw_summary, aes(x=Time , y = mean, col = ASO_dose))+
  geom_point(aes(), position = position_dodge(width = .5))+
  geom_errorbar(aes(ymin = -sd+mean, ymax = sd+mean),alpha = 0.3, width = 0.5, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),position = position_dodge(width = .5)) +
  labs(title = "Body Weight", y = "Mean Body Weight (g)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,30,5)) +
  scale_color_discrete(name = "ASO Dose",  labels = c("50 µg", "100 µg", "200 µg", "400 µg")) 
p7bw

# dot plot of tail length change, colored by ASO dose
p7tl<-ggplot(data = tl_summary, aes(x=Time , y = mean, col = ASO_dose))+
  geom_point(aes(), position = position_dodge(width = .5))+
  geom_errorbar(aes(ymin = tl_min, ymax = sd+mean),alpha = .7, width = 1, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),position = position_dodge(width = .5)) +
  labs(title = "Tail length", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,30,0.5)) +
  geom_hline(yintercept = v, color = c("red","green","blue","purple"))
p7tl

p7tl.1<-ggplot(data = tl_summary%>%filter(ASO_dose=="MOE50"), aes(x=Time , y = mean))+
  geom_point(aes())+
  geom_errorbar(aes(ymin = tl_min[1:14], ymax = sd+mean),alpha = .7, width = 1, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),position = position_dodge(width = .5)) +
  labs(title = "MOE50 tail", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,4.5,0.5), limit = c(0,5)) +
  geom_hline(yintercept = tl7[1,3], linetype = "dashed")

p7tl.2<-ggplot(data = tl_summary%>%filter(ASO_dose=="MOE100"), aes(x=Time , y = mean))+
  geom_point(aes(),color="blue")+
  geom_errorbar(aes(ymin = tl_min[15:28], ymax = sd+mean),
                color = "blue",alpha = .7, width = 1, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),
            color= "blue",position = position_dodge(width = .5)) +
  labs(title = "MOE100 tail", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,4.5,0.5), limit = c(0,5))+
  geom_hline(yintercept = tl7[2,3], linetype = "dashed")
p7tl.2

p7tl.3<-ggplot(data = tl_summary%>%filter(ASO_dose=="MOE200"), aes(x=Time , y = mean))+
  geom_point(aes(),color="green")+
  geom_errorbar(aes(ymin = tl_min[29:42], ymax = sd+mean),
                color = "green",alpha = .7, width = 1, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),
            color= "green",position = position_dodge(width = .5)) +
  labs(title = "MOE200 tail", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,4.5,0.5), limit = c(0,5))+
  geom_hline(yintercept = tl7[3,3], linetype = "dashed")
p7tl.3

p7tl.4<-ggplot(data = tl_summary%>%filter(ASO_dose=="MOE400"), aes(x=Time , y = mean))+
  geom_point(aes(),color="purple")+
  geom_errorbar(aes(ymin = tl_min[29:42], ymax = sd+mean),
                color = "purple",alpha = .7, width = 1, position = position_dodge(width = .5)) +
  geom_line(aes(group = ASO_dose),
            color= "purple",position = position_dodge(width = .5)) +
  labs(title = "MOE400 tail", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,4.5,0.5), limit = c(0,5))+
  geom_hline(yintercept = tl7[4,3], linetype = "dashed")

p7tl.4

grid.arrange(p7tl.1,p7tl.2,p7tl.3,p7tl.4, ncol=1)
#plot tail length and body weight on a common x-axis on one page
grid.newpage()
grid.draw(rbind(ggplotGrob(p7bw), ggplotGrob(p7tl), size = "last"))

# boxplot + dot bplot of body weigh
# $$$$$$$$$$$$not so useful$$$$$$$$$$
p8<-ggplot(data = bw_tl) + geom_boxplot(aes(x=Time , y = Weight, col = ASO_dose), position = position_dodge(width = 1)) +
  geom_dotplot(binaxis = "y", binwidth = .1, 
               stackdir = "center",
               aes(x = Time, y = Weight, col = ASO_dose), 
               position = position_dodge(width = 1))
p8


bw_tl_summary<-bw_tl_summary%>%add_column(level = sapply(bw_tl_summary$pvals,siglev))
bw_tl_summary%>%write_csv(file.path(dir,"bodyweight_tail_length_comparison.csv"))
bw_tl_summary%>%arrange(comparisons)
table(bw_tl_summary$comparisons)
