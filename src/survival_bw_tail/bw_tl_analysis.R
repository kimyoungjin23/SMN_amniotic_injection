source("C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection/src/survival_bw_tail/bw_tl_data.R")

dir<-"C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection/src/bw_tl.R"

# compare bodyweight at every time point
test<-list()
times<-unique(bw_tl$Time)
for (q in 1:length(times)){
  p = times[q] 
  d <- bw_tl[which(bw_tl$Time == p),]; 
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

# compare tail length at every time point

# define a test results list
test_tl<-list()

# set times variable
times<-unique(bw_tl$Time)

# compare tail lengths by ANOVA for each time point
for (q in 1:length(times)){
  p = times[q] 
  d <- bw_tl[which(bw_tl$Time == p),]; 
  test_tl[[p]]<-aov(Tail_length ~ ASO_dose, data = d)
}

# pool individual ANOVA results 
tl_anova<-list()
for(n in 1:14){
  tl_anova[[n]]<-(summary(test_tl[[n]]) )
}
tl_Tukey<-list()
for(n in 1:14){
  tl_Tukey[[n]]<- TukeyHSD(test_tl[[n]])
}
# finish tail ANOVA test 

days<-rep(unique(bw_tl$Time), each = 6)
comps<-rep(names(bw_Tukey[[1]]$ASO_dose[,"p adj"]), 14)

#bodyweight anova pval summary
bw_anova_pval<-matrix(ncol = 14, nrow = 6)
for(n in 1:14){
  bw_anova_pval[,n]<-matrix(bw_Tukey[[n]]$ASO_dose[,"p adj"],6,1)
}
# transform bw ANOVA pval 6 x 14 matrix into 84 x 1 matrix and make into one tibble

bw_pvals<-tibble(time = days , comparisons = comps, pvals = unlist(matrix(bw_anova_pval,ncol =1)))

#tail length anova pval summary
tl_anova_pval<-matrix(ncol = 14, nrow = 6)
for(n in 1:14){
  tl_anova_pval[,n]<-matrix(tl_Tukey[[n]]$ASO_dose[,"p adj"],6,1)
}

# transform bw ANOVA pval 6 x 14 matrix into 84 x 1 matrix and make into one tibble

tl_pvals<-tibble(time = days , comparisons = comps, pvals = unlist(matrix(tl_anova_pval,ncol =1 )))


# find significant bodyweights based on ANOVA for each time point
bw_sig<-bw_pvals%>%filter(pvals<0.05) %>% add_column(category = "bodyweight")
tl_sig<-filter(tl_pvals, pvals[,1]<0.05) %>% add_column(category = "tail")
bw_tl_summary<-bind_rows(bw_sig,tl_sig)

siglev<-function(p){
  ifelse(p<0.001,"***",
         ifelse(0.01>p,"**",
                ifelse(0,05>p,"*")
         )
  )
}

# when does the mean tail tail length become shorter than when that of when the mice were 1 wk old?
tl7<-tl_mean%>% filter(Time == "P7")

pndays<-as.character(times)
# based on the plot, 
tl7<-tl7%>% mutate(time_below_p7_tl= as.character(c(pndays[8],pndays[12],pndays[10],pndays[9])))
tl7

time.tail<-bw_tl%>%select(Time, Tail_length)
tl.summary<-time.tail%>%group_by(Time)%>%summarise(mean=mean(Tail_length), sd=sd(Tail_length), n=n())

ggplot(data = tl.summary, aes(x=Time , y = mean))+
  geom_point()+
  geom_errorbar(aes(ymin = mean -sd, ymax = sd+mean),alpha = .7, width = 1) +
  geom_line(aes(group=NA),linetype=1) +
  labs(title = "all tail lengths", y = "Mean Tail Length (cm)", x = "Postnatal Days") +
  scale_y_continuous(breaks = seq(0,5,0.5), limit = c(-1,6)) +
  geom_hline(yintercept = unlist(tl.summary[1,2]), linetype="dashed")
