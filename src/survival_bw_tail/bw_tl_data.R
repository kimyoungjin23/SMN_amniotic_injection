source("C:/Users/kimyo/Dropbox/CSHL/r/func/ggplot_library.R")

dir<-"C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection/data/bodyweight_tail"

bw_tl<-read_csv(file.path(dir,"bodyweight_tail.csv"))

bw_tl$Time<-factor(bw_tl$Time, levels = unique(bw_tl$Time))

bw_tl$ASO_dose<-factor(bw_tl$ASO_dose, levels = unique(bw_tl$ASO_dose))

#summarize body weight by aso dose groups
bw_mean<-aggregate(Weight ~ Time + ASO_dose, bw_tl, mean)
bw_sd<-aggregate(Weight ~ Time + ASO_dose, bw_tl, sd)
bw_summary<-tibble(Time = bw_mean$Time, ASO_dose = bw_mean$ASO_dose, mean_bw = bw_mean$Weight, sd_bw = bw_sd$Weight)

#summarize tail length by aso dose groups
tl_mean<-aggregate(Tail_length ~ Time + ASO_dose, bw_tl[-372,], mean)
tl_sd<-aggregate(Tail_length ~ Time + ASO_dose, bw_tl[-372,], sd)
tl_summary<-tibble(Time = tl_mean$Time, ASO_dose = tl_mean$ASO_dose, mean = tl_mean$Tail_length, sd = tl_sd$Tail_length)

tl_min = tl_summary$mean - tl_summary$sd
tl_min<-ifelse(tl_min>0,
               tl_min,
               0
)

# Look at half life of tail length?
mx.mean.tl<-tl_mean %>% group_by(ASO_dose) %>% summarise(max_mean_tl=max(Tail_length))
v<-mx.mean.tl$max_mean_tl/2
