library(tidyverse)
dir<-"/Users/youngjinkim/Dropbox/CSHL/R/SMN_amniotic_injection"
bw<-read_csv(file.path(dir,"bodyweight.csv"))
ggplot(data = bw)