library(tidyverse)
library(ggthemes)
# load data
projdir<-"C:/Users/kimyo/Dropbox/CSHL/r/smn_amniotic_injection"
strength_dir<-file.path(projdir, "data","strength","rotarod_restructure.csv")

# make sure the rotarod time is 'numeric'
rotarod<-read_csv(strength_dir)%>%
  mutate(rotarod_hang_seconds = as.numeric(rotarod_hang_seconds))

# set levels for the dose
rotarod$`AI_E16-17_dose`<-factor(rotarod$`AI_E16-17_dose`, levels = c("Het","MOE50","MOE100","MOE200","MOE400"))
# find max hangtime for each mouse
# set `max_r` as numeric and
# arrange the rotarod by dose
max_rotarod<-
  rotarod%>%
  group_by(mouse_cage,sex_M1F2,`AI_E16-17_dose`)%>%
  summarise(max_r = max(rotarod_hang_seconds, na.rm=TRUE)) %>%
    arrange(`AI_E16-17_dose`)
view(max_rotarod)

# mean max rotarod per treatment
summary_rr<-
  max_rotarod %>% 
  group_by(`AI_E16-17_dose`) %>% 
  summarise(mean_rr_max = mean(max_r, na.rm=TRUE), sd = sd(max_r, na.rm=TRUE))

# statistical test rotarod
strength<-list()
strength[[1]]<-summary(lm(max_r ~`AI_E16-17_dose`, max_rotarod))

##############grip###############

grip_dir<-file.path(projdir, "data","strength","grip_restructure.csv")
grip<-read_csv(grip_dir)
grip$`AI_E16-17_dose`<-factor(grip$`AI_E16-17_dose`, levels = c("Het","MOE50","MOE100","MOE200","MOE400"))
mean_grip<- grip%>%group_by(`AI_E16-17_dose`)%>%summarise(mean_grip=mean(grip))

# analyze grip data
strength[[2]]<-summary(lm(grip ~`AI_E16-17_dose`, grip))


