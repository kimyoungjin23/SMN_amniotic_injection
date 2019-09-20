library("tidyverse")

# western blot data
dataDir = file.path("C:/Users/kimyo/Dropbox/CSHL/r/SMN_amniotic_injection","data","smn_wb_quant_all_tissues.csv")
wb=read_csv(dataDir)
wb$tx=factor(wb$tx, levels = unique(wb$tx))
wb$tissue = factor(wb$tissue, levels = unique(wb$tissue))

# basic plot
p1 = ggplot(data = wb, aes(x=tissue,y=smn_fc ,fill = tx)) +
  geom_bar(stat="identity", position = "dodge") + 
  ylab("SMN fold change") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#grouped smn fold change mean and standard deviation
mean_smn = aggregate(smn_fc ~ tissue + tx, wb, mean)
mean_smn = mean_smn[order(mean_smn$tissue),]
sd_smn = aggregate(smn_fc ~ tissue + tx, wb, sd)
sd_smn = sd_smn[order(sd_smn$tissue),]
smn_group_summ <- data.frame(mean_smn, sd = sd_smn$smn_fc)

# plot
ggplot(data = smn_group_summ, aes(x=tissue,y=smn_fc ,fill = tx)) +
  geom_bar(stat="identity", position = "dodge", color = "black")+
  #add error bars, top only
  geom_errorbar(aes(x=tissue, y=smn_fc, 
                  ymin=smn_fc, ymax = smn_fc + sd),
                position = position_dodge())+
  ylab("SMN fold change vs. Het") +
  theme (
    panel.border = element_blank(),  
    # Remove panel grid lines
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Remove panel background
    panel.background = element_blank() ,
    # Add axis line
    axis.line = element_line(colour = "grey" )) 
