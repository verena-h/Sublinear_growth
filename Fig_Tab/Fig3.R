require(ggplot2)
require(gridExtra)
require(ggpubr)
require(patchwork)

rm(list=ls())
df_whole = read.csv("df_Variability_Sources_Fig_3.csv")
row.names(df_whole) = df_whole$X
df_whole = df_whole[,-1]

### absolute values, BEFORE flooding event ### 
Stream=rep(c("CAi","TYt","LLi", "ULt"),3)

source=c(rep("Process\nNoise",4),
         rep("Observation\nError",4),
         rep("Deterministic\nVariation",4)
)

mean=as.vector(t(df_whole[1:3,c(1,6,11,16)]))
sd=as.vector(t(df_whole[1:3,c(2,7,12,17)]))
q1=as.vector(t(df_whole[1:3,c(3,8,13,18)]))
q2=as.vector(t(df_whole[1:3,c(4,9,14,19)]))
q3=as.vector(t(df_whole[1:3,c(5,10,15,20)]))


abs_before = data.frame(Stream, source, mean, sd, q1, q2, q3)
abs_before$source_bi = NA
abs_before[abs_before$source == "Deterministic\nVariation",8] = "Det"
abs_before[abs_before$source != "Deterministic\nVariation",8] = "P_O"

dummy_abs_before = abs_before
dummy_abs_before[dummy_abs_before$source == "Deterministic\nVariation",3] = 0.3
dummy_abs_before[dummy_abs_before$source != "Deterministic\nVariation",3] = 0.01

plot_abs_before = ggplot(abs_before, aes(fill=Stream, y=mean, x=source)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.75) +
  labs(x="Before Flooding Event",y="Temporal Variability", tag ="C)") +
  geom_errorbar(aes(ymin=q1, ymax=q3), width=0.5, position=position_dodge(.9),
                colour="black", alpha=0.9, size=0.5) +
  facet_wrap(~source_bi,scales = "free") +
  geom_blank(data = dummy_abs_before) +
  
  theme_classic() +
  theme(strip.text.x = element_blank(),strip.background = element_blank(),
        axis.text.y=element_text(colour = "black"),
        axis.text.x=element_text(colour = "black"),
        axis.line.x = element_line(linewidth = 0.25),
        axis.line.y = element_line(linewidth = 0.25),
        axis.title.x = element_text(vjust = 0.4, size = 13)) 
plot_abs_before


### absolute values, AFTER flooding event ### 

mean=as.vector(t(df_whole[7:9,c(1,6,11,16)]))
sd=as.vector(t(df_whole[7:9,c(2,7,12,17)]))
q1=as.vector(t(df_whole[7:9,c(3,8,13,18)]))
q2=as.vector(t(df_whole[7:9,c(4,9,14,19)]))
q3=as.vector(t(df_whole[7:9,c(5,10,15,20)]))


abs_after = data.frame(Stream, source, mean, sd, q1, q2, q3)
abs_after$source_bi = NA
abs_after[abs_after$source == "Deterministic\nVariation",8] = "Det"
abs_after[abs_after$source != "Deterministic\nVariation",8] = "P_O"

dummy_abs_after = abs_after
dummy_abs_after[dummy_abs_after$source == "Deterministic\nVariation",3] = 0.3
dummy_abs_after[dummy_abs_after$source != "Deterministic\nVariation",3] = 0.01

plot_abs_after = ggplot(abs_after, aes(fill=Stream, y=mean, x=source)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.75) +
  labs(x="After Flooding Event",y="Temporal Variability", tag ="D)") +
  
  geom_errorbar(aes(ymin=q1, ymax=q3), width=0.5, position=position_dodge(.9),
                colour="black", alpha=0.9, size=0.5) +
  facet_wrap(~source_bi,scales = "free") +
  geom_blank(data = dummy_abs_after) + 
  theme_classic() +
  theme(strip.text.x = element_blank(),strip.background = element_blank(),
        axis.text.y=element_text(colour = "black"),
        axis.text.x=element_text(colour = "black"),
        axis.line.x = element_line(linewidth = 0.25),
        axis.line.y = element_line(linewidth = 0.25),
        axis.title.x = element_text(vjust = 0.4, size = 13)) 
plot_abs_after

### pr values, BEFORE flooding event ### 

source_p=c(rep("Deterministic\nVariation",4),
         rep("Process\nNoise",4),
         rep("Observation\nError",4)
)

mean=as.vector(t(df_whole[4:6,c(1,6,11,16)]))
sd=as.vector(t(df_whole[4:6,c(2,7,12,17)]))
q1=as.vector(t(df_whole[4:6,c(3,8,13,18)]))
q2=as.vector(t(df_whole[4:6,c(4,9,14,19)]))
q3=as.vector(t(df_whole[4:6,c(5,10,15,20)]))

pr_before = data.frame(Stream, source_p, mean, sd, q1, q2, q3)
pr_before$source_bi = NA
pr_before[pr_before$source_p == "Deterministic\nVariation",8] = "Det"
pr_before[pr_before$source_p != "Deterministic\nVariation",8] = "P_O"

dummy_pr_before = pr_before
dummy_pr_before[dummy_pr_before$source_p == "Deterministic\nVariation",3] = 100
dummy_pr_before[dummy_pr_before$source_p != "Deterministic\nVariation",3] = 4.5

plot_pr_before = ggplot(pr_before, aes(fill=Stream, y=mean, x=source_p)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.75) +
  labs(x="Before Flooding Event",y="Variability [%]", tag ="A)") +
  
  geom_errorbar(aes(ymin=q1, ymax=q3), width=0.5, position=position_dodge(.9),
                colour="black", alpha=0.9, size=0.5) +
  facet_wrap(~source_bi,scales = "free") +
  geom_blank(data = dummy_pr_before) +
  theme_classic() +
  theme(strip.text.x = element_blank(),strip.background = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_text(colour = "black"),
        axis.line.x = element_line(linewidth = 0.25),
        axis.line.y = element_line(linewidth = 0.25),
        axis.title.x = element_text(vjust = 0.4, size = 13))
plot_pr_before

### pr values, AFTER flooding event ### 

mean=as.vector(t(df_whole[10:12,c(1,6,11,16)]))
sd=as.vector(t(df_whole[10:12,c(2,7,12,17)]))
q1=as.vector(t(df_whole[10:12,c(3,8,13,18)]))
q2=as.vector(t(df_whole[10:12,c(4,9,14,19)]))
q3=as.vector(t(df_whole[10:12,c(5,10,15,20)]))

pr_after = data.frame(Stream, source_p, mean, sd, q1, q2, q3)
pr_after$source_bi = NA
pr_after[pr_after$source_p == "Deterministic\nVariation",8] = "Det"
pr_after[pr_after$source_p != "Deterministic\nVariation",8] = "P_O"

dummy_pr_after = pr_after
dummy_pr_after[dummy_pr_after$source_p == "Deterministic\nVariation",3] = 100
dummy_pr_after[dummy_pr_after$source_p != "Deterministic\nVariation",3] = 4.5

plot_pr_after = ggplot(pr_after, aes(fill=Stream, y=mean, x=source_p)) + 
  geom_bar(position="dodge", stat="identity", alpha = 0.75) +
  labs(x="After Flooding Event",y="Variability [%]", tag ="B)") +
  
  geom_errorbar(aes(ymin=q1, ymax=q3), width=0.5, position=position_dodge(.9),
                colour="black", alpha=0.9, size=0.5) +
  facet_wrap(~source_bi,scales = "free") +
  geom_blank(data = dummy_pr_after) +
  theme_classic() +
  theme(strip.text.x = element_blank(),strip.background = element_blank(),
        axis.text.x=element_blank(), axis.ticks.x=element_blank(),
        axis.text.y=element_text(colour = "black"),
        axis.line.x = element_line(linewidth = 0.25),
        axis.line.y = element_line(linewidth = 0.25),
        axis.title.x = element_text(vjust = 0.4, size = 13))
plot_pr_after

##### put all together #######

reso <- 300
length <- 3.25*reso/72


png(file="Fig3_260828.png",width=length/1.6, height=length/2.6, res = reso, units = "in");

  plot_pr_before+
  plot_pr_after+
  plot_abs_before +
  plot_abs_after +
plot_layout(axis_titles = "collect",
            ncol = 2, nrow =2, guides = "collect");
dev.off()
            
