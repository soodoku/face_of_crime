"

Law & Order, SVU, and CI

1. Gender and Race of Criminals and Victims (Aggregated)
2. Comparison to NCVS, UCR, and Census 

"

# Set directory
setwd(basedir)
setwd("face_of_crime/")

# Set library
library(ggplot2)
library(dplyr)
library(reshape2)

# Execute data cleaner
source("scripts/01_get_data_ready.R")

# Merge
# 
lo_ci$series  <- "Criminal Intent"
lo_svu$series <- "SVU"
lo_or$series  <- "Original"

lo_all <- rbind(lo_ci, lo_svu, lo_or)



# Total shows, n_victims, n_criminals
nrow(lo_all)
sum(as.numeric(lo_all$n_victims), na.rm=T)
sum(as.numeric(lo_all$n_criminals), na.rm=T)


# -----------
# Custom theme

cust_theme <- 
theme_minimal() +
theme(panel.grid.major.x = element_blank(),
	  panel.grid.major.y = element_line(color="#e7e7e7",  linetype = "dotted"),
	  panel.grid.minor   = element_blank(),
	  legend.position    ="none",
	  legend.key         = element_blank(),
	  legend.key.width   = unit(1,"cm"),
	  axis.title   = element_text(size=10, color="#555555"),
	  axis.text    = element_text(size=8, color="#555555"),
	  axis.ticks.y = element_blank(),
	  axis.title.x = element_text(vjust=-1),
	  axis.title.y = element_text(vjust= 1),
	  axis.ticks.x = element_line(color="#e3e3e3", size=.2),
	  plot.margin = unit(c(0,1,.5,.5), "cm"))

#------------

# Plot 1a
# Victims by Gender

out <- 
lo_all %>%
group_by(series) %>%
summarise(male=sum(n_v_male, na.rm=T), female=sum(n_v_female, na.rm=T), pfemale = female*100/sum(male, female))

rout <- melt(out[,c("series", "pfemale")])

ggplot(rout, aes(series, value)) + 
geom_bar(stat = "identity", width=.25, position="dodge", fill="#c9c9c9", colour="#c9c9c9") + 
scale_y_continuous(name="Percentage of Female Victims", limits=c(0,100), breaks=seq(0,100,10), labels=paste0(seq(0,100,10), "%")) +
xlab("") +
cust_theme

ggsave("figs/all_victims_by_gender.pdf", width=3.5, height=3.5)

# Plot 1b
# CI Perpetrators by Gender

out <- 
lo_all %>%
group_by(series) %>%
summarise(male=sum(n_c_male, na.rm=T), female=sum(n_c_female, na.rm=T), pfemale = female*100/sum(male, female))

rout <- melt(out[,c("series", "pfemale")])

ggplot(rout, aes(series, value, fill=variable)) + 
geom_bar(stat = "identity", width=.25, position="dodge", fill="#c9c9c9", colour="#c9c9c9") + 
scale_y_continuous(name="Percentage of Female Perpetrators", limits=c(0,100), breaks=seq(0,100,10), labels=paste0(seq(0,100,10), "%")) +
xlab("") +
cust_theme 

ggsave("figs/all_criminals_by_gender.pdf", width=3.5, height=3.5)

# Plot 2b
# CI Victims by Race

out <- 
lo_all %>%
group_by(series) %>%
summarise(white=sum(n_v_white, na.rm=T), black=sum(n_v_black, na.rm=T), pblack = black*100/sum(white, black))

rout <-  melt(out[,c("series", "pblack")])

ggplot(rout, aes(series, value, fill=variable)) + 
geom_bar(stat = "identity", width=.25, position="dodge", fill="#c9c9c9", colour="#c9c9c9") + 
scale_y_continuous(name="Percentage of Black Victims", limits=c(0,100), breaks=seq(0,100,10), labels=paste0(seq(0,100,10), "%")) +
xlab("") +
cust_theme

ggsave("figs/all_victims_by_race.pdf", width=3.5, height=3.5)

# Plot 2b
# CI Perpetrators  by Race

out <- 
lo_all %>%
group_by(series) %>%
summarise(white=sum(n_c_white, na.rm=T), black=sum(n_c_black, na.rm=T), pblack = black*100/sum(white, black))

rout <- melt(out[,c("series", "pblack")])

ggplot(rout, aes(series, value, fill=variable)) + 
geom_bar(stat = "identity", width=.25, position="dodge", fill="#c9c9c9", colour="#c9c9c9") + 
scale_y_continuous(name="Percentage of White and Black Perpetrators", limits=c(0,100), breaks=seq(0,100,10), labels=paste0(seq(0,100,10), "%")) +
xlab("") +
cust_theme

ggsave("figs/all_criminals_by_race.pdf", width=3.5, height=3.5)
