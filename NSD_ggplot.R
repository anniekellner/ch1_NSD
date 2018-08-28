rm(list = ls())
library(adehabitatLT)
library(tidyverse)
library(lubridate)

load("pb_traj_df.RData")

pb.traj.df$DM <- paste(day(pb.traj.df$date),month(pb.traj.df$date))
uni <- unique(pb.traj.df$burst)

for(i in 1:length(uni)){
  temp <- subset(x = pb.traj.df, burst== uni[[i]])}
  ggplot(temp, aes(date,R2n)) + geom_point() + 
  scale_y_continuous(labels = NULL) + theme(axis.title.x=element_blank())


for(i in 1:length(uni)){
  temp <- subset(x = pb.traj.df, burst== uni[[i]])
  print(ggplot(temp, aes(date,R2n)) + geom_point() + scale_y_continuous(labels = NULL) + theme(axis.title.x=element_blank()) + labs(caption = paste(uni[[i]])))
  ggsave(paste(uni[[i]],'.png',sep=''), width = 3,height = 3, units = 'in')}
  
  
test <- subset(pb.traj.df, burst=="pb_20529_2004")
  
  
 

