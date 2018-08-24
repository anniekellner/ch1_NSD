rm(list = ls())

library(dplyr)
library(lubridate)

load("ows_land.RData")
load("pb_traj_df.RData")

refda <- parse_date_time(paste(min(ows.land$datetime)), orders = 'ymd HMS', tz = 'US/Alaska') #set reference date 


sum <- as.table(tapply(pb.traj.df$dt, pb.traj.df$burst, mean, na.rm=T))#mean dt for each burst
sum<- as.data.frame(sum)
colnames(sum) <- c("burst","dt_seconds")
sum <- mutate(sum, hours=dt_seconds/3600) #convert seconds to hours

#write.csv(sum, file="fix_intervals.csv")
pb.traj.df$burst <- as.character(pb.traj.df$burst)
one <- subset(pb.traj.df, burst== "pb_06810_2008" | burst=="pb_20414_2009" | burst=="pb_20446_2009" | burst=="pb_20520_2012"|
              burst=='pb_20586_2008' | burst=='pb_20735_2009' | burst=='pb_20741_2008' | burst=='pb_20845_2015' | burst=='pb_20965_2008'|
              burst=='pb_20966_2008'| burst=='pb_20975_2008'| burst=='pb_20982_2008'| burst=='pb_21343_2015'| burst=='pb_21380_2015'|
              burst=='pb_32255_2008'| burst=='pb_32282_2008' | burst=='pb_32608_2008'| burst== 'pb_32698_2015')


