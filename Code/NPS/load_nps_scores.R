


# look at the # of responses per day
# breaks <- unique(satisfaction_cleaned$Date)

# nov23rd <- subset(satisfaction_cleaned,satisfaction_cleaned$Date=="2013-11-23")
# nov24th <- subset(satisfaction_cleaned,satisfaction_cleaned=="2013-11-24")
# head(nov23rd)

# hist(satisfaction_cleaned$Rating,breaks=unique(satisfaction_cleaned$Rating))
hist(satisfaction_cleaned$Date,breaks=unique(satisfaction_cleaned$Date))

# look at the distribution of satisfaction_cleaned scores for all time
# summary(satisfaction_cleaned$Rating) #look at a summary of the Ratings variable
# str(satisfaction_cleaned) #look at the structure of the data
# head(satisfaction_cleaned) #check out the first few observations to get a sense of the variables
# ratings_112613 <- subset(x=satisfaction_cleaned,subset=satisfaction_cleaned$Date == "2013-11-26") #create a subset that includes ratings from 11/26 only

# calculate NPS score


# make a histogram w/ all time responses

# plot(x=satisfaction_cleaned$Date,y=satisfaction_cleaned$Rating,type="h")
# plot(satisfaction_raw$Rating ~ satisfaction_raw$Date)
# plot(satisfaction_raw$Rating ~ satisfaction_raw$Date)
# dev.off()

# load data for app from Mixpanel
app_sat_raw <- read.csv(file=file.choose(),header=T)
# str(app_sat_raw)
# hist(app_sat_raw)
all_app_responders <- sum(app_sat_raw$X0,app_sat_raw$X1,app_sat_raw$X2,app_sat_raw$X3,app_sat_raw$X4,app_sat_raw$X5)
app_promoters <- sum(app_sat_raw$X5)
app_detractors <- sum(app_sat_raw$X3,app_sat_raw$X2,app_sat_raw$X1,app_sat_raw$X0)
percent_app_promoters <- (app_promoters / all_app_responders)*100
percent_app_detractors <- (app_detractors / all_app_responders)*100
app_nps <- round((percent_app_promoters - percent_app_detractors))
paste("App NPS is ",app_nps,"%",sep="")
# hist(app_sat_raw$X5)
# plot(y=app_sat_raw, x=app_sat_raw$Date)

