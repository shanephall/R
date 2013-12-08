cluster(satisfaction_cleaned$Rating,satisfaction_cleaned$Message)
unique(subset(satisfaction_cleaned$Message,subset=satisfaction_cleaned$Rating==0))

daysago <- as.Date(max(satisfaction_cleaned$Date) - 7)
today <- as.Date(max(satisfaction_cleaned$Date))
last7days <- c(paste(today):paste(daysago))
scan("stdin")