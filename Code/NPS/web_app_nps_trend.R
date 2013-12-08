date <- readline("What s the value of date?")
nps_score <- function(data)
{
  percent_promoters <- nrow(subset(data,subset=data$Rating>=9&data$Date==date_vector))/nrow(data)*100
  percent_detractors <- nrow(subset(data,subset=data$Rating<=6&data$Date==date_vector))/nrow(data)*100
  nps_score <- round(percent_promoters - percent_detractors,0)
}
sample <- subset(satisfaction_cleaned,subset=satisfaction_cleaned$Date=="2013-12-03")
nps <- nps_score(sample)

date_vector <- unique(satisfaction_cleaned$Date)
score_vector <- nps_score(date_vector)
