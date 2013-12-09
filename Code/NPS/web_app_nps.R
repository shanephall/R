# LOAD NPS DATA FROM CSV #
 
load_nps_data <- function() 
{
  nps_data <<- read.csv(file=file.choose(),header=T) # load data from local CSV
  nps_data$Date <<- as.Date(nps_data$Date) # convert date format
  nps_data$Rating <<- as.numeric(nps_data$Rating) # convert rating format
  return(nps_data)
}

nps_summary <- function()
{
  par(mfrow=c(2,2))
  a <- as.numeric(readline("How many days for chart 1,1? "))
  b <- as.numeric(readline("How many days for chart 1,2? "))
  c <- as.numeric(readline("How many days for chart 2,1? "))
  d <- as.numeric(readline("How many days for chart 2,2? "))
  nps_chart(TRUE,a)
  nps_chart(TRUE,b)
  nps_chart(TRUE,c)
  nps_chart(TRUE,d)
}

nps_score <- function(data)
{
  percent_promoters <- nrow(subset(data,subset=data$Rating>=9))/nrow(data)*100 
  percent_detractors <- nrow(subset(data,subset=data$Rating<=6))/nrow(data)*100 
  score <- round(percent_promoters - percent_detractors,0)
  return(score)
}

nps_chart <- function(override,days)
{
  if(override==FALSE)
  {
    days <- as.numeric(readline("How many days back? (0 for alltime)  "))
    fresh <- readline("Load fresh data? (YES / NO)  ")
  }
  else
  {
    fresh <- "NO"
  }
  data <- nps_data
  if(fresh=="YES")
  {
    data <- load_nps_data()
  }
  if(days>0)
  {    
    data <- subset(data,data$Date>=max(data$Date)-days) # truncate data to the selected range
  }  
  percent_0 <- paste(round(nrow(subset(data,subset=data$Rating==0))/nrow(data)*100),"%",sep="")
  percent_1 <- paste(round(nrow(subset(data,subset=data$Rating==1))/nrow(data)*100),"%",sep="")
  percent_2 <- paste(round(nrow(subset(data,subset=data$Rating==2))/nrow(data)*100),"%",sep="")
  percent_3 <- paste(round(nrow(subset(data,subset=data$Rating==3))/nrow(data)*100),"%",sep="")
  percent_4 <- paste(round(nrow(subset(data,subset=data$Rating==4))/nrow(data)*100),"%",sep="")
  percent_5 <- paste(round(nrow(subset(data,subset=data$Rating==5))/nrow(data)*100),"%",sep="")
  percent_6 <- paste(round(nrow(subset(data,subset=data$Rating==6))/nrow(data)*100),"%",sep="")
  percent_7 <- paste(round(nrow(subset(data,subset=data$Rating==7))/nrow(data)*100),"%",sep="")
  percent_8 <- paste(round(nrow(subset(data,subset=data$Rating==8))/nrow(data)*100),"%",sep="")
  percent_9 <- paste(round(nrow(subset(data,subset=data$Rating==9))/nrow(data)*100),"%",sep="")
  percent_10 <- paste(round(nrow(subset(data,subset=data$Rating==10))/nrow(data)*100),"%",sep="")
  promoters <- nrow(subset(data,subset=data$Rating>=9))/nrow(data)*100 # calculate % promoters, ie 9's & 10's
  detractors <- nrow(subset(data,subset=data$Rating<=6))/nrow(data)*100 # calculate % detractors, ie 0's through 6's
  nps_score <- round(promoters - detractors,0) # calculate NPS score
  chart_label_daterange <- if(min(data$Date)==max(data$Date)){max(data$Date)}else{paste(min(data$Date)," - ",max(data$Date))}
  chart_height <- as.numeric(round((1.05*(nrow(subset(data,subset=data$Rating>=9))))))
  histinfo <- hist(data$Rating,ylab="Responses",xlab="Score",labels=c(paste(percent_0),paste(percent_1),paste(percent_2),paste(percent_3),paste(percent_4),paste(percent_5),paste(percent_6),paste(percent_7),paste(percent_8),paste(percent_9),paste(percent_10)),col=c("#e25825","#e25825","#e25825","#e25825","#e25825","#e25825","#e25825","#f5e14d","#f5e14d","#9fc03f","#9fc03f"),border="white",main=paste("THEHUNT.COM - NPS \n",chart_label_daterange),breaks=c(-.5,.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),include.lowest=TRUE,ylim=c(0,chart_height))
  text_height <- round(nrow(subset(data,subset=data$Rating == 10))*.90)
  text(4,text_height, paste("Overall Score - ",nps_score,"%\nTotal Responses - ",as.character(nrow(data)),sep=""))
}

nps_chart_singleday <- function(silent)
{
  date <- readline("What date? (YYYY-MM-DD)  ")
  fresh <- readline("Load fresh data? (YES / NO)  ")
  data <- nps_data
  if(fresh=="YES")
  {
    data <- load_nps_data()
  } 
  data <- subset(data,data$Date==date) # truncate data to the selected date 
  global <<- data
  percent_0 <- paste(round(nrow(subset(data,subset=data$Rating==0))/nrow(data)*100),"%",sep="")
  percent_1 <- paste(round(nrow(subset(data,subset=data$Rating==1))/nrow(data)*100),"%",sep="")
  percent_2 <- paste(round(nrow(subset(data,subset=data$Rating==2))/nrow(data)*100),"%",sep="")
  percent_3 <- paste(round(nrow(subset(data,subset=data$Rating==3))/nrow(data)*100),"%",sep="")
  percent_4 <- paste(round(nrow(subset(data,subset=data$Rating==4))/nrow(data)*100),"%",sep="")
  percent_5 <- paste(round(nrow(subset(data,subset=data$Rating==5))/nrow(data)*100),"%",sep="")
  percent_6 <- paste(round(nrow(subset(data,subset=data$Rating==6))/nrow(data)*100),"%",sep="")
  percent_7 <- paste(round(nrow(subset(data,subset=data$Rating==7))/nrow(data)*100),"%",sep="")
  percent_8 <- paste(round(nrow(subset(data,subset=data$Rating==8))/nrow(data)*100),"%",sep="")
  percent_9 <- paste(round(nrow(subset(data,subset=data$Rating==9))/nrow(data)*100),"%",sep="")
  percent_10 <- paste(round(nrow(subset(data,subset=data$Rating==10))/nrow(data)*100),"%",sep="")
  bucket_sizes <- c(percent_0,percent_1,percent_2,percent_3,percent_4,percent_5,percent_6,percent_7,percent_8,percent_9,percent_10)
  promoters <- nrow(subset(data,subset=data$Rating>=9))/nrow(data)*100 # calculate % promoters, ie 9's & 10's
  detractors <- nrow(subset(data,subset=data$Rating<=6))/nrow(data)*100 # calculate % detractors, ie 0's through 6's
  nps_score <- round(promoters - detractors,0) # calculate NPS score
  if(silent==FALSE)
  {
    chart_label_daterange <- date
    chart_height <- as.numeric(round((1.05*(nrow(subset(data,subset=data$Rating>=9))))))
    histinfo <- hist(data$Rating,ylab="Responses",xlab="Score",labels=c(paste(percent_0),paste(percent_1),paste(percent_2),paste(percent_3),paste(percent_4),paste(percent_5),paste(percent_6),paste(percent_7),paste(percent_8),paste(percent_9),paste(percent_10)),col=c("#e25825","#e25825","#e25825","#e25825","#e25825","#e25825","#e25825","#f5e14d","#f5e14d","#9fc03f","#9fc03f"),border="white",main=paste("THEHUNT.COM - NPS \n",chart_label_daterange),breaks=c(-.5,.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5,10.5),include.lowest=TRUE,ylim=c(0,chart_height))  
    text_height <- round(nrow(subset(data,subset=data$Rating == 10))*.90)
    text(4,text_height, paste("Overall Score - ",nps_score,"%\nTotal Responses - ",as.character(nrow(data)),sep=""))
  }
}


