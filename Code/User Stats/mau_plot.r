data<-read.csv("user_stats.csv")
data<-data[complete.cases(data),]
data$Date<-factor(data$Date)
pdf("MAU_plot.pdf")
plot(x=data$Date,y=data$Mau,pch=20,xlab="Date",ylab="Mau",main="Monthly Active Users")
lines(data$Date,data$Mau,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Mau)+750,mean(data$Mau)+750,max(data$Mau)-750),x=c(15,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Mau)),sep = ": "),paste("Mean",as.character(round(mean(data$Mau),0)),sep = ": "),paste("Max",as.character(max(data$Mau)),sep = ": ")),col=c(rgb(0,0,0,150,max=255),rgb(0,0,0,150,max=255),rgb(0,0,0,150,max=255)))
abline(h=c(min(data$Mau),mean(data$Mau),max(data$Mau)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
dev.off()

