data<-read.csv("user_stats.csv")
data<-data[complete.cases(data),]
data$Date<-factor(data$Date)
pdf("Quitter_plot.pdf")
plot(x=data$Date,y=data$Quitters,pch=20,xlab="Date",ylab="Quitters",main="Quitters")
lines(data$Date,data$Quitters,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Quitters)+100,mean(data$Quitters)+100,max(data$Quitters)-100),x=c(-1,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Quitters)),sep = ": "),paste("Mean",as.character(round(mean(data$Quitters),0)),sep = ": "),paste("Max",as.character(max(data$Quitters)),sep = ": ")),col=c(rgb(0,0,0,150,max=255),rgb(0,0,0,150,max=255),rgb(0,0,0,150,max=255)))
abline(h=c(min(data$Quitters),mean(data$Quitters),max(data$Quitters)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
dev.off()