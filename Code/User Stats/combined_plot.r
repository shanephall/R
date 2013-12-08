data<-read.csv(file.choose())
data<-data[complete.cases(data),]
data$Date<-factor(data$Date)
pdf("mixed_plot.pdf")
par(mfrow=c(2,2))
# Begin Quitters
plot(x=data$Date,y=data$Quitters,pch=20,xlab="Date",ylab="Quitters",main="Quitters")
lines(data$Date,data$Quitters,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Quitters)+(max(data$Quitters)-min(data$Quitters))*.05,mean(data$Quitters)+(max(data$Quitters)-min(data$Quitters))*.05,max(data$Quitters)-(max(data$Quitters)-min(data$Quitters))*.07),x=c(-1,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Quitters)),sep = ": "),paste("Mean",as.character(round(mean(data$Quitters),0)),sep = ": "),paste("Max",as.character(max(data$Quitters)),sep = ": ")),col=c(rgb(200,0,0,200,max=255),rgb(0,0,200,200,max=255),rgb(0,200,0,200,max=255)))
abline(h=c(min(data$Quitters),mean(data$Quitters),max(data$Quitters)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
# End Quitters
# Begin Signups
plot(x=data$Date,y=data$Signups,pch=20,xlab="Date",ylab="Signups",main="Signups")
lines(data$Date,data$Signups,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Signups)+(max(data$Signups)-min(data$Signups))*.05,mean(data$Signups)+(max(data$Signups)-min(data$Signups))*.05,max(data$Signups)-(max(data$Signups)-min(data$Signups))*.07),x=c(-1,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Signups)),sep = ": "),paste("Mean",as.character(round(mean(data$Signups),0)),sep = ": "),paste("Max",as.character(max(data$Signups)),sep = ": ")),col=c(rgb(200,0,0,200,max=255),rgb(0,0,200,200,max=255),rgb(0,200,0,200,max=255)))
abline(h=c(min(data$Signups),mean(data$Signups),max(data$Signups)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
# Begin Reactivations
plot(x=data$Date,y=data$Reactivations,pch=20,xlab="Date",ylab="Reactivations",main="Reactivations")
lines(data$Date,data$Reactivations,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Reactivations)+(max(data$Reactivations)-min(data$Reactivations))*.05,mean(data$Reactivations)+(max(data$Reactivations)-min(data$Reactivations))*.05,max(data$Reactivations)-(max(data$Reactivations)-min(data$Reactivations))*.07),x=c(-1,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Reactivations)),sep = ": "),paste("Mean",as.character(round(mean(data$Reactivations),0)),sep = ": "),paste("Max",as.character(max(data$Reactivations)),sep = ": ")),col=c(rgb(200,0,0,200,max=255),rgb(0,0,200,200,max=255),rgb(0,200,0,200,max=255)))
abline(h=c(min(data$Reactivations),mean(data$Reactivations),max(data$Reactivations)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
# End Reactivations
# Begin MAUs
plot(x=data$Date,y=data$Maus,pch=20,xlab="Date",ylab="Maus",main="Maus")
lines(data$Date,data$Maus,lwd=2,col=rgb(10,10,10,100,max=255))
text(y=c(min(data$Maus)+(max(data$Maus)-min(data$Maus))*.05,mean(data$Maus)+(max(data$Maus)-min(data$Maus))*.05,max(data$Maus)-(max(data$Maus)-min(data$Maus))*.07),x=c(-1,-1,-1),pos=c(4,4,4),labels=c(paste("Min",as.character(min(data$Maus)),sep = ": "),paste("Mean",as.character(round(mean(data$Maus),0)),sep = ": "),paste("Max",as.character(max(data$Maus)),sep = ": ")),col=c(rgb(200,0,0,200,max=255),rgb(0,0,200,200,max=255),rgb(0,200,0,200,max=255)))
abline(h=c(min(data$Maus),mean(data$Maus),max(data$Maus)),col=c(rgb(200,0,0,100,max=255),rgb(0,0,200,100,max=255),rgb(0,200,0,100,max=255)),lwd=4,lty="solid")
abline(v=c(1,1+7*1,1+7*2,1+7*3,1+7*4,1+7*5,1+7*6,1+7*7,1+7*8,1+7*9,1+7*10),col=rgb(0,0,0,.25),lwd=2,lty="dotted")
# End MAUs
dev.off()