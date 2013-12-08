#q1
data(warpbreaks)
str(warpbreaks)
aovObject <- aov(warpbreaks$breaks ~ warpbreaks$wool + warpbreaks$tension)
summary(aovObject)
# The degrees of freedom for tension is 2 and the F-statistic is 7.537
#q2
probability <- 0.2
odds <- probability / (1 - probability)
logodds <- log(odds)
paste("Log odds are", logodds)
#q3
library(glm2)
data(crabs)
summary(crabs$Satellites)
plot(crabs$Width,crabs$Satellites,pch=19,col="darkgrey",xlab="Width",ylab="Satellites")
poisson <- glm(crabs$Satellites ~ crabs$Width,family="poisson")
# poisson$fitted
lines(crabs$Width,poisson$fitted,col="blue",lwd=3)
# summary(poisson)
paste("The multiplicative difference is ",round(exp(0.16405),4))
#q4
crabs <- exp(-3.30476+0.16405*22)
crabs
#q5
library("MASS", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")
data(quine)
lm1 = lm(log(Days + 2.5) ~.,data=quine)
aicmodel <- step(lm1)
