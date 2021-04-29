# if not already installed 
install.packages("fGarch")

source('Functions Needed in THQ paper.R')
pdf("Figures 1 to 3.pdf")

library("fGarch")
set.seed(100371)

###############################################
#Producing Figure 1 ###########################
###############################################

par(mfrow=c(1,1))

plotNdensity.fun(c(2.1,1.7,2.1),c(0.2,0.5,0.1), maxYaxis=4.5, xaxisrange=c(0.5,3.25),col=c("black","gray78","black"),lty=c(1,1,3))
legend(1,4.6, box.lty=0, c(expression(mu["A"]%~%N(2.1,0.2^2)),expression(mu["B"]%~%N(1.7,0.5^2)),expression(mu["C"]%~%N(2.1,0.1^2))), lty = c(1,1,3^2),  col=c("black","gray78","black"),lwd=2)
abline(v=2.5,lty=6)
###############################################
#Producing Figure 2 ###########################
###############################################

par(mfrow=c(2,1))

plotNdensity.fun(c(10,1,2,3),c(3,3,3,3), maxYaxis=0.4, xaxisrange=c(-6,15),  cols=c(1:4),lty=c(1,1,1,1))
title("Scenario 1")
legend(-6,0.4,c(expression(mu["P"]%~%N(10,3^2)),expression(mu["A"]%~%N(1,3^2)),expression(mu["B"]%~%N(2,3^2)),expression(mu["C"]%~%N(3,3^2))), 
       lty = rep(1,4),  col=1:4,lwd=2) 

plotNdensity.fun(c(10,1,1,1),c(3,1,3,5), maxYaxis=0.4, xaxisrange=c(-6,15),  cols=1:4)
abline(v=1)
legend(5,0.4,c(expression(mu["P"]%~%N(10,3^2)),expression(mu["A"]%~%N(1,1^2)),expression(mu["B"]%~%N(1,3^2)),expression(mu["C"]%~%N(1,5^2))), 
       lty = rep(1,4),  col=1:4,lwd=2) 
title("Scenario 2")

###############################################
#Producing Figure 3 ###########################
###############################################

Sucras.App<-c()
Pbest.App<-c()
MeanRank.App<-c()
a<-c()
for (i in seq(1,10,0.5))
{a<-relativeranking.fun(c(-2,1,1.5,2),c(1,1,1,i))
Sucras.App<-rbind(Sucras.App,a$SUCRA)
Pbest.App<-rbind(Pbest.App,a$Pbest)
MeanRank.App<-rbind(MeanRank.App,a$MeanRank)
}

par(mfrow=c(2,1))
plot(c(1,10),c(0,1),type="n", xlab="SD in treatment C",ylab="SUCRA")
lines(seq(1,10,0.5),Sucras.App[,1], lty=1,col=1, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,2], lty=1,col=2, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,3], lty=1,col=3, lwd=2)
lines(seq(1,10,0.5),Sucras.App[,4], lty=1,col=4, lwd=2)

legend(0.7,0.95, c("P","A","B","C" ), lty=1, col = 1:4, lwd=rep(2,4),cex=0.6) 

plot(c(1,10),c(0,1),type="n", xlab="SD in treatment C",ylab="P(best outcome)")
lines(seq(1,10,0.5),Pbest.App[,1], lty=1,col=1, lwd=2)
lines(seq(1,10,0.5),Pbest.App[,2], lty=1,col=2, lwd=2)
lines(seq(1,10,0.5),Pbest.App[,3], lty=1,col=3,lwd=2)
lines(seq(1,10,0.5),Pbest.App[,4], lty=1,col=4, lwd=2)

legend(0.7,0.95, c("P","A","B","C" ), lty=1, col = 1:4, lwd=rep(2,4),cex=0.6) 

dev.off()




###############################################
#Producing Table 3 ###########################
###############################################
set.seed(100371)
scen1=relativeranking.fun(c(10,1,2,3),c(3,3,3,3))
set.seed(100371)
scen2=relativeranking.fun(mu=c(10, 1,1,1),sigma=c(3,1,3,5))

scen1$Pscore=1-scen1$Pscore
scen2$Pscore=1-scen2$Pscore


Table1=matrix(unlist(c(scen1,scen2)),ncol=4,byrow=T)
Table1[c(1,2,5,6,7,10),]<-Table1[c(1,2,5,6,7,10),]*100
Table3<-round(Table1,1)

colnames(Table3)=c("P","A","B","C")
Table3<-Table3[c(5,2,1,3,4,10,7,6,8,9),]
rownames(Table3)=rep(names(scen1)[c(5,2,1,3,4)],2)

sink("Table 3.txt")
cat("\n \n TABLE3 \n \n")
print(Table3)
sink()

###############################################
#Producing Table 4  ###########################
###############################################
set.seed(100371)
a<-rbind(relativeranking.fun(c(1,2,3,10),c(3,3,3,3))$SUCRA,
         relativeranking.fun(c(1,2,3,10),c(10,3,3,3))$SUCRA,
         relativeranking.fun(c(1,2,3,10),c(15,3,3,3))$SUCRA,
         relativeranking.fun(c(1,2,3,10),c(20,3,3,3))$SUCRA)

Table4<-round(a*100,2)
print(Table4)

sink("Table 4.txt")
cat("\n \n TABLE 4 \n \n")
print(Table4)

sink()



rm(list=ls())
