#sample data sets
df<-read.csv('O:\\HR Department\\HR Staff\\Shiv Rajendran\\Metrics for 2018\\2018 Employee data.csv')

#import packages and view dataset
str(df)
library(readr)

#Convert currency to numerics
df$Annual.Salary=parse_number(df$Annual.Salary)

#seperate sets
males=df[df$Gender=="Male",]
females=df[df$Gender=="Female",]

#std deviation
sd.males=sd(males$Annual.Salary)
sd.females=sd(females$Annual.Salary)

#means
mean.males=mean(males$Annual.Salary)
mean.females=mean(females$Annual.Salary)

#STD deviation of difference in salary
sd.dif= sqrt(sd.males^2/length(males$Annual.Salary)
             + sd.females^2/length(females$Annual.Salary)
             )

#z score
zeta=(mean.males-mean.females)/sd.dif

#plot
plot(x=seq(from = -5, to= 5, by=0.1),y=dnorm(seq(from = -5, to= 5,  by=0.1),mean=0),type='l',xlab = 'mean difference',  ylab='possibility')
abline(v=zeta, col='red')


p = 1-pnorm(zeta)
p
