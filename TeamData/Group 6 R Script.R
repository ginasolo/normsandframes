library(readxl)
G6Data <- read_excel("Group 6 - n173.xlsx")
View(G6Data)
attach(G6Data)

#Anova's
main<-aov(Percentage~factor(Frame)*factor(Norm))
summary(main)

independent<-aov(Percentage~Condition)
summary(independent)

all<-aov(Percentage~Frame*Norm+Loc+Gender)
summary(all)

all.cond<-aov(Percentage~Condition+Loc+Gender)
summary(all.cond)


#Sample Means Tests
t.test(Percentage~Frame)
t.test(Percentage~Norm)
wilcox.test(Percentage~Frame)


#Regressions
lm1<-lm(Percentage~Frame*Norm+Risk_1+Gender)
summary(lm1)

lm2<-lm(Percentage~Condition+Risk_1+Gender)
summary(lm2)


