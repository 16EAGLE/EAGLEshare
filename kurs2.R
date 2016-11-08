x <- matrix(c(4,7,3,8,9,2),nrow = 2)
x
x[2,2]
x[,2]

number_1 <- rnorm(80,mean=0,sd=1)
number_1

mat_1 <- matrix(number_1,nrow = 20,ncol = 4)
mat_1

df_1 <- data.frame(mat_1)
df_1

names(df_1) <- c("var1","var2","var3","var4")

head(df_1)
plot(df_1)

x <- seq(1,100,by=2.5)
x
x[5]
x[4:10]
l <- length(x)
x[l]
x[length(x)-1]

x[-2]

idx <- c(1,4,6)
x[idx]
x[-idx]
x>20

(x<=10) | (x>30)

x[x<10|x>30]

x2 <- numeric(length(x))
x2
x2[x<=30] <- 1
x2
x2[(x>30)&(x<70)] <- 2
x2[x>70] <- 3
x2

#alternative
library(car)
x2 <- recode(x,"0:30=1; 30:70=2; else=3")
x2

summary(x)
sum(x)
consum(x)
rec(x)
sort(x,decreasing = T)
sample(x,10)

#data frame
test <- dara.frame(A0c(1,2,3),B=c("plot1","plot2","plot3"))
test
test[,1]
test[,"A"]
test$A

#Testcomment



