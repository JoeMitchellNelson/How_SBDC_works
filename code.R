require(pacman)
p_load(tidyverse,ggplot2)

offer_levels <- c(5000,10000,25000,75000)

normaldf <- data.frame(x=rnorm(10000,mean=50000,sd=20000),
                       offer = sample(offer_levels,10000,replace=T))
normaldf$accept = ifelse(normaldf$x < normaldf$offer,1,0)
normaldf2 <- normaldf %>% group_by(offer) %>% summarise(propyes=mean(accept))


ggplot(normaldf,aes(x=x)) + geom_density()

ggplot() +
  stat_ecdf(data=normaldf,aes(x=x),geom = "step")

ggplot(normaldf2,aes(x=offer_levels)) + 
  geom_point(aes(y=propyes)) +
  scale_x_continuous(breaks=c(0,offer_levels)) +
  scale_y_continuous(breaks=round(normaldf2$propyes,2)) +
  stat_ecdf(data=normaldf,aes(x=x),geom = "step")

                       
n = 200


df <- data.frame(CBO_name=paste0("Fake_CBO_",1:n),
                 CBO_needs=round(rnorm(n,mean=50000,sd=20000)),
                 offered_to_CBO=sample(offer_levels,n,replace=T))

df$CBO_accepts <- ifelse(df$CBO_needs < df$offered_to_CBO,"Yes","No")

df2 <- df %>% group_by(offered_to_CBO) %>% 
  summarise(propyes= mean(CBO_accepts=="Yes"))



ggplot(df2) +
  geom_point(aes(x=offered_to_CBO,y=propyes))



ggplot(df) +
  geom_density(aes(x=CBO_needs)) +
  geom_vline(xintercept = mean(df$CBO_needs))



logitmodel <- glm(I(CBO_accepts=="Yes") ~ offered_to_CBO, data=df,family="binomial")
summary(logitmodel)

fitfunc2 <- function (x,b0,b1) {
  1/(1+exp(-b0-b1*x))
}

fitfunc <- function (x) {fitfunc2(x,logitmodel$coefficients[["(Intercept)"]],logitmodel$coefficients[["offered_to_CBO"]])}

ggplot(data=data.frame(x=0:100000),aes(x=x)) +
  geom_function(fun = fitfunc, colour = "red") +
  geom_point(data=df2,aes(x=offered_to_CBO,y=propyes))


fitdens <- function (x) { logitmodel$coefficients[["offered_to_CBO"]]*exp(logitmodel$coefficients[["(Intercept)"]]+logitmodel$coefficients[["offered_to_CBO"]]*x)/((exp(logitmodel$coefficients[["(Intercept)"]]+logitmodel$coefficients[["offered_to_CBO"]]*x)+1)^2)}

realdens <- function (x) {dnorm(x,mean=50000,sd=20000)}

ggplot(data=data.frame(x=0:100000),aes(x=x)) +
  geom_function(fun = fitdens, colour = "red") +
  geom_function(fun = realdens, colour = "black")
  

-logitmodel$coefficients[["(Intercept)"]]/logitmodel$coefficients[["offered_to_CBO"]]
