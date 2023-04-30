#1
mean <- 200
sd <- sqrt(256)
a <- pnorm(224, mean=mean, sd=sd, lower.tail=FALSE)
a

b <- pnorm(224, mean=mean, sd=sd) - pnorm(186, mean=mean, sd=sd)
b

c <- qnorm(0.25, mean=mean, sd=sd)
c

p_greaterthan_210 <- pnorm(210, mean=mean, sd=sd, lower.tail=FALSE)
p_lessthan_240_and_greaterthan_210 <- pnorm(240, mean=mean, sd=sd) - pnorm(210, mean=mean, sd=sd)
prob <- p_lessthan_240_and_greaterthan_210/p_greaterthan_210
prob

Q1 <- qnorm(0.25, mean = mean, sd = sd)

Q3 <- qnorm(0.75, mean = mean, sd = sd)
interquartile_range <- Q3 - Q1
interquartile_range

p_greaterthan_210 <- pnorm(210, mean=mean, sd=sd, lower.tail = FALSE)
p_lessthan_220_and_greaterthan_210 <- pnorm(220, mean=mean, sd=sd)-pnorm(210, mean=mean, sd=sd)
prob <- p_lessthan_220_and_greaterthan_210/p_greaterthan_210
prob

p_greaterthan_200 <- pnorm(200, mean = mean, sd = sd, lower.tail = FALSE)
p_greaterthan_220 <- pnorm(220, mean = mean, sd = sd, lower.tail = FALSE)
prob <- (p_greaterthan_220)/(p_greaterthan_200) 
prob

#prob for a: 6.68%
#prob for b: 74.24%
#c: 189.21V
#prob for d by Bayes theorem: 97.66%
#IQR: 21.58
#prob for f by Bayes theorem: 60.28%
#prob for g: 21.13%

#2
bounds <- qnorm(0.95, mean=25, sd=12)
bounds

bounds <- qnorm(0.1, mean = 25, sd = 12, lower.tail = FALSE)
bounds

#bounds for 95% & below: 44.74
#bounds for 10% & above: 40.38

