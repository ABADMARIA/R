#1
a <- pexp(0.25, 4)
a

b <- 1-pexp(0.5, 4)
b

c <- pexp(1, 4)-pexp(0.25, 4)
c

#prob for a: 63.21%
#prob for b: 13.53%
#prob for c: 34.96%

#3
a <- (1-ppois(2, lambda = 2))
a

b <- (1 - pexp(0.5, 2))
b

c <- pexp(0.5, 2)
c

d <- pexp(1, 2)-pexp(0.5, 2)
d

#prob for a: 32.33%
#prob for b: 36.79%
#prob for c: 63.21%
#prob for d: 23.25%

#7
a <- (1 - pexp(1/6, 15))
a

b <- ppois(8, lambda = 15)
b

c <- pexp(0.5, 15) - pexp(0.25, 15)
c
#given F(x; λ) = 0.75. Find x
visits_hourly <- 15

d <- qpois(0.75, visits_hourly)
d

#prob for a: 8.21%
#prob for b: 3.74%
#prob for c: 2.30%
#d: top quartile = 18. Therefore, 75% of the time the amount of visits per hour the website receives is 18 or less.

