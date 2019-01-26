###############
### SADA 1 
################
library(tidyverse)

######
# Uloha 19:

#a) pravdepodobnost ze vytihaneme 2 cervene, ak je 6B, 4M, 5C
vzorka <- c(rep(1,6), rep(2,4), rep(3,5))

# analyticky 1/9
mean(replicate(100000,identical(sample(vzorka, 2,replace = TRUE),c(3,3))))

#b) to iste, ale bez vratenia

#analyticky 2/21
mean(replicate(50000,identical(sample(vzorka, 2,replace = FALSE),c(3,3))))

#####
## 20
#####
dbinom(4,size = 10, prob = 7/100)


#####
## 23
#####
n <- 3
p <- 0.47
dbinom(0:3, size = n, prob = p)
cumsum(dbinom(0:3, size = n, prob = p))
       

#####
## 24
#####
pnorm(c(0,1.35,2.79,1.354))

#####
## 25
#####
qnorm(c(0.7703,0.9292,0.9853))

#####
## 26
#####
pnorm(c(-0.38,-1.43,-2.88))

#####
## 27
#####
qnorm(c(0.0120,0.1469))

#####
## 28
#####
pnorm(1.25, mean = 0.5,sd = 1.2)

pnorm(2.6, mean = 0.5,sd = 1.2) - pnorm(1.15, mean = 0.5,sd = 1.2)


##################
#### SADA 2 
#################

# Uloha 1 
1 - pnorm(300,mean = 250,sd = 60)

# Uloha 2
pnorm(60,mean = 60,sd = sqrt(90))
1 - pnorm(90,mean = 60,sd = sqrt(90))
pnorm(80,mean = 60,sd = sqrt(90)) - pnorm(60,mean = 60,sd = sqrt(90))

# Uloha 3
kg <- c(100, 101, 105, 99,100, 98,99,104,98,93,100,103,97,95,99,105,101,94)
sort(kg)
length(kg)
median(kg)

# Uloha 4
kg <- c(100, 101, 105, 99,100, 98,99,104,98,93,100,103,97,95,99,105,101)
sort(kg)
length(kg)
median(kg) 

# Uloha 5
x <- c(24,20,14,17,18,23,17,15,19,18,15,22)
ecdf(x)
table(x)
plot(ecdf(x))
mean(x)

var(x) * 11 / 12
sd(x)
median(x)

# Uloha 6
x <- c(3,1,8,6,4,6,9,5)
median(x)
mean(x)
var(x)
sd(x)

# Uloha 8
x <- c( rep(34.5,2),rep(44.5,3),rep(54.5,11), rep(64.5,20), rep(74.5,32), rep(84.5, 25), rep(94.5,7))
table(x)
median(x)
mean(x)
var(x)
sd(x)


# Uloha 9
x <- c(rep(2.5,2 ),rep(3.5, 4),rep(4.5, 4),rep(5.5,1 ))
mean(x)
var(x)
sd(x)
# Uloha 10
x <- c(0.253,0.253,0.253,0.254,0.252,0.251,0.248,0.250)
mean(x)
var(x)
sd(x)

#a)
t.test(x, mu = 0.25, alternative = "greater")
qt(0.95,df = 7)

#b)
t.test(x, mu = 0.25, alternative = "less")
qt(0.05,df = 7)

#b)
t.test(x, mu = 0.25)
qt(0.025,df = 7)
qt(0.975,df = 7)


# Uloha 11
x <- c(10.4, 10.3, 9.6, 10.2, 9.9, 10.4)
mean(x)
var(x)
sd(x)

t.test(x, mu = 10, alternative = "less")
qt(0.05,df = 5)

# Uloha 12 
x <- c(0.62, 0.64, 0.67, 0.63, 0.59, 0.65, 0.62, 0.59)
mean(x)
var(x)

statistika <- (length(x) - 1) * var(x) / 0.003

# Kvantily
qchisq(c(0.025, 0.05, 0.95, 0.975), df = length(x)-1)

# Uloha 13
x <- c(2.8,2.9,2.7,2.7,2.8,2.5)

mean(x)
var(x)
sd(x)

qchisq(0.05,df = 5)

# Uloha 14
x <- c(36.1,36.9,35.8,39.6,33.2,38.5,31.8)
y <- c(37.3,35.8,34.3,48.6,29.6,39.9,28.5)

d <- y-x
mean(d)
var(d)
sd(d)

t.test(d,alternative = "greater")
qt(0.95, df = 6)


# Uloga 15
x <- c(1, 0.77, 1.45, 1.77, 0.85, 0.15)
y <- c(0.8, 0.66, 1.22, 1.5, 0.65, 0.05)

d <- x-y
mean(d)
var(d)
sd(d)

t.test(d,alternative = "greater")
qt(0.95, df = 5)
 
# Uloha16 

x <- c(50,57,84,39,73,90,78,73,65)
mean(x)
var(x)
sd(x)
t.test(x, mu = 65, alternative = "less")
qt(0.05,df = 8)
#pt(0.484,df = 8)

# Uloha 17
x <- c(0.95, 1.15, 0.65, 0.56, 0.43, 0.55, 1.12, 1.23, 1.3, 0.64)
mean(x)
var(x)

statistika <- (length(x) - 1) * var(x) / 0.1
qchisq(0.95, df = length(x)-1)
