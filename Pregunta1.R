#Poblacion
mu <- 95.3
sigma <- 5.7

curve(dnorm(x, mean= mu, sd= sigma),xlim=c(80, 120))

rnorm(1,mu,sigma)
pnorm(90,mu,sigma)

#set.seed(123)
#muestra aleatoria de tamaño 4
rnorm(4,mu, sigma)

#apartado a)
Y <- function(i)sum(rnorm(4,mu, sigma))
Y(1)
Y100000 <- sapply(1:100000,Y)
hist(Y100000)
mean(Y100000)


#apartado b)
#en teoria la media de la suma muestral
4*mu

# varianza de la suma muestral
4*sigma^2

var(Y100000)


###
hist(Y100000, freq= FALSE)
curve(dnorm(x, mean = 4*mu, sd= sqrt(4)*sigma),add= TRUE)

#apartado c)

1-pnorm(103,mu,sigma)
Y <- function(i)sum(rnorm(4,mu, sigma))

Y100000 <- sapply(1:100000,Y)
hist(Y100000)
mean(Y100000>103)

#apartado d) n= 4

xbar <- function(i)(mean(rnorm(4,mu,sigma)))
xbar100000 <- sapply(1:100000,xbar)
hist(xbar100000)

mean(xbar100000<98)
pnorm(98,mu,sigma/sqrt(4))

#apartado e)

Ssq <- function(i)(var(rnorm(100,mu,sigma)))
Ssq100000 <- sapply(1:100000,Ssq)
hist(Ssq100000)

mean(Ssq100000>32)
1-pchisq((100-1)*32/sigma^2,100-1)

hist(Ssq100000*(100-1)/sigma^2,prob=TRUE)
curve(dchisq(x,100-1), add= TRUE, col="magenta")
