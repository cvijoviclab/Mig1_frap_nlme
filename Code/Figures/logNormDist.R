#Half time distribution

#Glucose 2 %
curve(dlnorm(x, meanlog=6.78, sdlog=0.175), from=0, to=20000, col='blue')
#Glucose0_05%
curve(dlnorm(x, meanlog=8.84, sdlog=0.598), from=0, to=20000, col='red', add=TRUE)
#Glycerol 2%
curve(dlnorm(x, meanlog=4.09, sdlog=0.167), from=0, to=20000, col='purple', add=TRUE)
#Population distribution
#Glucose 2 %
curve(dlnorm(x, meanlog=0.656, sdlog=0.0932), from=0, to=4, col='blue')
#Glucose0_05%
curve(dlnorm(x, meanlog=0.51, sdlog=0.172), from=0, to=4, col='red', add=TRUE)
#Glycerol 2%
curve(dlnorm(x, meanlog=0.508, sdlog=0.17), from=0, to=4, col='purple', add=TRUE)

library(ggplot2)
library(ggthemes)
library(stringr)
data <- data.frame(
  var1 = dlnorm(seq(1,2000,by=1), meanlog=6.78, sdlog=0.175),
  var2 = rnorm(2000, mean=2)
)
data
  ggplot( aes(x=x)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8)

y1 = dlnorm(seq(0,20000,by=1), meanlog=6.78, sdlog=0.175)
y2 = dlnorm(seq(0,20000,by=1), meanlog=8.84, sdlog=0.598)
y3 = dlnorm(seq(0,20000,by=1), meanlog=4.09, sdlog=0.167)
plot(seq(0,20000,by=1), y3)
# plot the first curve by calling plot() function
# First curve is plotted
plot(seq(0,20000,by=1), y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,0.040), ylab="y" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(seq(0,20000,by=1), y3, col="dark red",pch="+")
lines(seq(0,20000,by=1), y3, col="dark red", lty=3)
points(seq(0,20000,by=1), y2, col="red", pch="*")
lines(seq(0,20000,by=1), y2, col="red",lty=2)


y1 = dlnorm(seq(0,4,by=0.0001), meanlog=0.656, sdlog=0.0932)
y2 = dlnorm(seq(0,4,by=0.0001),  meanlog=0.51, sdlog=0.172)
y3 = dlnorm(seq(0,4,by=0.0001), meanlog=0.508, sdlog=0.17)
plot(seq(0,4,by=0.0001), y3)
# plot the first curve by calling plot() function
# First curve is plotted
plot(seq(0,4,by=0.0001), y1, type="o", col="blue", pch="o", lty=1, ylim=c(0,1.80), ylab="y" )

# Add second curve to the same plot by calling points() and lines()
# Use symbol '*' for points.
points(seq(0,4,by=0.0001), y3, col="dark red",pch="+")
lines(seq(0,4,by=0.0001), y3, col="dark red", lty=3)
points(seq(0,4,by=0.0001), y2, col="red", pch="*")
lines(seq(0,4,by=0.0001), y2, col="red",lty=2)



# Libraries
library(ggplot2)
library(hrbrthemes)
library(ggridges)

# Dummy data
data <- data.frame(
  var1 = dlnorm(seq(0,2000,by=1), meanlog=6.78, sdlog=0.175),
  var2 = dlnorm(seq(0,2000,by=1), meanlog=0.656, sdlog=0.0932)
)

p <- ggplot(data, aes(x=var1, y = ..density..) ) + geom_density()
p
# Chart
p <- ggplot(data, aes(x=x) ) +
  # Top
  geom_density( aes(x = var1, y = ..density..), fill="#69b3a2" ) +
  #geom_label( aes(x=4.5, y=0.25, label="variable1"), color="#69b3a2") +
  # Bottom
  geom_density( aes(x = var1, y = -..density..), fill= "#404080") +
  #geom_label( aes(x=4.5, y=-0.25, label="variable2"), color="#404080") +
  xlab("value of x")
p


q <- ggplot(data, aes(x= x)) +
  geom_density(alpha=0.6) +
  xlab("") +
  ylab("Assigned Probability (%)") +
  facet_wrap(~text, scale="free_y")
q
