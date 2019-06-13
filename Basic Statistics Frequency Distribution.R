attach(mtcars)
str(mtcars)
x = mtcars$mpg
max_min=range(x)

breaks.x = seq(max_min[1]
             , max_min[2]
             , length=7)
delta.x<-diff(breaks.x)

x.cut = cut(x, breaks.x, right=FALSE)

x.freq = table(x.cut)

## Frequency Distribution
cbind(x.freq)

## Relative Frequency Distribution
x.Rel.Freq<-cbind(x.freq)/sum(x.freq)

## Relative Frequency Density
x.Rel.Freq.density<-x.Rel.Freq/delta.x

barplot(x.Rel.Freq.density,beside = TRUE,col = "red")
hist(x,nclass = 7,probability = T)

## Cumulative Frequency Distribution
x.cumfreq=cumsum(x.freq)

## Cumulative Relative Frequency Distribution
cum.rel.freq<-cbind(x.cumfreq)/sum(x.freq)

cbind(x.freq,x.cumfreq,x.Rel.Freq,cum.rel.freq)

cumfreq0=c(0,x.cumfreq)
plot(breaks.x,cumfreq0
     ,xlab="mpg"
     ,ylab="Cumulative Frequency")
lines(breaks.x,cumfreq0)
grid(col="red")

###################################

y = mtcars$hp
max_min=range(y)

breaks.y = seq(max_min[1]
               , max_min[2]
               , length=7)
delta.y<-diff(breaks.y)

y.cut = cut(y, breaks.y, right=FALSE)

y.freq = table(y.cut)

hist(y,probability = T)

z <-table(x.cut,y.cut)

## plot as 3D histogram
library(plot3D)
hist3D(z=z,border="black")

## plot as 2D heat map
image2D(z=z,border="black")
