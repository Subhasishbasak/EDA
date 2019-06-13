data<-read.csv(file="http://www.football-data.co.uk/mmz4281/1718/E0.csv")

FTHG<-data$FTHG

Freq<-table(FTHG)

mean(FTHG)
var(FTHG)

## Expected Frequency

Expected_freq<-dpois(0:5,lambda = mean(FTHG))*length(FTHG)

cbind.data.frame(Freq[1:6],Expected_freq)
