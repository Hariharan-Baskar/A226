library(readr)
data_sheet <-read.csv("GEEQ.csv")

date <- as.Date(data_sheet$Date)
data_sheet$Date <-date


monthly_volume <- aggregate(cbind(monthly_AVG_volume = data_sheet$Volume),
                           list(month = format(data_sheet$Date,"%Y-%m")),
                            FUN = mean
)

histogram <- hist(
  monthly_volume$monthly_AVG_volume,
  xlab = "Average Volume" ,
  ylab = "Frequency (Volume Ranges per Month) ",
  main = "Histogram of Average monthly trading volume",
  breaks = "FD"
  )

axis(1, at = pretty(monthly_volume$monthly_AVG_volume, n=20))


x <- seq(min(monthly_volume$monthly_AVG_volume),
        max(monthly_volume$monthly_AVG_volume), 
        length = 100)
y <- dnorm(x,
           mean = mean(monthly_volume$monthly_AVG_volume),
           sd = sd(monthly_volume$monthly_AVG_volume)
           )*length(monthly_volume$monthly_AVG_volume)

y <- y * diff(histogram$mids[1:2])
lines(x,y,col = "red")

