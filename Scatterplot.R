df <- read.csv("GEEQ.csv")
df$Date <- as.Date(df$Date)
monthly_data <- aggregate(
  cbind(Volume = df$Volume, Close = df$Close),
  by = list(month = format(df$Date, "%Y-%m")),
  FUN = mean
)
colnames(monthly_data)[2] <- "Average_Volume" 
head(monthly_data)
monthly_data$month <- as.Date(paste0(monthly_data$"-01"),format ="%Y-%m-%d")

plot(
  monthly_data$month,
  monthly_data$Volume,
  main="Scatterplot of Average Monthly Trading Volume",
  xlab="Month Year",
  ylab="Average Trading Volume",
  pch = 19,
  col="blue",
  xaxt="n"
)
axis(1, at = monthly_data$ month, labels = format(monthly_data$month, "%b %Y"), las = 2, cex.axis =0.7)
abline(lm(monthly_data$Volume ~
            monthly_data$month), col = "red")
