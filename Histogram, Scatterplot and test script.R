library(readr)
df <- read.csv("GEEQ.csv")

#Data wrangling code 
df$Date <- as.Date(df$Date)
monthly_data <- aggregate(
  cbind(Volume = df$Volume, Close = df$Close),
  by = list(month = format(df$Date, "%Y-%m")),
  FUN = mean
)
colnames(monthly_data)[2] <- "Average_Volume" 
head(monthly_data)
monthly_data$month <- as.Date(paste0(monthly_data$month, "-01"), format = "%Y-%m-%d")


#Histogram code
histogram <- hist(
  monthly_data$Average_Volume,
  xlab = "Average Volume of the GEEQ coin in thousands" ,
  ylab = "Frequency (Volume Ranges per Month) ",
  main = "Histogram of Average monthly trading volume",
  breaks = "FD",
  xaxt = "n"
)

#Positioning for the ticks
tick_positions <- pretty(monthly_data$Average_Volume, n = 10)  
tick_labels <- paste0(tick_positions / 1000, "K")

#Formatting the x axis 
axis(1, at = tick_positions, labels = tick_labels)
axis(1, at = seq(min(tick_positions), max(tick_positions), by = diff(tick_positions)[1] / 5),
     labels = FALSE, tck = -0.01)#Minor ticks


x <- seq(0,
         max(monthly_data$Average_Volume), 
         length = 100)
y <- dnorm(x,
           mean = mean(monthly_data$Average_Volume),
           sd = sd(monthly_data$Average_Volume)
)*length(monthly_data$Average_Volume)

y <- y * diff(histogram$mids[1:2])
lines(x,y,col = "red")


#Line graph code
plot(
  monthly_data$month,
  monthly_data$Average_Volume,
  main="Scatterplot of Average Monthly Trading Volume",
  xlab="Month and Year",
  ylab="Average Trading Volume of the GEEQ coin",
  pch = 19,
  col="blue",
  xaxt="n"
)
axis(1, at = monthly_data$ month, labels = format(monthly_data$month, "%b %Y"), las = 2, cex.axis =0.7)
abline(lm(monthly_data$Average_Volume ~
            monthly_data$month), col = "red")


# Statistical Tests to determine the normality:
# Shapiro-Wilk test
shapiro_test <- shapiro.test(monthly_data$Average_Volume)


# Anderson-Darling Test:
#install.packages('nortest')
library(nortest)
ad_test <- ad.test(monthly_data$Average_Volume)



# checking the correlation based on the normality test values

if (shapiro_test$p.value > 0.05 && ad_test$p.value > 0.05) {
  # Using Pearson if data is normally distributed
  correlation_test <- cor.test(
    as.numeric(monthly_data$month), 
    monthly_data$Average_Volume,
    method = "pearson"
  )
} else {
  # Using Spearman if data is not normally distributed
  correlation_test <- cor.test(
    as.numeric(monthly_data$month),
    monthly_data$Average_Volume,
    method = "spearman"
  )
}

# Print the results of the normality and correlation test
print(shapiro_test)
print(ad_test)
print(correlation_test)



