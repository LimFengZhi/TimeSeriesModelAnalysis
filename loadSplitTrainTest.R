# Import Data
sales_ds <- read.csv("Sales.csv")
View(sales_ds)

## Time Plot: Sales
data_frame <- ts(sales_ds[,2], frequency = 12, start = c(2015,1))
plot(data_frame, ylab="Sales", xlab="Year", main="Monthly Sales Data (2015 - Onwards)")

# 4 years train, 2 years test (YEAR-BASED split)
train <- window(data_frame, end = c(2018, 12))
test  <- window(data_frame, start = c(2019, 1))
h <- length(test)