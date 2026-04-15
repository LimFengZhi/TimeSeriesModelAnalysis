# Import Data
sales_ds <- read.csv("Sales.csv")
View(sales_ds)

## Time Plot: Sales
data_frame <- ts(sales_ds[,2], frequency = 12, start = c(2015,1))
plot(data_frame, ylab="Sales", xlab="Year", main="Monthly Sales Data (2015 - Onwards)")

# Split Train Test 70%
# Train/Test Split
train <- head(data_frame, round(length(data_frame) * 0.70))
h <- length(data_frame) - length(train)
test <- tail(data_frame, h)

train <- ts(train, frequency = 12, start = start(data_frame))
test  <- ts(test, frequency = 12, start = c(2018, 7))