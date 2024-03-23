# Install and load necessary packages
if (!require("pacman")) install.packages("pacman")
pacman::p_load(fivethirtyeight, tidyverse, caret, rpart, rpart.plot)

# Exploratory Data Analysis (EDA)
candy_data_url <- "https://raw.githubusercontent.com/fivethirtyeight/data/master/candy-power-ranking/candy-data.csv"

# Load the dataset
candy_data <- read_csv(candy_data_url)
glimpse(candy_data)
summary(candy_data)

# Visualize the distribution of win percentage
ggplot(candy_data, aes(x=winpercent)) + geom_histogram(bins=30, fill="skyblue", color="black") +
  labs(title="Distribution of Candy Win Percentage", x="Win Percentage", y="Frequency") +
  theme_minimal()

# Relationship between Chocolate and Win Percentage
ggplot(candy_data, aes(x=factor(chocolate), y=winpercent)) + geom_boxplot(aes(fill=factor(chocolate))) +
  scale_fill_manual(values = c("0"="pink", "1"="brown")) +
  labs(title="Impact of Chocolate on Win Percentage", x="Chocolate", y="Win Percentage") +
  theme_minimal()

# Splitting data into training and testing sets
set.seed(123)
training_rows <- createDataPartition(candy_data$winpercent, p=.75, list=FALSE)
training_data <- candy_data[training_rows, ]
testing_data <- candy_data[-training_rows, ]

# Building a predictive model
model_control <- trainControl(method="cv", number=10)
candy_model <- train(winpercent ~ chocolate + fruity + caramel + peanutyalmondy + nougat + crispedricewafer + hard + bar + pluribus + sugarpercent, data=training_data, method="rpart", trControl=model_control)

# Model Summary
print(candy_model)


# Visualize the Decision Tree for a regression model
rpart.plot(candy_model$finalModel, type=4, extra=1)


# Predictions
predictions <- predict(candy_model, newdata=testing_data)
result <- postResample(pred = predictions, obs = testing_data$winpercent)
cat("R-Squared:", result[1], "\nRMSE:", result[2], "\n")


