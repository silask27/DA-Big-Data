games <- read.csv('games.csv')
rpe <- read.csv('rpe.csv')
wellness <- read.csv('wellness.csv')
gps <- read.csv('gps.csv')
View(games)
games$Date.x <- games$Date
View(rpe)
View(merged2)
View(wellness)

merged_data <- merge(wellness, rpe, by = "PlayerID")
merged2 <- merge(merged_data, games, by = "Date.x")

hist(wellness$Fatigue)
hist(wellness$Soreness)
hist(wellness$Irritability)
hist(wellness$SleepQuality)
hist(wellness$Desire)

hist(wellness$Fatigue)
hist(wellness$Soreness)
hist(wellness$Irritability)
hist(wellness$SleepQuality)
hist(wellness$Desire)

# Define the ordinal encoding for BestOutOfMyself
ordinal_encoding <- c("Not at all" = 1, "Somewhat" = 2, "Absolutely" = 3)

# Convert BestOutOfMyself to quantitative scale using ordinal encoding
merged_data$BestOutOfMyself_Quantitative <- ordinal_encoding[merged_data$BestOutOfMyself]
rpe$BestOutOfMyself_Quantitative <- ordinal_encoding[rpe$BestOutOfMyself]

merged2$BestOutOfMyself_Quantitative <- ordinal_encoding[merged2$BestOutOfMyself]
merged2$PFI <- (merged2$BestOutOfMyself_Quantitative - merged2$Fatigue) / 
  (merged2$BestOutOfMyself_Quantitative + merged2$Fatigue)






# Create histogram for Best out of Myself
hist(rpe$BestOutOfMyself_Quantitative, 
     col = "lightblue",  # Set color of bars
     main = "Distribution of Best Out Of Myself (Quantitative)",  # Main title
     xlab = "Best Out Of Myself (Quantitative)",  # X-axis label
     ylab = "Frequency",  # Y-axis label
     border = "black"  # Border color of bars
)

# Calculate mean and median
mean_value <- mean(rpe$BestOutOfMyself_Quantitative, na.rm = T)
median_value <- median(rpe$BestOutOfMyself_Quantitative, na.rm = T)

# Add mean line (red)
abline(v = mean_value, col = "red", lwd = 2)

# Add median line (blue)
abline(v = median_value, col = "blue", lwd = 2)

# Add legend
legend("topright", 
       legend = c("Mean", "Median"), 
       col = c("red", "blue"), 
       lty = 1, 
       lwd = 2)

# Add grid
grid()

# Add a title
title(main = "Distribution of Best Out Of Myself (Quantitative)", 
      sub = "Histogram with Mean and Median Lines", 
      cex.main = 1.2, 
      cex.sub = 0.8)
summary(rpe$BestOutOfMyself_Quantitative)



# Create histogram for Fatigue
hist(wellness$Fatigue, 
     col = "lightblue",  # Set color of bars
     main = "Distribution of Fatigue",  # Main title
     xlab = "Fatigue",  # X-axis label
     ylab = "Frequency",  # Y-axis label
     border = "black"  # Border color of bars
)

# Calculate mean and median
mean_value <- mean(wellness$Fatigue, na.rm = T)
median_value <- median(wellness$Fatigue, na.rm = T)

# Add mean line (red)
abline(v = mean_value, col = "red", lwd = 2)

# Add median line (blue)
abline(v = median_value, col = "blue", lwd = 2)

# Add legend
legend("topright", 
       legend = c("Mean", "Median"), 
       col = c("red", "blue"), 
       lty = 1, 
       lwd = 2)

# Add grid
grid()

# Add a title
title(main = "Distribution of Fatigue", 
      sub = "Histogram with Mean and Median Lines", 
      cex.main = 1.2, 
      cex.sub = 0.8)
summary(wellness$Fatigue)



# Create histogram for PFI
hist(merged_data$PFI, 
     col = "lightblue",  # Set color of bars
     main = "Distribution of PFI",  # Main title
     xlab = "PFI",  # X-axis label
     ylab = "Frequency",  # Y-axis label
     border = "black"  # Border color of bars
)
# Calculate mean and median
mean_value <- mean(merged_data$PFI, na.rm = T)
median_value <- median(merged_data$PFI, na.rm = T)
# Add mean line (red)
abline(v = mean_value, col = "red", lwd = 2)=
# Add median line (blue)
abline(v = median_value, col = "blue", lwd = 2)
# Add legend
legend("topright", 
       legend = c("Mean", "Median"), 
       col = c("red", "blue"), 
       lty = 1, 
       lwd = 2)
# Add grid
grid()
# Add a title
title(main = "Distribution of PFI", 
      sub = "Histogram with Mean and Median Lines", 
      cex.main = 1.2, 
      cex.sub = 0.8)
summary(merged_data$PFI)










# View the updated data frame
View(merged_data)

# Assuming merged_data is already loaded and contains BestOutOfMyself_Quantitative and Fatigue columns

# Compute Performance-Fatigue Index (PFI)
merged_data$PFI <- (merged_data$BestOutOfMyself_Quantitative - merged_data$Fatigue) / 
  (merged_data$BestOutOfMyself_Quantitative + merged_data$Fatigue)




pfi_model <- lm(PFI ~ SleepHours + Nutrition + USG + SessionLoad + WakeTime + BedTime
                + Duration + DailyLoad + AcuteLoad + ChronicLoad + SessionType
                + Outcome + Opponent + TeamPoints + TeamPointsAllowed
                + Tournament + TournamentGame, data = merged2)
summary(pfi_model)

backward <- step(pfi_model, direction = "backward")

pfi_model_step <- lm(formula = PFI ~ Nutrition + USG + SessionLoad + WakeTime + 
                       BedTime + Duration + AcuteLoad + ChronicLoad + SessionType + 
                        + Opponent + Tournament, data = merged2)

fatigue_model <- lm(Fatigue ~ SleepHours, data = wellness)
summary(fatigue_model)
summary(pfi_model_step)
plot(wellness$SleepHours, wellness$Fatigue)

merged2$PFI_binary <- ifelse(merged2$PFI > 0, 1, 0)
merged2$PFI_binary <- as.factor(merged2$PFI_binary)

library(randomForest)
merged2 <- na.omit(merged2)
pfi_rf <- randomForest(PFI_binary ~ Nutrition + USG + SessionLoad + WakeTime + 
                         BedTime + Duration + AcuteLoad + ChronicLoad + SessionType + 
                         + Opponent + Tournament, data = merged2, family = 'binomial')
pfi_rf
varImpPlot(pfi_rf)






















