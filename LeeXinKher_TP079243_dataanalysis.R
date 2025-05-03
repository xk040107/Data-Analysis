# LEE XIN KHER TP079243
# To investigate the relationship between OS and the amount of financial loss caused by hacking. 
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(Metrics)
library(randomForest)
library(caret)
library(caTools)

fileURL = "C:\\Users\\Xin Kher\\Documents\\apu\\data analysis (r)\\assignment\\cleaned_hackingData.csv"


df = read.csv(fileURL)
df

plot_missing(df)
View(df)
as.data.frame(sort(table(df$OS, useNA = "ifany")))

#1
# Violin + boxplot
ggplot(df, aes(x = OS, y = Loss, fill = OS))+
  geom_violin(trim = FALSE, alpha = 0.7)+
  geom_boxplot(width = 0.1) +
  theme_minimal()+
  labs(title = "Financial Loss Density by OS",
       x = "Operating System",
       y = "Financial Loss(USD)")

# Calculate Coefficient of Variation (CV) for each OS group
cv_results <- df %>%
  group_by(OS) %>%
  summarise(
    Mean_Loss = mean(Loss, na.rm = TRUE),
    SD_Loss = sd(Loss, na.rm = TRUE),
    CV = (SD_Loss / Mean_Loss) * 100
  )

# Print results
print(cv_results)

ggplot(df, aes(x=OS, y=Loss, color=OS)) +
  geom_jitter(width=0.2, alpha=0.5) +  # Jitter to avoid overlapping points
  theme_minimal() +
  labs(title="Dispersion of Financial Loss Across OS",
       x="Operating System", y="Financial Loss (USD)")

# 2
# ANOVA
df$OS <- factor(df$OS)
df$Loss <- as.numeric(df$Loss)

# PERFORM ONE-WAY ANOVA
anova_result <- aov(Loss ~ OS, data = df)
summary(anova_result)

# Create Density Plot (KDE)
ggplot(df, aes(x=Loss, color=OS, fill=OS)) +
  geom_density(alpha = 0.4) +  # KDE with transparency
  theme_minimal() +
  labs(title = "Kernel Density Estimation (KDE) of Financial Loss by OS",
       x = "Financial Loss (USD)", 
       y = "Density") +
  theme(legend.position = "right")

ggplot(df, aes(x=Loss, fill=OS)) +
  geom_density(alpha=0.5) +  # Density plot with transparency
  scale_x_log10() +  # Log scale for better visualization (optional)
  facet_wrap(~OS) +  # Separate plots for each OS
  theme_minimal() +
  theme(legend.position="none") + 
  labs(x="Financial Loss (USD)", y="Density", 
       title="Kernel Density Estimation (KDE) of Financial Loss by OS")

ggplot(df, aes(x=OS, y=Loss, color=OS)) +
  stat_summary(fun=mean, geom="point", size=3) +
  stat_summary(fun.data=mean_cl_normal, geom="errorbar", width=0.2) +
  theme_minimal() +
  labs(title="Mean Financial Loss by OS with Confidence Intervals",
       x="Operating System",
       y="Mean Financial Loss (USD)")

tukey_result <- TukeyHSD(anova_result)
# Print results
print(tukey_result)
# Plot Tukey's HSD results
plot(tukey_result)

# All p-values > 0.05 → No significant difference between any OS pair.

# 3
# Linear Regression
set.seed(123)
split = sample.split(df$Loss, SplitRatio = 0.8)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)
dim(training_set)
# 80%
dim(test_set)
# 20%
dim(df)
model <- lm(Loss ~ OS, data = training_set)
summary(model)

lm_preds = predict(model, test_set)
lm_preds

mae <- mae(test_set$Loss, lm_preds)
mse <- mse(test_set$Loss, lm_preds)
rmse <- rmse(test_set$Loss, lm_preds)
mae
mse
rmse


ggplot(data.frame(actual = test_set$Loss, predicted = lm_preds), aes(x = actual, 
                                                                     y = predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Actual vs. Predicted Financial Loss",
       x = "Actual Loss (USD)",
       y = "Predicted Loss (USD)") +
  theme_minimal()


# Add DownTime
df$OS <- as.factor(df$OS)
# Convert Downtime to numeric if needed
df$DownTime <- as.numeric(df$DownTime)

set.seed(123)
split2 <- sample.split(df$Loss, SplitRatio = 0.8)
training_set2 <- subset(df, split2 == TRUE)
test_set2 <- subset(df, split2 == FALSE)

model2 <- lm(Loss ~ OS + DownTime, data = training_set2)
summary(model2)
lm_preds2 = predict(model2, test_set2)

mae2 <- mae(test_set2$Loss, lm_preds2)
mse2 <- mse(test_set2$Loss, lm_preds2)
rmse2 <- rmse(test_set2$Loss, lm_preds2)
mae2
mse2
rmse2

ggplot(data.frame(Actual = test_set2$Loss, Predicted = lm_preds2), aes(x = Actual, 
                                                                       y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") + # Line of perfect prediction
  labs(title = "Actual vs Predicted Loss", x = "Actual Loss", y = "Predicted Loss") +
  theme_minimal()


# Random Forest OS
set.seed(123)  # Ensure reproducibility
rf_model <- randomForest(Loss ~ OS, 
                         data = training_set, 
                         ntree = 500,  # Number of trees
                         mtry = 1,     # Since we have only one predictor (OS)
                         importance = TRUE)

print(rf_model)  # Model summary
rf_preds <- predict(rf_model, test_set)

# Evaluate performance
rf_mae <- mae(test_set$Loss, rf_preds)
rf_mse <- mse(test_set$Loss, rf_preds)
rf_rmse <- rmse(test_set$Loss, rf_preds)

rf_mae
rf_mse
rf_rmse

# Random Forest OS + Downtime and Loss
rf_model2 <- randomForest(Loss ~ OS + DownTime, data = training_set2, ntree = 500, importance = TRUE)
print(rf_model2)
rf_preds2 <- predict(rf_model2, test_set2)

# Evaluate performance
rf_mae2 <- mae(test_set2$Loss, rf_preds2)
rf_mse2 <- mse(test_set2$Loss, rf_preds2)
rf_rmse2 <- rmse(test_set2$Loss, rf_preds2)

rf_mae2
rf_mse2
rf_rmse2

# Compare Performance: Linear Regression vs. Random Forest
comparison <- data.frame(
  Model = c("Linear (Loss ~ OS)", "Linear (Loss ~ OS + DownTime)", 
            "Random Forest (Loss ~ OS)", "Random Forest (Loss ~ OS + DownTime)"),
  MAE = c(mae, mae2, rf_mae, rf_mae2),
  MSE = c(mse, mse2, rf_mse, rf_mse2),
  RMSE = c(rmse, rmse2, rf_rmse, rf_rmse2)
)

print(comparison)

importance(rf_model2)  # Get feature importance scores
varImpPlot(rf_model2)  # Plot feature importance

ggplot(data.frame(Actual = test_set2$Loss, Predicted = rf_preds2), aes(x = Actual, y = Predicted)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  labs(title = "Random Forest: Actual vs. Predicted Financial Loss",
       x = "Actual Loss (USD)",
       y = "Predicted Loss (USD)") +
  theme_minimal()

