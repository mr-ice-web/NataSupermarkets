install.packages("tidyverse")
install.packages("corrplot")
install.packages("factoextra")
install.packages("gpairs")
install.packages("summarytools")
install.packages("psych")
install.packages("coefplot")
library(factoextra)
library(tidyverse)
library(corrplot)
library(coefplot)
library(ggplot2)
library(dplyr)
library(summarytools)
library(psych)
library(gpairs)

data <- read_csv(file.choose()) #storing the data as a tibble for easier visualization and printing
glimpse(data)
data %>% head(50) %>% print(n = 50, width = Inf) #Grabs the first 50 elements of data and prints it out on the screen in a readable format
data <- data %>% rename(
  CustomerID = ID,
  Year = Year_Birth,
  DateJoined = Dt_Customer,
  Camp_one = AcceptedCmp1,
  Camp_two = AcceptedCmp2,
  Camp_three = AcceptedCmp3,
  Camp_four = AcceptedCmp4,
  Camp_five = AcceptedCmp5,
  Camp_six = Response,
) #Renaming some columns meaningfully and based on R's naming rules

summary(data) #getting a statistical summary of the data for observing errors
colSums(is.na(data)) #combing through all columns to count the number of NA values
data <- data %>% drop_na(Income) #removing all rows with NA values in the Income column
colSums(is.na(data)) #checking if all rows with NA values have been removed
sapply(data, class) #looping through all columns to identify their class
unique(data$Education) #checking all the unique values in the character categorical column to spot a hierarchy, error values, and typos
unique(data$Marital_Status) #checking all the unique values in the character categorical column to spot a hierarchy, error values, and typos
data <- data %>%
  mutate(Marital_Status = case_when(
    Marital_Status %in% c("Alone", "Divorced", "Widow") ~ "Single", Marital_Status %in% c("Married", "Together") ~ "Together",
    TRUE ~ Marital_Status
  )) %>%
  filter(!Marital_Status %in% c("YOLO", "Absurd")) #This block of code transforms the Marital_Status column into two simple categories -- Single or Together. It then removes inapplicable values
unique(data$Marital_Status)
data <- data %>% mutate(Marital_Status = case_when(Marital_Status == "Single" ~ 0, Marital_Status == "Together" ~ 1)) #Because we simplified the Marital status into 2 categories, I encoded them into binary values which will later be converted into factors
unique(data$Marital_Status)
data <- data %>% mutate(
  Education = factor(Education, levels = c("Basic", "2n Cycle", "Graduation", "Master", "PhD"), ordered = TRUE),
  Marital_Status = factor(Marital_Status, levels = c(0,1), labels = c("Single", "Together")),
  Camp_one = factor(Camp_one, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Camp_two = factor(Camp_two, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Camp_three = factor(Camp_three, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Camp_four = factor(Camp_four, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Camp_five = factor(Camp_five, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Camp_six = factor(Camp_six, levels = c(0,1), labels = c("Unaccepted", "Accepted")),
  Complain = factor(Complain, levels = c(0,1), labels = c("No", "Yes"))
)
summary(data$Education)
#Here, I've convereted all categorical columns into factors because they had only binary values or a hierarchy like Education
sapply(data, class)
data <- data %>% select(-Z_CostContact, -Z_Revenue) #Removing unwanted columns
data <- data %>% mutate(Age = 2025 - Year) %>% filter(Age>18 & Age<90)
numeric_data <- data %>%
  select_if(is.numeric) %>%
  select(-CustomerID, -Year, -Kidhome, -Teenhome) #Here we are creating a new object called numeric_data to store all the numeric columns minus any unwanted columns
scaled_data <- numeric_data %>%
  mutate(across(everything(), scale)) #Next, we scale the numeric columns so that the results are comparable and not skewed
long_data <- scaled_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") #converted the wide-format table to long format for easier plotting

#Next, we will visualize boxplots to identify outliers. Using ggplot to assign x and y values
#geom_boxplot for decoration
#theme_minimal removes background distractions and makes the boxplot look better
#theme is used to rotate x value text
#Finally, I've used labs() function to add the boxplot title and axis titles
ggplot(long_data, aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Boxplots of Scaled Numerical Variables",
    x = "Variables",
    y = "Standardized Value"
  )

mean_income <- mean(data$Income, na.rm = TRUE) #storing the mean of income in mean_income
sd_income <- sd(data$Income, na.rm = TRUE) #storing the standard deviation of income in sd_income

#This code checks if the income is more or less than 3 standard deviations and then delets that row.
#Essentially, we're deleting all rows that are outside 99.7% of the data set -- extreme outliers
data_clean <- data %>%
  filter(Income > (mean_income - 3 * sd_income) & Income < (mean_income + 3 * sd_income))

#Calculating total spends 
data_clean <- data_clean %>%
  mutate(Total_spends = MntWines + MntFruits + MntMeatProducts + MntFishProducts + MntSweetProducts + MntGoldProds)

#Calculating total spends 
data_clean <- data_clean %>%
  mutate(MeatFishSpend = MntMeatProducts + MntFishProducts)

#ChildrenHome 
data_clean <- data_clean %>%
  mutate(ChildrenHome = Kidhome + Teenhome)


#Formating the date column
data_clean$DateJoined <- as.Date(data_clean$DateJoined, format = "%d-%m-%Y")

#Repeating the same process as above on the cleaned data
cleaned_numeric_data <- data_clean %>%
  select_if(is.numeric) %>%
  select(-CustomerID, -Year, -Kidhome, -Teenhome) #Here we are creating a new object called cleaned_numeric_data to store all the numeric columns minus any unwanted columns
rescaled_data <- cleaned_numeric_data %>%
  mutate(across(everything(), scale)) #Next, we scale the numeric columns so that the results are comparable and not skewed
clean_long_data <- rescaled_data %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") #converted the wide-format table to long format for easier plotting
ggplot(clean_long_data, aes(x = Variable, y = Value)) +
  geom_boxplot(outlier.color = "red", fill = "lightblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(
    title = "Boxplots of Scaled Numerical Variables",
    x = "Variables",
    y = "Standardized Value"
  )
data_clean <- data_clean %>% select(-Year)
summary(data_clean)
dim(data_clean) #checking the dimensions of cleaned data to see data loss


#K-means clustering begins for customer segmentation and profiling

wss <- vector() #Creating an object called wss to store Within Cluster Sum of Squares. This is an empty vector

#Running a for loop to check wss for different K values from 1 to 10
for(k in 1:10)
{
  set.seed(42) #This expression sets a random start point for the kmeans function
  clustering <- kmeans(rescaled_data, centers = k, nstart = 25)
  wss[k] <- clustering$tot.withinss #Storing the k values in the wss vector
}

#Plotting the elbow graph to find inflection point
plot(1:10, wss, type = "b", pch = 19, frame = FALSE, xlab = "Number of k Clusters",
     ylab = "Total Within-Cluster Sum of Squares (WSS)", main = "Elbow Method for choosing k")

#Based on elbow graph running kmeans clustering for k = 3, k = 4, & k = 5 to find the best fit
set.seed(42)
km3 <- kmeans(rescaled_data, centers = 3, nstart = 25)

set.seed(42)
km4 <- kmeans(rescaled_data, centers = 4, nstart = 25)

set.seed(42)
km5 <- kmeans(rescaled_data, centers = 5, nstart = 25)

#Comparing different wss values
km3$tot.withinss
km4$tot.withinss
km5$tot.withinss

#Installing the factoextra package to plot different clusters for better visual representation

fviz_cluster(km3, data = rescaled_data)
fviz_cluster(km4, data = rescaled_data)
fviz_cluster(km5, data = rescaled_data)

#Selecting k = 3 as final k value and labeling based on clusters
data_clean$cluster = km3$cluster

#Grouping all values based on clusters, then summarising all numeric values, and finally printing the summary
print(data_clean %>%
        select(-CustomerID) %>% group_by(cluster) %>%
        summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

#Before we make scatterplots, we need to first convert all columns to numeric
#Then, we need to convert the tibble to a data frame
#This is done because gpairs() does not plot tibbles or non-numeric data
data_for_plot <- data_clean %>%
  select(-Education) %>%                  
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
  select_if(is.numeric) %>% select(Income, Age, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds)

df_for_plot <- as.data.frame(data_for_plot) #data frame only for the plot

gpairs(df_for_plot)

#Grouping customers by the campaigns they accepted to find correlation

print(data_clean %>%
        group_by(Camp_one) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

print(data_clean %>%
        group_by(Camp_two) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

print(data_clean %>%
        group_by(Camp_three) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

print(data_clean %>%
        group_by(Camp_four) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

print(data_clean %>%
        group_by(Camp_five) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

print(data_clean %>%
        group_by(Camp_six) %>% summarise(across(where(is.numeric), list(mean = mean, sd = sd), na.rm = TRUE)), width = Inf)

#Finding more correlation between Income and other spending variables
cor_mat <- cor(data_for_plot)
cor_mat
#Plotting the correlation
corrplot(cor_mat, method = "circle", type = "upper", tl.cex = 0.8)

#creating a new dataframe 
total_product_spends <- data_clean %>% summarise(     
  Total_Wine_Spend   = sum(MntWines, na.rm = TRUE),
  Total_Fruits_Spend = sum(MntFruits, na.rm = TRUE),
  Total_Meat_Spend   = sum(MntMeatProducts, na.rm = TRUE),
  Total_Fish_Spend   = sum(MntFishProducts, na.rm = TRUE),
  Total_Sweet_Spend  = sum(MntSweetProducts, na.rm = TRUE),
  Total_Gold_Spend   = sum(MntGoldProds, na.rm = TRUE)
)

total_product_spends_long <- total_product_spends %>%    #converting to long form for easy plotting
  pivot_longer(cols = everything(),
               names_to = "Product_Category",
               values_to = "Total_Spend")
# Total Spend Across Product Categories - Barplot

ggplot(total_product_spends_long, aes(x = reorder(Product_Category, Total_Spend), y = Total_Spend)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(title = "Total Spend by Product Category",
       x = "Product Category",
       y = "Total Spend Amount") +
  coord_flip() +
  theme_minimal()

#Converting categorical values like "Accepted" / "Unaccepted" to 1 and 0

data_clean$Camp_one_binary <- ifelse(data_clean$Camp_one == "Accepted", 1, 0)
data_clean$Camp_two_binary <- ifelse(data_clean$Camp_two == "Accepted", 1, 0)
data_clean$Camp_three_binary <- ifelse(data_clean$Camp_three == "Accepted", 1, 0)
data_clean$Camp_four_binary <- ifelse(data_clean$Camp_four == "Accepted", 1, 0)
data_clean$Camp_five_binary <- ifelse(data_clean$Camp_five == "Accepted", 1, 0)


# Understanding trends with Total spend
#1. Income vs Total Spend - Scatter plot with regression line

par(mfrow = c(1, 1))
plot(x = data_clean$Income, 
     y = data_clean$Total_spends, 
     main = "Association between Income and Total Spend",
     xlab = "Income", 
     ylab = "Total Spend",
     pch = 1,
     col = "blue")
abline(lm(Total_spends ~ Income, data = data_clean), col = "red", lwd = 2)

#2. Kid at home vs Total Spend - Boxplot
boxplot(Total_spends ~ Kidhome, 
        data = data_clean,
        main = "Association between Number of Kids and Total Spend",
        xlab = "Number of Kids at Home", 
        ylab = "Total Spend",
        col = "lightblue",
        horizontal=TRUE)

#3. Teen at home vs Total spend - Boxplot
boxplot(Total_spends ~ Teenhome, 
        data = data_clean,
        main = "Association between Number of Teenagers and Total Spend",
        xlab = "Number of Teenagers at Home", 
        ylab = "Total Spend",
        col = "lightgreen",
        horizontal=TRUE)

#4. Marital Status vs Total Spend
boxplot(Total_spends ~ Marital_Status, 
        data = data_clean,
        main = "Association between Marital Status and Total Spend",
        xlab = "Marital Status", 
        ylab = "Total Spend",
        col = "lightcoral",
        horizontal=TRUE)  

#5. Education level vs Total Spend - Jitter Plot
stripchart(Total_spends ~ Education, 
           data = data_clean,
           method = "jitter", 
           jitter = 0.2,
           pch = 1,
           col = "purple",
           main = "Association between Education Level and Total Spend (Scatter)",
           xlab = "Education Level",
           ylab = "Total Spend",
           vertical = TRUE)

# Campaign Performance Analysis - JItter Plot

par(mfrow = c(2, 3))

#Campaign performance 1
stripchart(data_clean$Total_spends ~ data_clean$Camp_one_binary,
           method = "jitter",
           pch = 1,
           col = "blue",
           vertical = TRUE,
           main = "Campaign 1",
           xlab = "Accepted (0/1)", ylab = "Total Spend")

#Campaign performance 2
stripchart(data_clean$Total_spends ~ data_clean$Camp_two_binary,
           method = "jitter",
           pch = 1,
           col = "red",
           vertical = TRUE,
           main = "Campaign 2",
           xlab = "Accepted (0/1)", ylab = "Total Spend")

#Campaign Performance 3
stripchart(data_clean$Total_spends ~ data_clean$Camp_three_binary,
           method = "jitter",
           pch = 1,
           col = "lightblue",
           vertical = TRUE,
           main = "Campaign 3",
           xlab = "Accepted (0/1)", ylab = "Total Spend")

#Campaign perfromace 4
stripchart(data_clean$Total_spends ~ data_clean$Camp_four_binary,
           method = "jitter",
           pch = 1,
           col = "pink",
           vertical = TRUE,
           main = "Campaign 4",
           xlab = "Accepted (0/1)", ylab = "Total Spend")

#Campaign performace 5
stripchart(data_clean$Total_spends ~ data_clean$Camp_five_binary,
           method = "jitter",
           pch = 1,
           col = "darkblue",
           vertical = TRUE,
           main = "Campaign 5",
           xlab = "Accepted (0/1)", ylab = "Total Spend")

# Campaign vs Spending Behavious - Correlations Heatmap

camp_vars <- data_clean[, c("Camp_one_binary", "Camp_two_binary", "Camp_three_binary", 
                            "Camp_four_binary", "Camp_five_binary",
                            "MntWines", "MntFruits", "MntMeatProducts", 
                            "MntFishProducts", "MntSweetProducts", "MntGoldProds")]

cor_matrix <- cor(camp_vars, use = "complete.obs")

corrplot(cor_matrix, 
         method = "ellipse", 
         tl.col = "black", 
         tl.cex = 0.8,
         cl.cex = 0.8,
         title = "Correlations between Campaign effectiveness and Spending Behavious",
         mar = c(0,0,2,0))


# Correlation between Campaigns and Engagement channels

campaign_engage_vars <- data_clean[, c("Camp_one_binary", "Camp_two_binary", 
                                       "Camp_three_binary", "Camp_four_binary", "Camp_five_binary",
                                       "NumWebVisitsMonth", "NumWebPurchases", 
                                       "NumStorePurchases", "NumCatalogPurchases", 
                                       "NumDealsPurchases")]      #creating new engagement dataframe

campaign_engage_cor <- cor(campaign_engage_vars, use = "complete.obs")

corrplot(campaign_engage_cor,
         method = "ellipse",
         title = "Correlation between Campaigns vs Engagement Channels",
         tl.col = "black", tl.cex = 0.8,
         cl.cex = 0.8, mar = c(0, 0, 2, 0))

#Scatter Plot Matrix: Age, Income, Education, Total Spend, Marital Status

data_clean$Education_num <- as.numeric(as.factor(data_clean$Education))
data_clean$Marital_num <- as.numeric(as.factor(data_clean$Marital_Status))    # Converting factors to numeric code


age_ed_spend_vars <- data_clean[, c("Age", "Income", "Total_spends", "Education_num", "Marital_num")]   #to create new dataframe of selected variables

age_ed_spend_cor <- cor(age_ed_spend_vars, use = "complete.obs")

#(basic scatterplot matrix)
pairs(age_ed_spend_vars,
      main = "Scatterplot Matrix: Age, Income, Education, Marital Status, Total Spend",
      pch = 1,
      col = "black")

#(scatterplot matrix with regression lines for better understanding of significance)
pairs.panels(age_ed_spend_vars,
             method = "pearson",        # correlation type
             hist.col = "lightblue",    # histogram fill
             density = TRUE,            # show density curves
             ellipses = TRUE,           # confidence ellipses
             lm = TRUE,                 # <--- THIS adds regression lines!
             main = "Scatterplot Matrix with Regression Lines")

# Heatmap : Age, Income, Total Spend and Engagement Channels
engage_age_income_vars <- data_clean[, c("Age", "Income", "Total_spends",
                                         "NumWebVisitsMonth", "NumWebPurchases", 
                                         "NumStorePurchases", "NumCatalogPurchases", 
                                         "NumDealsPurchases")]

engage_age_income_cor <- cor(engage_age_income_vars, use = "complete.obs")

corrplot(engage_age_income_cor,
         method = "ellipse",          # Ellipse shape for correlation
         type = "lower",              # Show only lower triangle
         title = "Correlation: Age, Income, Engagement vs Total Spend",
         tl.col = "black",            # Axis label color
         tl.cex = 0.8,                # Axis label size
         cl.cex = 0.8,                # Legend label size
         mar = c(0, 0, 2, 0))

#Association between age and product preferences

data_clean$Age_Group <- cut(data_clean$Age,
                            breaks = c(18, 30, 40, 50, 60, 100),
                            labels = c("18–30", "31–40", "41–50", "51–60", "60+"))    #to group ages 

data_clean$AgeGroup_num <- as.numeric(data_clean$Age_Group)   #converting age group(factor) to numeric codes

par(mfrow = c(2,3))

boxplot(MntWines ~ AgeGroup_num, data = data_clean,
        main = "Wines Spend by Age", 
        xlab = "Age Group", ylab = "Amount Spent on Wines",
        col = "lightblue", las = 2)
cor.test(data_clean$AgeGroup_num, data_clean$MntWines, use = "complete.obs")

boxplot(MntMeatProducts ~ AgeGroup_num, data = data_clean,
        main = "Meat Spend by Age Group", 
        xlab = "Age Group", ylab = "Amount Spent on Meat Products",
        col = "lightgreen", las = 1)
cor.test(data_clean$AgeGroup_num, data_clean$MntMeatProducts, use = "complete.obs")

boxplot(MntFishProducts ~ AgeGroup_num, data = data_clean,
        main = "Fish Spend by Age", 
        xlab = "Age Group", ylab = "Amount Spent on Fish Products",
        col = "lightcoral", las = 2)
cor.test(data_clean$AgeGroup_num, data_clean$MntFishProducts, use = "complete.obs")

boxplot(MntSweetProducts ~ AgeGroup_num, data = data_clean,
        main = "Sweets Spend by Age Group", 
        xlab = "Age Group", ylab = "Amount Spent on Sweets",
        col = "lightpink", las = 1)
cor.test(data_clean$AgeGroup_num, data_clean$MntSweetProducts, use = "complete.obs")

boxplot(MntGoldProds ~ AgeGroup_num, data = data_clean,
        main = "Gold Spend by Age", 
        xlab = "Age Group", ylab = "Amount Spent on Gold Products",
        col = "gold", las = 2)
cor.test(data_clean$AgeGroup_num, data_clean$MntGoldProds, use = "complete.obs")


#Data preperation for linear regression  
numeric_cols <- sapply(rescaled_data, is.numeric)
cor(rescaled_data[, numeric_cols]) %>% round(2)

str(data_clean)
numerical_independent_vars <- rescaled_data %>%
  select(Income, Age, Recency, NumDealsPurchases, NumWebPurchases, 
         NumCatalogPurchases, NumStorePurchases, NumWebVisitsMonth)  # Including Kidhome, Teenhome as they are numeric
cor_matrix <- cor(numerical_independent_vars, use = "pairwise.complete.obs")
print("Correlation Matrix:")
print(round(cor_matrix, 2))

data_for_plot <- rescaled_data %>%
  mutate(across(where(is.factor), ~ as.numeric(as.factor(.)))) %>%
  select_if(is.numeric) %>% select(Income, Age, MntWines, MntFruits, MntMeatProducts, MntFishProducts, MntSweetProducts, MntGoldProds, NumDealsPurchases, NumWebPurchases, 
                                   NumCatalogPurchases, NumStorePurchases, NumWebVisitsMonth)

df_for_plot <- as.data.frame(data_for_plot) #data frame only for the plot
gpairs(df_for_plot)

#Linear Regression for TotalSpends

model_TotalSpends <- lm(Total_spends ~ Income + NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = rescaled_data)
summary(model_TotalSpends)
coefplot(model_TotalSpends, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on TotalSpending Spending")

coef(model_TotalSpends)*c(1, 500, 5, 5, 5)


#Linear Regression on Wine products 
model_Wine1 <- lm(MntWines ~ Income + MntMeatProducts + NumWebPurchases + NumCatalogPurchases + NumStorePurchases, data = rescaled_data)
summary(model_Wine1)
coefplot(model_Wine1)

#Predictions 
coef(model_Wine1)*c(1, 500, 50, 5, 5, 5)

#Linear Regression for Fruits
model_Fruits <- lm(MntFruits ~ Income + MntMeatProducts + MntFishProducts + MntSweetProducts + NumStorePurchases, data = rescaled_data)
summary(model_Fruits)
coefplot(model_Fruits, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on Fruits Spending")

coef(model_Fruits)*c(1, 500, 50, 50, 50, 5)

#Linear Regression for Meat
model_Meat <- lm(MntMeatProducts ~ Income + MntWines + MntFruits + MntFishProducts + MntSweetProducts + NumCatalogPurchases, data = rescaled_data)
summary(model_Meat)
coefplot(model_Meat, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on Meat Spending")

coef(model_Meat)*c(1, 500, 50, 50, 50, 5, 5)

#Linear Regression for Fish

model_Fish <- lm(MntFishProducts ~ Income + MntFruits + MntMeatProducts + MntSweetProducts + MntGoldProds + NumCatalogPurchases + NumStorePurchases, data = rescaled_data)
summary(model_Fish)
coefplot(model_Fish, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on Fish Spending")

#Linear Regression for Gold

model_Gold <- lm(MntGoldProds ~ Income + MntFishProducts + NumWebPurchases + NumCatalogPurchases, data = rescaled_data)
summary(model_Gold)
library(coefplot)
coefplot(model_Gold, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on Gold Spending")

#Linear Regression for Sweets

model_Sweets <- lm(MntSweetProducts ~ Income + MntFruits + MntMeatProducts + MntFishProducts + NumCatalogPurchases + NumStorePurchases, data = rescaled_data)
summary(model_Sweets)
library(coefplot)
coefplot(model_Sweets, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Effect on Sweets Spending")

# Rescaling variables

data_clean$Total_spends_scaled <- scale(data_clean$Total_spends)
data_clean$Income_scaled <- scale(data_clean$Income)
data_clean$Age_scale <- scale(data_clean$Age)
data_clean$Children_scaled <- scale(data_clean$ChildrenHome)
data_clean$Recency <- scale(data_clean$Recency)

# Calculate the correlation matrix
correlation_matrix <- cor(numeric_data)

# Print the correlation matrix
print(correlation_matrix)

#Chi Square Tes for Marital_Status and Campigns 
contingency_table <- table(data_clean$Marital_Status, data_clean$Camp_three)
print(contingency_table)

chi2_test <- chisq.test(contingency_table)
print("Chi-Square Test of Independence:")
print(chi2_test)

contingency_table <- table(data_clean$Marital_Status, data_clean$Camp_four)
print(contingency_table)

chi2_test <- chisq.test(contingency_table)
print("Chi-Square Test of Independence:")
print(chi2_test)

#Chi Square Tes for Education and Campigns 
contingency_table_education <- table(data_clean$Education, data_clean$Camp_three)
print(contingency_table_education)

chi2_test_education <- chisq.test(contingency_table_education)
print("Chi-Square Test of Independence for Education and Campaign Three:")
print(chi2_test_education)


#Logistic Regression on Campaign Dump 

lmCamp23 <- glm(Camp_one ~ Income_scaled + Age_scale + Children_scaled + Total_spends_scaled + Marital_Status,
                data = data_clean,
                family = binomial)
summary(lmCamp23)
coefplot(lmCamp23, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp23)
exp(coef(lmCamp23))

#Logistic Regression on Campaign 1 

lmCamp1 <- glm(Camp_one ~ Income_scaled + Total_spends_scaled,
               data = data_clean,
               family = binomial)
summary(lmCamp1)
coefplot(lmCamp1, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp1)
exp(coef(lmCamp1))
exp(confint(lmCamp1)) 


#Logistic Regression on Campaign 2 

lmCamp2 <- glm(Camp_two ~ Total_spends_scaled,
               data = data_clean,
               family = binomial)
summary(lmCamp2)
coefplot(lmCamp2, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp2)
exp(coef(lmCamp2))

#Logistic Regression on Campaign 3 

lmCamp3 <- glm(Camp_three ~ Income_scaled + Age_scale + Total_spends_scaled,
               data = data_clean,
               family = binomial)
summary(lmCamp3)
coefplot(lmCamp3, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp3)
exp(coef(lmCamp3))

#Logistic Regression on Campaign 4 

lmCamp4 <- glm(Camp_four ~ Age_scale + Children_scaled + Total_spends_scaled,
               data = data_clean,
               family = binomial)
summary(lmCamp4)
coefplot(lmCamp4, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp4)
exp(coef(lmCamp4))

#Logistic Regression on Campaign 5

lmCamp5 <- glm(Camp_five ~ Income_scaled + Children_scaled + Marital_Status,
               data = data_clean,
               family = binomial)
summary(lmCamp5)
coefplot(lmCamp5, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp5)
exp(coef(lmCamp5))


#Logistic Regression on Campaign 6

lmCamp6 <- glm(Camp_six ~ Income_scaled + Total_spends_scaled + Marital_Status,
               data = data_clean,
               family = binomial)
summary(lmCamp6)
coefplot(lmCamp6, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(lmCamp6)
exp(coef(lmCamp6))

str(data_clean)

#Logistic Regression on Complains

Complains_model <- glm(Complain ~ Total_spends_scaled + DateJoined,
                       data = data_clean,
                       family = binomial)
summary(Complains_model)
coefplot(Complains_model, intercept=FALSE, outerCI=1.96, lwdOuter=1.5,
         ylab="Predictor", xlab="Campaign Effect")
coef(Complains_model)
exp(coef(Complains_model))
