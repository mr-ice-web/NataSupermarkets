# Nata Supermarkets: Marketing Analytics Report

This project presents a comprehensive marketing analytics report for Nata Supermarkets, a Canadian grocery retailer. The analysis aims to provide actionable insights to address declining performance and leverage consumer data for sustainable growth in a highly competitive market.

## Project Overview

The report details a data-driven approach to understanding customer behavior and optimizing marketing strategies for Nata Supermarkets. It includes:

* **Market Analysis**: An introduction to the Canadian grocery retail environment, key market players, and evolving consumer behaviors.
* **Data Description and Preparation**: A thorough overview of the dataset containing demographic and purchasing data for 2,240 customers, along with the steps taken for data cleaning, handling missing values, outlier detection, and standardization using R.
* **Descriptive Analysis**: Exploration of overall spending distribution across product categories, the influence of income, education, marital status, and age on spending, and the relationship between total spending and engagement channels.
* **Customer Segmentation (Clustering)**: Identification and profiling of three distinct customer clusters based on their spending habits, income, education, and engagement behaviors.
* **Main Analysis (Regression Models)**:
    * **Linear Regression**: Analysis of factors driving total spending and category-level spending (wines, fruits, fish, gold, sweets).
    * **Logistic Regression**: Evaluation of consumer traits influencing campaign acceptance (Campaigns 1-6) and complaint behavior.
* **Decisions & Recommendations**: Strategic recommendations for Nata Supermarkets, focusing on a dual-segmentation strategy, targeted promotions, and optimized marketing spend.

## Problem Statement

Nata Supermarkets, founded in 1972 and operating 37 stores nationwide in Brandon, Manitoba, has experienced declining performance relative to competitors. In the intensely competitive Canadian food retail sector, characterized by low margins and high price sensitivity, market leaders leverage advanced analytics to optimize operations and loyalty programs, advantages that Nata struggles to replicate. Key challenges include poor targeting through promotions and ineffective stock management. Nata has historically failed to deeply understand its customers' preferences and forecast their needs, unlike competitors who leverage data analysis to predict customer behavior and modify offers accordingly.

## Goal

The primary goal of this project is to provide Nata Supermarkets with actionable insights derived from consumer data to inform smarter promotions, optimize inventory, and enhance the overall shopping experience, thereby positioning the company for sustainable growth in Canada's evolving grocery landscape.

## Dataset

The dataset includes demographic and purchasing data for 2,240 customers of Nata Supermarkets. It covers:
* Birth year, marital status, and income.
* Spending across various product categories (wine, meat, fish, fruits, sweets, gold products).
* Responses to six marketing campaigns.
* Engagement channels (store, web, catalogue purchases, website visits).

## Tools and Technologies

* **R**: The primary programming language used for data preparation, exploratory analysis, statistical modeling, and visualization.
* **R Packages**:
    * `tidyverse`: For data manipulation.
    * `corrplot`: For correlation visualization.
    * `factoextra`: For clustering analysis.
    * `gpairs`: For scatterplot matrix visualisations.
    * `psych`, `coefplot`, `summarytools`: Used in R Studio to handle data cleansing.
    * `ggplot2`: For producing boxplots of standardized variables.

## Key Findings

* **Income is the strongest predictor of total spending**: A significant positive linear association was noted between Income and 'Total_spends', confirmed by a high correlation value (~0.82). High-income consumers were consistently linked with greater total spending, reinforcing the importance of focusing on these groups for luxury products and tailored promotions.
* **Product Preferences**: Wines ranked highest in total spend among all categories, followed by meat goods and gold objects. Seafood, sweets, and fruits were linked to reduced general expenditure. This indicates that wines and meats generate the most sales for Nata and should be prioritized in marketing initiatives.
* **Education and Spending**: Customers with basic education demonstrated regularly low spending levels. Those with graduate, master's, or PhD degrees showed both larger and more erratic purchasing behaviors, usually ranging from CAD 500 to CAD 2,000.
* **Household Composition**: Households without children exhibited the largest spending ranges. Those with one or two children showed a significant median and maximum expenditure drop. Consumers without adolescents also tended to spend more.
* **Engagement Channels**: `Total_Spend` showed the most positive correlations with Catalog purchases, web purchases, and store purchases. The number of website visits, however, revealed a poor or even negative association with expenditure and web purchases, implying that visits by themselves do not convert to sales.
* **Campaign Effectiveness**: Campaign 1 was most favourably linked with wine and meat purchases—the top-spending categories—and also had the most total spend. Most campaigns performed better in leading to more Catalog Purchases compared to other channels. Campaigns 1 and 5 linked favorably with catalogue and in-store purchases but not with online visitors.
* **Customer Segments**: Three distinct clusters were identified:
    * **Cluster 1 (Value, High Engagement Consumers)**: This smaller cluster has the greatest overall spend per consumer, particularly in wines, meat, and gold items. These clients often have the greatest earnings, are older, more educated, and are more likely to accept campaigns (especially Campaigns 1, 2, and 5).
    * **Cluster 3 (The Core Middle Spenders)**: This is the biggest share of Nata's clientele. These people are middle-aged, generally with modest earnings and average overall expenditure across most product categories. Their engagement behavior consists of a sensible balance of store and catalogue purchases, with an average response rate for campaigns. Their household size usually consists of one or two children or teens, implying family responsibilities may affect expenditure balance.
* **Complaint Behavior**: Higher overall expenditure was linked to a decreased risk of complaints, though marginally significant, and earlier join dates also predicted fewer complaints. This implies that high-spend, longer-tenured consumers are less prone to show discontent.

## Recommendations

* **Dual-Segmentation Strategy**: Nata Supermarkets should prioritize a dual-segmentation strategy by targeting high-income, high-spend consumers (Cluster 1) with premium offerings (particularly wines, meats, and gold items). Furthermore, engage middle-income, value-conscious customers (Cluster 3) with steady catalogue promotions and consistent stock availability. While Cluster 2 is less profitable, targeted family bundles, coupons, and in-store deals can increase their participation cost-effectively.
* **Targeted Campaigns**: Scale Campaigns 1, 2, and 5 strategically based on segment responsiveness. Reposition Campaigns 3 and 6 towards younger, single, value-driven customers.
* **Optimized Marketing Spend**: Marketing spend should shift from broad outreach to precision targeting, using income, spend history, and channel preferences as segmentation filters. Catalogue and in-store channels should be emphasized for marketing efforts.
* **Cross-Selling and Bundling**: Leverage insights from category-level spending forecasters, such as cross-selling meat items to improve wine purchasing baskets, and bundling techniques (e.g., seafood plus sweet snacks or wine) to greatly raise sales of fish items.
* **Customer Loyalty**: Recognize that spend level and loyalty go hand in hand with satisfaction, or at least a decreased propensity for complaints, suggesting efforts to foster loyalty among high-spending customers will contribute to overall satisfaction.
