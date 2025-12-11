############################################################
# CIA Countries Analysis
# Author: Nora Zakrzewski
# Description:
#   - Loads CIA dataset
#   - Builds GDP regression models
#   - Performs clustering on socio-economic variables
#   - Visualizes country clusters
############################################################


cia <- read.csv("/Users/nora/Desktop/DSS R Programming/Datasets/CIACountries.csv")
head(cia)

model <- lm(gdp ~ pop + area + oil_prod + educ + roadways + net_users, data = cia)
summary(model)


model2 <- lm(gdp ~ oil_prod + roadways + net_users, data = cia)
summary(model2)

library(tidyverse)
library(cluster)
library(factoextra)

# Remove rows with missing values (optional but recommended for clustering)
cia_clean <- na.omit(cia)

# Separate numeric columns (for clustering math)
cia_numeric <- cia_clean[, sapply(cia_clean, is.numeric)]

# Assign country names as row labels (so they stay in results)
row.names(cia_numeric) <- cia_clean$country

# Scale numeric data
scaled <- scale(cia_numeric)

# Run clustering (example with 4 clusters) (commented out after analysis)
# set.seed(123)
#k3 <- kmeans(scaled, centers = 3, nstart = 25)
#k4 <- kmeans(scaled, centers = 4, nstart = 25)
#k5 <- kmeans(scaled, centers = 5, nstart = 25)

#visualize clusters (commented out)
#fviz_cluster(k3, data = scaled, ggtheme = theme_minimal())
#fviz_cluster(k4, data = scaled, ggtheme = theme_minimal())
#fviz_cluster(k5, data = scaled, ggtheme = theme_minimal())

# 3 clusters is best fit, proceed with this
set.seed(123)
k3 <- kmeans(scaled, centers = 3, nstart = 25)
fviz_cluster(
  k3, 
  data = scaled, 
  ggtheme = theme_minimal(),
  main = "Country Clusters Based on Socio-Economic Indicators"
)

#Add cluster membership back to the data
cia_results <- cia_clean
cia_results$cluster <- k3$cluster

head(cia_results[, c("country", "cluster")])
